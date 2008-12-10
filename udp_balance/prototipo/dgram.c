#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "crono.h"
#include "dgram.h"
#include "list.h"
#include "util.h"
#include "types.h"


#define     BUFFER_LEN     1500


static ssize_t
do_dgram_write (fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr,
                socklen_t rem_addr_len, int *id_result)
{
	ssize_t nsent;
	struct msghdr hdr;
	struct iovec my_iov[1];

	my_iov[0].iov_base = dg->dg_data;
	my_iov[0].iov_len = dg->dg_datalen;

	hdr.msg_name = rem_addr;
	hdr.msg_namelen = rem_addr_len;
	hdr.msg_iov = my_iov;
	hdr.msg_iovlen = ARRAYLEN(my_iov);
	hdr.msg_control = NULL;
	hdr.msg_controllen = 0;
	hdr.msg_flags = 0;

	if (id_result == NULL)
		do {
			nsent = sendmsg (sfd, &hdr, 0);
		} while (nsent == -1 && errno == EINTR);
	else
		do {
			nsent = sendmsg_getID_fake (sfd, &hdr, 0, id_result);
		} while (nsent == -1 && errno == EINTR);

	if (nsent == -1)
		return -1;

	assert (nsent == dg->dg_datalen);
	return nsent;
}


static bool
id_match (void *dg_void, void *id_void)
{
	dgram_t *dg = (dgram_t *)dg_void;
	int *id = (int *)id_void;

	if (dg->dg_id == *id)
		return TRUE;
	return FALSE;
}


bool
dgram_must_be_discarded (dgram_t *dg, struct timeval *now)
{
	struct timeval left;

	timeout_left (dg->dg_life_to, now, &left);
	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


bool
dgram_must_be_retransmitted (dgram_t *dg, struct timeval *now)
{
	struct timeval left;

	timeout_left (dg->dg_retry_to, now, &left);
	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


static void
free_reply_tmout (dgram_t *dg, void *discard)
{
	assert (dg->dg_retry_to != NULL);

	free (dg->dg_retry_to);
	dg->dg_retry_to = NULL;
}


dgram_t *
dgram_create (void)
{
	dgram_t *new_dg;

	new_dg = my_alloc (sizeof(dgram_t));

	new_dg->dg_id = -1;
	new_dg->dg_data = NULL;
	new_dg->dg_datalen = 0;
	new_dg->dg_life_to = NULL;
	new_dg->dg_retry_to = NULL;

	return new_dg;
}


void
dgram_discard (int id)
{
	list_node_t *rmvd;

	rmvd = list_remove_if (&data_unaked, id_match, &id);
	if (list_is_empty (rmvd))
		rmvd = list_remove_if (&data_out, id_match, &id);

	if (!list_is_empty (rmvd)) {
		assert (list_node_is_last (rmvd, list_head (rmvd)));
		list_destroy (&rmvd, free);
	}
}


void
dgram_outward (int id)
{
	list_node_t *rmvd;

	rmvd = list_remove_if (&data_unaked, id_match, &id);
	if (!list_is_empty (rmvd)) {
		assert (list_node_is_last (rmvd, list_head (rmvd)));
		list_push (&data_out, list_head (rmvd));
	}
}


void
dgram_outward_all_unacked (void)
{
	struct timeval now;
	list_node_t *rmvd;

	gettime (&now);

	rmvd = list_remove_if (&data_unaked, must_be_retransmitted, &now);

	list_foreach_do (rmvd, free_reply_tmout, NULL);

	data_out = list_cat (rmvd, data_out);
}


void
dgram_purge_all_old (void)
{
	struct timeval now;
	list_node_t *rmvd;

	gettime (&now);

	/* data_out */
	rmvd = list_remove_if (&data_out, must_be_discarded, &now);
	list_destroy (&rmvd, free);

	/* data_unaked */
	rmvd = list_remove_if (&data_unaked, must_be_discarded, &now);
	list_destroy (&rmvd, free);
}


void
dgram_min_timeout (dgram_t *dg, struct timeval *min_result)
{
	struct timeval now;
	struct timeval left;

	assert (dg->dg_life_to != NULL);

	/* dg ha dg_retry_to solo se e' nella coda unacked. */
	if (dg->dg_retry_to != NULL) {
		timeout_left (dg->dg_retry_to, &now, &left);
		tv_min (min_result, min_result, &left);
	}

	timeout_left (dg->dg_life_to, &now, &left);
	tv_min (min_result, min_result, &left);
}


dgram_t *
dgram_read (fd_t sfd, struct sockaddr_in *src_addr_result,
            socklen_t *src_addr_result_len)
{
	ssize_t nrecv;
	struct msghdr hdr;
	dgram_t *dg;
	char buffer[BUFFER_LEN];
	struct iovec iov[1];

	iov[0].iov_base = buffer;
	iov[0].iov_len = BUFFER_LEN;

	hdr.msg_name = src_addr_result;
	hdr.msg_namelen = src_addr_result_len == NULL ?
	                  0 : *src_addr_result_len;
	hdr.msg_iov = iov;
	hdr.msg_iovlen = ARRAYLEN(iov);
	hdr.msg_control = NULL;
	hdr.msg_controllen = 0;
	hdr.msg_flags = 0;

	do {
		nrecv = recvmsg (sfd, &hdr, 0);
	} while (nrecv == -1 && errno == EINTR);

	if (nrecv == -1) {
		perror ("recvmsg (dgram_read)");
		exit (EXIT_FAILURE);
	}

	dg = dgram_create ();
	if (nrecv > 0) {
		dg->dg_data = my_alloc (nrecv);
		memcpy (dg->dg_data, buffer, nrecv);
	} else
		dg->dg_data = NULL;
	dg->dg_datalen = nrecv;
	dg->dg_life_to = NULL;
	dg->dg_retry_to = NULL;
	dg->dg_id = -1;

	if (src_addr_result_len != NULL)
		*src_addr_result_len = hdr.msg_namelen;

	return dg;
}


ssize_t
dgram_write_getID (fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr,
                   socklen_t rem_addr_len)
{
	return do_dgram_write (sfd, dg, rem_addr, rem_addr_len, &dg->dg_id);
}


ssize_t
dgram_write (fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr,
             socklen_t rem_addr_len)
{
	return do_dgram_write (sfd, dg, rem_addr, rem_addr_len, NULL);
}


void
dgram_print (const dgram_t *dg)
{
	printf ("[");
	printf ("id:%d data:%.*s datalen:%d ",
		dg->dg_id, dg->dg_datalen, dg->dg_data, dg->dg_datalen);
	printf ("life_to: ");

	if (dg->dg_life_to == NULL)
		printf ("-");
	else
		timeout_print (dg->dg_life_to);

	printf ("retry_to: ");
	if (dg->dg_retry_to == NULL)
		printf ("-");
	else
		timeout_print (dg->dg_retry_to);
}


void
dgram_destroy (dgram_t *dg)
{
	assert (dg != NULL);

	if (dg->dg_data != NULL)
		free (dg->dg_data);
	if (dg->dg_life_to != NULL)
		free (dg->dg_life_to);
	if (dg->dg_retry_to != NULL)
		free (dg->dg_retry_to);
	free (dg);
}


dgram_t *
dgram_create_keepalive (void)
{
	return dgram_create ();
}
