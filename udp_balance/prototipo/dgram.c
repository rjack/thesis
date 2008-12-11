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
#include "iface.h"
#include "list.h"
#include "types.h"
#include "util.h"


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


int
dgram_cmp_id (dgram_t *dg, int *id)
{
	assert (dg != NULL);
	assert (id != NULL);
	assert (dg->dg_id != -1);
	assert (*id != -1);

	if (dg->dg_id == *id)
		return 0;
	if (dg->dg_id < *id)
		return -1;
	return 1;
}


bool
dgram_must_be_discarded (dgram_t *dg)
{
	struct timeval now;
	struct timeval left;

	gettime (&now);

	timeout_left (dg->dg_life_to, &now, &left);
	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


bool
dgram_must_be_retransmitted (dgram_t *dg)
{
	struct timeval now;
	struct timeval left;

	gettime (&now);

	timeout_left (dg->dg_retry_to, &now, &left);
	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


void
dgram_destroy_reply_timeout (dgram_t *dg)
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
	new_dg->dg_iface_id = NULL;

	return new_dg;
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


void
dgram_set_life_timeout (dgram_t *dg)
{
	struct timeval now;

	assert (dg != NULL);
	assert (dg->dg_life_to == NULL);

	dg->dg_life_to = new_timeout (&time_150ms);
	gettime (&now);
	timeout_start (dg->dg_life_to, &now);
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
	if (dg->dg_iface_id != NULL)
		iface_id_destroy (dg->dg_iface_id);
	free (dg);
}


dgram_t *
dgram_create_keepalive (void)
{
	return dgram_create ();
}
