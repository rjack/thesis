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
#include "list.h"
#include "util.h"
#include "types.h"

/*
 * Code per i datagram.
 */
/* da softphone a server */
static list_node_t *data_out;

/* da server a softphone */
static list_node_t *data_in;

/* spediti, da confermare */
static list_node_t *data_unaked;

#define     BUFFER_LEN     1500


static list_node_t **
get_list (int list_id)
{
	assert (list_id == DGRAM_OUTWARD
	        || list_id == DGRAM_INWARD
		|| list_id == DGRAM_UNACKED);

	switch (list_id) {
	case DGRAM_OUTWARD:
		return &data_out;
	case DGRAM_INWARD:
		return &data_in;
	case DGRAM_UNACKED:
		return &data_unaked;
	default:
		return NULL;
	}
}


static bool
must_be_discarded (void *dg_void, void *now_void)
{
	struct timeval left;
	dgram_t *dg = (dgram_t *)dg_void;
	struct timeval *now = (struct timeval *)now_void;

	timeout_left (dg->dg_life_to, now, &left);
	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


static bool
must_be_retransmitted (void *dg_void, void *now_void)
{
	struct timeval left;
	dgram_t *dg = (dgram_t *)dg_void;
	struct timeval *now = (struct timeval *)now_void;

	timeout_left (dg->dg_retry_to, now, &left);
	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


static void
free_reply_to (void *ptr, void *discard)
{
	dgram_t *dg = (dgram_t *)ptr;

	assert (dg->dg_retry_to != NULL);

	free (dg->dg_retry_to);
	dg->dg_retry_to = NULL;
}


void
dgram_init_module (void)
{
	data_out = list_create ();
	data_in = list_create ();
	data_unaked = list_create ();
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
dgram_outward_all_unacked (void)
{
	struct timeval now;
	list_node_t *rmvd;

	gettime (&now);

	rmvd = list_remove_if (&data_unaked, must_be_retransmitted, &now);

	list_foreach_do (rmvd, free_reply_to, NULL);

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
dgram_timeout_min (struct timeval *min_result)
{
	struct timeval now;
	struct timeval left;
	list_node_t *head;
	list_node_t *node;
	dgram_t *dg;

	gettime (&now);

	/* data out: hanno solo dg_life_to */
	head = list_head (data_out);
	node = head;
	if (head != NULL)
		do {
			dg = (dgram_t *)node->n_ptr;
			assert (dg->dg_retry_to == NULL);
			timeout_left (dg->dg_life_to, &now, &left);
			tv_min (min_result, min_result, &left);
			node = list_next (node);
		} while (node != head);

	/* data unacked: hanno sia dg_life_to che dg_retry_to */
	head = list_head (data_unaked);
	node = head;
	if (head != NULL)
		do {
			dg = (dgram_t *)node->n_ptr;
			timeout_left (dg->dg_life_to, &now, &left);
			tv_min (min_result, min_result, &left);
			timeout_left (dg->dg_retry_to, &now, &left);
			tv_min (min_result, min_result, &left);
			node = list_next (node);
		} while (node != head);
}


dgram_t *
dgram_list_peek (int list_id)
{
	list_node_t **list;
	list_node_t *node;

	list = get_list (list_id);
	node = list_head (*list);

	if (node == NULL)
		return NULL;
	return node->n_ptr;
}


void
dgram_list_add (int list_id, dgram_t *dg)
{
	list_node_t **list;

	assert (dg != NULL);

	list = get_list (list_id);
	list_enqueue (list, new_node (dg));
}


dgram_t *
dgram_list_pop (int list_id)
{
	list_node_t **list;
	list_node_t *node;
	dgram_t *dg;

	list = get_list (list_id);
	node = list_dequeue (list);
	if (node != NULL) {
		dg = node->n_ptr;
		free (node);
		return dg;
	}
	return NULL;
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
dgram_write (fd_t sfd, dgram_t *dg,
             struct sockaddr_in *rem_addr, socklen_t rem_addr_len)
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

	do {
		nsent = sendmsg_getID_fake (sfd, &hdr, 0, &dg->dg_id);
	} while (nsent == -1 && errno == EINTR);
	if (nsent == -1)
		return -1;

	assert (nsent == dg->dg_datalen);
	return nsent;
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
dgram_free (dgram_t *dg)
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
