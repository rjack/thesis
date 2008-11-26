#include <assert.h>
#include <errno.h>
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

#define     BUFFER_LEN     65535
static char buffer[BUFFER_LEN];
static struct iovec iov[1];


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


static list_node_t **
get_list (int list_id)
{
	assert (list_id == DGRAM_OUTWARD || list_id == DGRAM_INWARD);

	if (list_id == DGRAM_INWARD)
		return &data_in;
	return &data_out;
}


static bool
dgram_must_be_discarded (dgram_t *dg)
/* Ritorna TRUE se dg e'piu' vecchio di 150ms,
 *         FALSE altrimenti.
 * NON dealloca i timeout. */
{
	struct timeval left;

	timeout_left (dg->dg_life_to, &now, &left);

	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


static bool
must_be_retransmitted (dgram_t *dg)
/* Ritorna TRUE e dealloca dg_retry_to se dg e'piu' vecchio di 30ms,
 *         FALSE altrimenti. */
{
	struct timeval left;

	timeout_left (dg->dg_retry_to, &now, &left);

	if (tv_cmp (&left, &time_0ms) <= 0) {
		free (dg->dg_retry_to);
		dg->dg_retry_to = NULL;
		return TRUE;
	}
	return FALSE;
}


void
dgram_init_module (void)
{
	data_out = list_create ();
	data_in = list_create ();
	data_unaked = list_create ();

	iov[0].iov_base = buffer;
	iov[0].iov_len = BUFFER_LEN;
}


void
dgram_outward_all_unacked (struct timeval *now)
{
	/* TODO dgram_outward_all_unacked */
}


void
dgram_purge_all_old (struct timeval *now)
{
}


void
dgram_timeout_min (struct timeval *result)
{
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
dgram_read (fd_t sfd)
{
	ssize_t nrecv;
	struct msghdr hdr;
	dgram_t *dg;

	/* XXX salvare mittente! */
	hdr.msg_name = NULL;
	hdr.msg_namelen = 0;
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
	dg->dg_data = my_alloc (nrecv);
	memcpy (dg->dg_data, buffer, nrecv);
	dg->dg_datalen = nrecv;
	dg->dg_life_to = NULL;
	dg->dg_retry_to = NULL;
	dg->dg_id = -1;

	return dg;
}


int
dgram_write (fd_t sfd, dgram_t *dg)
{
	ssize_t nsent;
	struct msghdr hdr;
	struct iovec my_iov[1];

	my_iov[0].iov_base = dg->dg_data;
	my_iov[0].iov_len = dg->dg_datalen;

	hdr.msg_name = NULL;
	hdr.msg_namelen = 0;
	hdr.msg_iov = my_iov;
	hdr.msg_iovlen = ARRAYLEN(my_iov);
	hdr.msg_control = NULL;
	hdr.msg_controllen = 0;
	hdr.msg_flags = 0;

	do {
		nsent = sendmsg (sfd, &hdr, 0);
	} while (nsent == -1 && errno == EINTR);

	assert (nsent == dg->dg_datalen);

	return nsent;
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
