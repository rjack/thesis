#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#include "dgram.h"
#include "iface.h"
#include "list.h"
#include "ted_fake.h"
#include "types.h"
#include "util.h"


static list_t nm_list;


static bool
nm_has_iface (struct sock_notify_msg *nm, iface_t *if_ptr)
{
	assert (nm != NULL);
	assert (if_ptr != NULL);

	if (nm->nm_iface == if_ptr)
		return TRUE;
	return FALSE;
}


void
ted_fake_init (void)
{
	nm_list = list_create (free, sizeof(struct sock_notify_msg));
	if (nm_list == LIST_ERR) {
		fprintf (stderr, "Errore inizializzazione TED farlocco\n");
		exit (EXIT_FAILURE);
	}
}


ssize_t
sendmsg_getID_fake (int sfd, const struct msghdr *hdr, int flags,
                    int *id_result)
{
	static int id;

	assert (id_result != NULL);

	id++;
	/* -1 sta a dire "nessun id" */
	if (id == -1)
		id++;
	*id_result = id;

	if ((rand() % 100) < FAIL_PERCENT) {
		if (verbose)
			printf ("sendmsg_getID_fake: simulato errore "
			        "trasmissione dgram %d\n", *id_result);
		return SENDMSG_FAKE_ERR;
	}
	return sendmsg (sfd, hdr, flags);
}


void
ted_fake_set_acked (dgram_t *dg, bool acked)
{
	struct sock_notify_msg *new_msg;

	assert (dg != NULL);

	new_msg = my_alloc(sizeof(struct sock_notify_msg));
	new_msg->nm_ack = acked;
	new_msg->nm_dgram_id = dg->dg_id;
	new_msg->nm_iface = dg->dg_if_ptr;

	list_enqueue (nm_list, new_msg);
}


struct sock_notify_msg *
ted_fake_get_notify (iface_t *if_ptr)
{
	list_t rmvd;
	struct sock_notify_msg *nm;

	assert (if_ptr != NULL);

	rmvd = list_remove_if (nm_list, (f_bool_t)nm_has_iface, if_ptr);
	if (list_is_empty (rmvd)) {
		list_destroy (rmvd);
		return NULL;
	}
	assert (list_length (rmvd) == 1);
	nm = list_dequeue (rmvd);
	list_destroy (rmvd);

	return nm;
}


void
ted_fake_clear_iface_ptr (iface_t *if_ptr)
{
	struct sock_notify_msg *nm;
	list_iterator_t lit;

	for (nm = list_iterator_get_first (nm_list, &lit);
	     nm != NULL;
	     nm = list_iterator_get_next (nm_list, &lit))
		if (nm->nm_iface == if_ptr)
			nm->nm_iface = NULL;
}


void
ted_fake_set_errqueue_events (iface_t *if_ptr)
{
	struct pollfd *pfd;
	if (list_contains (nm_list, (f_bool_t)nm_has_iface, if_ptr, 0)) {
		pfd = iface_get_pollfd (if_ptr);
		pfd->revents |= POLLERR;
	}
}


bool
ted_fake_events_pending (void)
{
	if (!list_is_empty (nm_list))
		return TRUE;
	return FALSE;
}
