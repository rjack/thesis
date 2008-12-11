#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
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


void
ted_init (void)
{
	nm_list = list_create (free, sizeof(struct sock_notify_msg));
	if (nm_list == LIST_ERR) {
		fprintf (stderr, "Errore inizializzazione TED farlocco\n");
		exit (EXIT_FAILURE);
	}
}


ssize_t
sendmsg_getID_fake (int sfd, const struct msghdr *msg, int flags,
                    int *id_result)
{
	static int id;

	assert (id_result != NULL);

	id++;
	if (id == -1)
		id++;
	*id_result = id;

	if ((rand() % 100) < FAIL_PERCENT)
		return SENDMSG_FAKE_ERR;
	return sendmsg (sfd, msg, flags);
}


void
ted_set_acked (dgram_t *dg, bool acked)
{
	struct sock_notify_msg *new_msg;

	assert (dg != NULL);

	new_msg = my_alloc(sizeof(struct sock_notify_msg));
	new_msg->nm_ack = acked;
	new_msg->nm_dgram_id = dg->dg_id;

	list_enqueue (nm_list, new_msg);
}


void
ted_run (list_t ifaces)
{
	iface_t *if_ptr;
	struct sock_notify_msg *nm;

	while (!list_is_empty (nm_list)) {
		nm = list_dequeue (nm_list);
		if_ptr = list_contains (ifaces, (f_compare_t)iface_cmp_id, &(nm->nm_iface_id), 0);
		if (if_ptr != NULL)
			iface_cmsg_write (if_ptr, nm, sizeof(*nm));
		free (nm);
	}
}
