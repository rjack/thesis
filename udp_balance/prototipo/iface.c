#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linux/types.h>
#include <linux/errqueue.h>
#include <unistd.h>

#include "crono.h"
#include "dgram.h"
#include "iface.h"
#include "list.h"
#include "types.h"
#include "util.h"


struct match_iface_args {
	const char *mia_name;
	const char *mia_loc_ip;
};



static bool
match_iface (iface_t *if_ptr, iface_id_t *id)
{
	if (strcmp (if_ptr->if_id.ii_name, id->ii_name) == 0
	    && strcmp (if_ptr->if_id.ii_loc_ip, id->ii_loc_ip) == 0)
		return TRUE;
	return FALSE;
}


bool
iface_must_send_keepalive (const iface_t *if_ptr)
{
	return if_ptr->if_must_send_keepalive;
}


#ifdef NON_COMPILARE
int
iface_up (const char *name, const char *loc_ip)
{
	struct timeval now;
	iface_t *if_ptr;

	gettime (&now);

	assert (ifaces_len <= IFACE_MAX);
	if (ifaces_len == IFACE_MAX) {
		fprintf (stderr,
			 "Impossibile aggiungere un'interfaccia di rete, "
			 "numero massimo = %d\n", IFACE_MAX);
		exit (EXIT_FAILURE);
	}

	if_ptr = my_alloc (sizeof(iface_t));

	if_ptr->if_suspected = FALSE;
	if_ptr->if_must_send_keepalive = FALSE;

	strcpy (if_ptr->if_id.ii_name, name);
	strcpy (if_ptr->if_id.ii_loc_ip, loc_ip);
	strcpy (if_ptr->if_id.ii_loc_port, PX_LOC_PORT);
	/* XXX PX_LOC_PORT potrebbe essere randomizzata finche' non se ne
	 * trova una libera. */
	if_ptr->if_pfd.fd = socket_bound_conn (loc_ip, PX_LOC_PORT,
					       PX_REM_IP, PX_REM_PORT);
	if_ptr->if_pfd.events = 0;
	if_ptr->if_pfd.revents = 0;
	timeout_set (&if_ptr->if_keepalive, &time_150ms);
	timeout_start (&if_ptr->if_keepalive, &now);

	list_enqueue (&ifaces, new_node (if_ptr));
	ifaces_len++;

	/* TODO controllo errore socket_bound_conn */
	return 1;
}
#endif /* NON_COMPILARE */


#ifdef NON_COMPILARE
void
iface_down (iface_id_t *if_id)
{
	list_node_t *rmvd;

	rmvd = list_remove_if (ifaces, (bool (*)(void *, void*))&match_iface,
	                       if_id);
	if (!list_is_empty (rmvd)) {
		assert (list_node_is_last (rmvd, list_head (rmvd)));
		list_destroy (&rmvd, &iface_destroy);
		ifaces_len--;
	}
}
#endif /* NON_COMPILARE */


void
iface_destroy (iface_t *if_ptr)     /* TODO */
{
	assert (FALSE);
}


int
iface_get_events (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	return if_ptr->if_pfd.revents;
}


void
iface_keepalive_left (iface_t *if_ptr, struct timeval *result)
{
	struct timeval now;

	gettime (&now);

	timeout_left (&if_ptr->if_keepalive, &now, result);
	if (tv_cmp (result, &time_0ms) <= 0)
		if_ptr->if_must_send_keepalive = TRUE;
	else
		if_ptr->if_must_send_keepalive = FALSE;
}


void
iface_set_events (iface_t *if_ptr, int e)
{
	assert (if_ptr != NULL);

	if_ptr->if_pfd.events |= e;
}


void
iface_reset_events (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	if_ptr->if_pfd.events = 0;
	if_ptr->if_pfd.revents = 0;
}


void
iface_print (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	printf ("%s %s:%s [%c%c] ",
		if_ptr->if_id.ii_name,
		if_ptr->if_id.ii_loc_ip,
		if_ptr->if_id.ii_loc_port,
		if_ptr->if_suspected ? 's' : '-',
		if_ptr->if_must_send_keepalive ? 'k' : '-');
	printf ("keepalive");
	timeout_print (&if_ptr->if_keepalive);
}


struct pollfd *
iface_get_pollfd (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	return &(if_ptr->if_pfd);
}


void
iface_set_pollfd (iface_t *if_ptr, struct pollfd *pfd)
{
	assert (if_ptr != NULL);

	memcpy (&(if_ptr->if_pfd), pfd, sizeof(*pfd));
}


ssize_t
iface_write (iface_t *if_ptr, dgram_t *dg)
{
	ssize_t nsent;
	struct timeval now;

	assert (if_ptr != NULL);
	assert (dg != NULL);

	nsent = dgram_write_getID (if_ptr->if_pfd.fd, dg, NULL, 0);
	if (nsent == -1)
		return -1;

	assert (nsent == dg->dg_datalen);

	/* reset timeout keepalive */
	gettime (&now);
	timeout_start (&if_ptr->if_keepalive, &now);
	if_ptr->if_must_send_keepalive = FALSE;

	return nsent;
}


dgram_t *
iface_read (iface_t *if_ptr)
{
	dgram_t *dg;

	assert (if_ptr != NULL);

	dg = dgram_read (if_ptr->if_pfd.fd, NULL, NULL);

	return dg;
}


int
iface_handle_err (iface_t *if_ptr)
/* FIXME fa troppe cose! separare decisioni da meccanismo. */
{
	/* Puo' essere:
	 * 1. IP_NOTIFY errore trasmissione
	 *    -> leggere id
	 *    -> recuperare dai dgram unaked il dgram
	 *       con quell'id
	 *    -> rimetterlo tra i dgram in uscita
	 *
	 * 2. IP_NOTIFY riuscita trasmissione
	 *    -> leggere id
	 *    -> recuperare dai dgram unaked il dgram
	 *       con quell'id
	 *    -> buttarlo
	 *
	 * 3. ICMP errore
	 *    -> fprintf
	 *    -> iface_down */

	ssize_t nrecv;
	char cbuf[CONTROLBUFLEN];
	struct msghdr msg;
	struct cmsghdr *cmsg;

	assert (if_ptr != NULL);

	msg.msg_name = NULL;
	msg.msg_namelen = 0;
	msg.msg_iov = NULL;
	msg.msg_iovlen = 0;
	msg.msg_control = cbuf;
	msg.msg_controllen = CONTROLBUFLEN;
	msg.msg_flags = 0;

	nrecv = recvmsg (if_ptr->if_pfd.fd, &msg, MSG_ERRQUEUE);
	if (nrecv == -1) {
		perror ("Errore nella gestione dell'errore");
		exit (EXIT_FAILURE);
	}

	for (cmsg = CMSG_FIRSTHDR (&msg);
	     cmsg != NULL;
	     cmsg = CMSG_NXTHDR (&msg, cmsg)) {
		/* Errore a livello IP, tira giu' l'interfaccia. */
		if (cmsg->cmsg_level == SOL_IP) {
			if (cmsg->cmsg_type == IP_RECVERR) {
				struct sock_extended_err *ee;
				ee = (struct sock_extended_err *) CMSG_DATA (cmsg);
				fprintf (stderr, "ERRORE ");
				if (ee->ee_origin == SO_EE_ORIGIN_LOCAL)
					fprintf (stderr, "LOCALE");
				else if (ee->ee_origin == SO_EE_ORIGIN_ICMP)
					fprintf (stderr, "ICMP");
				else
					fprintf (stderr, "SCONOSCIUTO (%d)",
						 ee->ee_origin);
				fprintf (stderr, " sull'interfaccia ");
				iface_print (if_ptr);
				fprintf (stderr, "\n");

				/* FIXME iface_down (&if_ptr->if_id); */
			}

			else if (cmsg->cmsg_type == IP_NOTIFY) {
				struct sock_notify_msg *nm;
				nm = (struct sock_notify_msg *) CMSG_DATA (cmsg);
				if (nm->nm_ack == TRUE)
					dgram_discard (nm->nm_dgram_id);
				else
					dgram_outward (nm->nm_dgram_id);
			}
		}
	}

	return 0;
}
