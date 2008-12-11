#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <linux/types.h>
#include <linux/errqueue.h>
#include <unistd.h>

#include "errno.h"
#include "crono.h"
#include "dgram.h"
#include "iface.h"
#include "list.h"
#include "ted_fake.h"
#include "types.h"
#include "util.h"


int
iface_cmp_id (iface_t *if_ptr, iface_id_t *id)
{
	if (strcmp (if_ptr->if_id.ii_name, id->ii_name) == 0
	    && strcmp (if_ptr->if_id.ii_loc_ip, id->ii_loc_ip) == 0)
		return 0;
	return 1;
}


bool
iface_must_send_keepalive (const iface_t *if_ptr)
{
	return if_ptr->if_must_send_keepalive;
}


iface_t *
iface_create (const char *name, const char *loc_ip)
{
	struct timeval now;
	iface_t *new_if;

	gettime (&now);

	new_if = my_alloc (sizeof(iface_t));

	new_if->if_suspected = FALSE;
	new_if->if_must_send_keepalive = FALSE;
	iface_id_set (&(new_if->if_id), name, loc_ip, PX_LOC_PORT);

	/* XXX PX_LOC_PORT potrebbe essere randomizzata finche' non se ne
	 * trova una libera. */
	new_if->if_pfd.fd = socket_bound_conn (loc_ip, PX_LOC_PORT,
					       PX_REM_IP, PX_REM_PORT);
	new_if->if_pfd.events = 0;
	new_if->if_pfd.revents = 0;
	timeout_set (&(new_if->if_keepalive), &time_150ms);
	timeout_start (&(new_if->if_keepalive), &now);

	/* TODO controllo errore socket_bound_conn */

	return new_if;
}


void
iface_id_set (iface_id_t *if_id, const char *name, const char *ip,
              const char *port)
{
	my_strncpy (if_id->ii_name, name, IFACE_ID_NAME_LEN);
	my_strncpy (if_id->ii_loc_ip, ip, IFACE_ID_LOC_IP_LEN);
	my_strncpy (if_id->ii_loc_port, port, IFACE_ID_LOC_PORT_LEN);
}


void
iface_destroy (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	free (if_ptr);
}


int
iface_get_events (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	return if_ptr->if_pfd.revents;
}


void
iface_set_suspected (iface_t *if_ptr)
{
	assert (if_ptr != NULL);
	if_ptr->if_suspected = TRUE;
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

	if_ptr->if_pfd.events = POLLIN | POLLERR;
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

	/* Marchia il dg con id interfaccia. */
	dg->dg_iface_id = my_alloc (sizeof(iface_id_t));
	memcpy (dg->dg_iface_id, &(if_ptr->if_id), sizeof(iface_id_t));

	nsent = dgram_write_getID (if_ptr->if_pfd.fd, dg, NULL, 0);
	if (nsent == -1)
		return -1;

#ifndef NDEBUG
	if (nsent >= 0)
		assert (nsent == dg->dg_datalen);
#endif /* NDEBUG */

	/* Reset timeout keepalive. */
	gettime (&now);
	timeout_start (&if_ptr->if_keepalive, &now);
	if_ptr->if_must_send_keepalive = FALSE;

	/*
	 * Simulazione TED.
	 * Positivo: segnala ricezione da parte dell'AP.
	 * Negativo: segnala fallimento trasmissione all'AP.
	 */
	if (!TED_FAKE_POSITIVE && nsent == SENDMSG_FAKE_ERR)
		ted_set_acked (dg, FALSE);
	if (TED_FAKE_POSITIVE && nsent != SENDMSG_FAKE_ERR)
		ted_set_acked (dg, TRUE);

	/* La simulazione dell'errore di ricezione dell'AP deve essere
	 * silenziosa, come nella realta'. */
	if (nsent == SENDMSG_FAKE_ERR)
		nsent = dg->dg_datalen;

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


void
iface_id_destroy (iface_id_t *if_id)
{
	assert (if_id != NULL);
	free (if_id);
}


int
iface_handle_err (iface_t *if_ptr)
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
	int dg_id;

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
		if (cmsg->cmsg_level == SOL_IP) {
			if (cmsg->cmsg_type == IP_RECVERR) {
				struct sock_extended_err *ee;

				ee = (struct sock_extended_err *)CMSG_DATA (cmsg);
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

				errno = E_IFACE_FATAL;
				dg_id = -1;
			}

			else if (cmsg->cmsg_type == IP_NOTIFY) {
				struct sock_notify_msg *nm;

				nm = (struct sock_notify_msg *)CMSG_DATA (cmsg);
				if (nm->nm_ack == TRUE)
					errno = E_IFACE_DG_ACK;
				else
					errno = E_IFACE_DG_NAK;
				dg_id = nm->nm_dgram_id;
			} else {
				fprintf (stderr,
				         "Che accidenti e' arrivato?\n");
				exit (EXIT_FAILURE);
			}
		}
	}

	return dg_id;
}
