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


bool
iface_has_name (const iface_t *if_ptr, const char *name)
{
	assert (if_ptr != NULL);
	assert (name != NULL);

	if (strcmp (if_ptr->if_name, name) == 0)
		return TRUE;
	return FALSE;
}


bool
iface_must_send_keepalive (const iface_t *if_ptr)
{
	return if_ptr->if_must_send_keepalive;
}


iface_t *
iface_create (const char *name, const char *loc_ip)
{
	static int id = 0;
	struct timeval now;
	iface_t *new_if;

	gettime (&now);

	new_if = my_alloc (sizeof(iface_t));

	new_if->if_id = id++;
	new_if->if_suspected = FALSE;
	new_if->if_must_send_keepalive = FALSE;
	iface_set_name (new_if, name, loc_ip, PX_LOC_PORT);

	/* XXX PX_LOC_PORT potrebbe essere randomizzata finche' non se ne
	 * trova una libera. */
	new_if->if_pfd.fd = socket_bound_conn (loc_ip, PX_LOC_PORT,
					       PX_REM_IP, PX_REM_PORT);
	new_if->if_pfd.events = 0;
	new_if->if_pfd.revents = 0;
	timeout_set (&(new_if->if_keepalive), &time_150ms);
	timeout_start (&(new_if->if_keepalive), &now);

	/* Ha un firmware che notifica i dgram ricevuti o i dgram non
	 * ricevuti? Affidiamoci alla sorte.
	new_if->if_firmware_positive = ((rand () % 100) > 50) ?
	                               TRUE : FALSE;
	 */
	new_if->if_firmware_positive = FALSE;

	/* TODO controllo errore socket_bound_conn */

	return new_if;
}


void
iface_set_name (iface_t *if_ptr, const char *name, const char *ip,
                const char *port)
{
	my_strncpy (if_ptr->if_name, name, IFACE_ID_NAME_LEN);
	my_strncpy (if_ptr->if_loc_ip, ip, IFACE_ID_LOC_IP_LEN);
	my_strncpy (if_ptr->if_loc_port, port, IFACE_ID_LOC_PORT_LEN);
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

	printf ("%d %s %s %s:%s [%c%c] ",
		if_ptr->if_id,
		if_ptr->if_name,
		if_ptr->if_firmware_positive ? "fw_pos" : "fw_neg",
		if_ptr->if_loc_ip,
		if_ptr->if_loc_port,
		if_ptr->if_suspected ? 's' : '-',
		if_ptr->if_must_send_keepalive ? 'k' : '-');
	/*
	printf ("keepalive");
	timeout_print (&if_ptr->if_keepalive);
	*/
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
	dg->dg_if_ptr = if_ptr;

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
	if (!if_ptr->if_firmware_positive && nsent == SENDMSG_FAKE_ERR)
		ted_fake_set_acked (dg, FALSE);
	if (if_ptr->if_firmware_positive && nsent != SENDMSG_FAKE_ERR)
		ted_fake_set_acked (dg, TRUE);

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


int
iface_handle_err (iface_t *if_ptr)
{
	/*
	 * XXX Premessa: questa funzione e' *BRUTTA*.
	 *
	 * Nell'ULB finale, errori ICMP e notifiche TED vengono tutti pescati
	 * dalla msg_errqueue.
	 *
	 * Nel prototipo, le notifiche TED vanno pescate per vie traverse,
	 * perche' non si puo' imbrogliare la errqueue di un socket udp
	 * mandandole cmsg farlocchi.
	 *
	 * Quindi, prima proviamo  a leggere l'errqueue per gli errori ICMP,
	 * in modo NON BLOCCANTE (puo' essere che non ce ne siano e che siamo
	 * qui per colpa del TED).
	 *
	 * Se la errqueue e' vuota, chiamiamo il TED simulato.
	 *
	 * L'esito e' indicato da errno:
	 *    E_IFACE_FATAL   errore ICMP
	 *    E_IFACE_DG_ACK  il datagram e' arrivato all'AP
	 *    E_IFACE_DG_NAK  il datagram non e' arrivato all'AP
	 *
	 * ACK o NAK dipende dal firmware della scheda, quindi il
	 * comportamento puo' essere potenzialmente diverso da interfaccia a
	 * interfaccia.
	 *
	 * FIXME ora TED si comporta allo stesso modo per tutte le
	 * FIXME interfacce!
	 */

	ssize_t nrecv;
	char cbuf[CONTROLBUFLEN];
	struct msghdr msg;
	struct cmsghdr *cmsg;
	int dg_id;
	int my_errno;

	assert (if_ptr != NULL);

	msg.msg_name = NULL;
	msg.msg_namelen = 0;
	msg.msg_iov = NULL;
	msg.msg_iovlen = 0;
	msg.msg_control = cbuf;
	msg.msg_controllen = CONTROLBUFLEN;
	msg.msg_flags = 0;

	nrecv = recvmsg (if_ptr->if_pfd.fd, &msg,
	                 MSG_ERRQUEUE | MSG_DONTWAIT);
	if (nrecv == -1) {
		struct sock_notify_msg *nm;

		if (errno != EAGAIN) {
			perror ("Errore nella gestione dell'errore");
			exit (EXIT_FAILURE);
		}

		nm = ted_fake_get_notify (if_ptr);
		if (nm->nm_ack == TRUE)
			my_errno = E_IFACE_DG_ACK;
		else
			my_errno = E_IFACE_DG_NAK;
		dg_id = nm->nm_dgram_id;
		free (nm);
		goto happy_ending;
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

				my_errno = E_IFACE_FATAL;
				dg_id = -1;
			}

			/* XXX OK, non si puo' imbrogliare la errqueue di un
			 * XXX socket udp. Era anche abbastanza ovvio, ma
			 * XXX provare non costava nulla, a parte tempo e
			 * XXX sudore.
			else if (cmsg->cmsg_type == IP_NOTIFY) {
				struct sock_notify_msg *nm;

				nm = (struct sock_notify_msg *)CMSG_DATA (cmsg);
				if (nm->nm_ack == TRUE)
					my_errno = E_IFACE_DG_ACK;
				else
					my_errno = E_IFACE_DG_NAK;
				dg_id = nm->nm_dgram_id;
			}
			*/

			else {
				fprintf (stderr,
				         "iface_handle_err: che accidenti e' "
				         "arrivato?\n");
				exit (EXIT_FAILURE);
			}
		}
	}

happy_ending:
	errno = my_errno;
	return dg_id;
}
