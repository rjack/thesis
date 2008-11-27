/*
 * udp_balancer: prototipo.
 */

#define     ULP_PROTO_MAIN


#include <assert.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "crono.h"
#include "dgram.h"
#include "iface.h"
#include "types.h"
#include "util.h"


/****************************************************************************
				   Costanti
****************************************************************************/

/* XXX C'e' modo di scoprire a runtime la dimensione di allocazione per il
 * msg_control delle struct msghdr e poterlo cosi' allocare dinamicamente
 * senza rischiare un MSG_TRUNC quando si fa una recvmsg con flag
 * MSG_ERRQUEUE?
 * Nell'attesa di scoprirlo uso la costante trovata nel sorgente di traceroute
 * <http://traceroute.sf.net/> */
#define     CONTROLBUFLEN     1024

#define     SP_BIND_IP       "127.0.0.1"
#define     SP_BIND_PORT     "7777"

/* FIXME in realta' l'IP del softphone e' quello da cui ricevo i datagram. */
#define     SP_CONN_IP       "127.0.0.1"
#define     SP_CONN_PORT     "7778"

#define     IM_BIND_IP       "127.0.0.1"
#define     IM_BIND_PORT     "8888"


/****************************************************************************
			       Variabili locali
****************************************************************************/

static const char *program_name = NULL;


/****************************************************************************
				   Funzioni
****************************************************************************/

static void
print_usage (void)
{
	assert (program_name != NULL);
	fprintf (stderr, "usage: %s\n", program_name);
}


static bool
is_done (void)
{
	return FALSE;
}


/****************************************************************************
				     Main
****************************************************************************/

int
main (const int argc, const char *argv[])
{
	struct pollfd fds[2 + IFACE_MAX];
	size_t ifaces_used = 0;

	struct pollfd *sp = &fds[0];
	struct pollfd *im = &fds[1];

	/*
	 * Init variabili globali
	 */
	program_name = argv[0];

	time_0ms.tv_sec = 0;
	time_0ms.tv_usec = 0;

	time_30ms.tv_sec = 0;
	time_30ms.tv_usec = 30000;

	time_150ms.tv_sec = 0;
	time_150ms.tv_usec = 150000;

	debug = TRUE;

	/*
	 * Opzioni a riga di comando.
	 * TODO SP_BIND_IP, SP_BIND_PORT, IM_BIND_IP, IM_BIND_PORT
	 *      PROXY_REMOTE_IP, PROXY_REMOTE_PORT, SP_REMOTE_IP,
	 *      SP_REMOTE_PORT.
	 */
	if (argc != 1) {
		print_usage ();
		exit (EXIT_FAILURE);
	}

	/*
	 * Setup iniziale.
	 */
	iface_init_module ();
	dgram_init_module ();

	/* Creazione socket bindati per IM e SP
	 * FIXME non connettere sp ma ogni sendmsg usa addr ritornato da
	 * FIXME recvmsg. */
	sp->fd = socket_bound_and_connected (SP_BIND_IP, SP_BIND_PORT,
	                                     SP_CONN_IP, SP_CONN_PORT);
	if (sp->fd == -1)
		goto socket_bound_and_connected_err;
	im->fd = socket_bound_and_connected (IM_BIND_IP, IM_BIND_PORT,
	                                     NULL, NULL);
	if (im->fd == -1)
		goto socket_bound_and_connected_err;

	while (!is_done ()) {
		dgram_t *dg;
		int err;
		int nready;
		int next_tmout;
		struct timeval min;
		struct timeval left;
		iface_t *current_iface;
		/* per i cicli */
		iface_iterator_t ii;
		iface_t *if_ptr;

		/*
		 * Tutti i socket si devono aspettare dati ed errori.
		 */
		sp->events = POLLIN | POLLERR;
		sp->revents = 0;
		im->events = POLLIN | POLLERR;
		im->revents = 0;
		for (if_ptr = iface_iterator_get_first (&ii);
		     if_ptr != NULL;
		     if_ptr = iface_iterator_get_next (&ii)) {
			iface_reset_events (if_ptr);
			iface_set_events (if_ptr, POLLIN | POLLERR);
		}

		current_iface = iface_get_current ();
		if (debug) {
			char ifstr[100];
			iface_to_string (current_iface, ifstr);
			printf ("Wifi interface: %s\n", ifstr);
		}

		/*
		 * Gestione dei timeout: pulizia code e calcolo timeout minimo.
		 */
		gettime (&now);

		dgram_outward_all_unacked (&now);
		dgram_purge_all_old (&now);

		/* Keepalive. */
		for (if_ptr = iface_iterator_get_first (&ii);
		     if_ptr != NULL;
		     if_ptr = iface_iterator_get_next (&ii)) {
			iface_keepalive_left (if_ptr, &now, &left);
			tv_min (&min, &min, &left);
		}

		dgram_timeout_min (&min, &now);

		if (tv_cmp (&min, &time_0ms) <= 0)
			next_tmout = 0;
		else {
			assert (tv_cmp (&min, &time_0ms) > 0);
			next_tmout = (int)(tv2d (&min, FALSE) * 1000);
			assert (next_tmout > 0);
			assert (next_tmout <= 150);
		}

		/*
		 * Impostazione eventi attesi.
		 */

		/* Se ho dati ricevuti dal server, voglio scrivere al
		 * softphone */
		if (dgram_list_peek (DGRAM_INWARD) != NULL)
			sp->events |= POLLOUT;

		/* Se ho un'interfaccia wifi attiva e dati dal softphone,
		 * scrivo al server. */
		if (current_iface != NULL && dgram_list_peek (DGRAM_OUTWARD))
			iface_set_events (current_iface, POLLOUT);

		/* POLLOUT se scaduto keepalive. */
		for (if_ptr = iface_iterator_get_first (&ii);
		     if_ptr != NULL;
		     if_ptr = iface_iterator_get_next (&ii)) {
			iface_keepalive_left (if_ptr, &now, &left);
			if (tv_cmp (&left, &time_0ms) <= 0)
				iface_set_events (if_ptr, POLLOUT);
		}

		/*
		 * Poll
		 */
		iface_fill_pollfd (&fds[2], &ifaces_used);

		nready = poll (fds, 2 + ifaces_used, next_tmout);
		if (nready == -1) {
			perror ("poll");
			exit (EXIT_FAILURE);
		}

		iface_read_pollfd (&fds[2]);

		/*
		 * Eventi softphone.
		 */
		if (sp->revents & POLLERR) {
			fprintf (stderr,
				 "Errore di comunicazione con il softphone, "
				 "che si fa? Nel dubbio, me la filo.\n");
			exit (EXIT_FAILURE);
		}
		if (sp->revents & POLLIN) {
			dg = dgram_read (sp->fd);
			/* TODO controllo errore */
			dgram_list_add (DGRAM_OUTWARD, dg);
		}
		if (sp->revents & POLLOUT) {
			dg = dgram_list_pop (DGRAM_INWARD);
			assert (dg != NULL);
			dgram_write (sp->fd, dg);
			/* TODO controllo errore */
			dgram_free (dg);
		}

		/*
		 * Eventi interface monitor.
		 */
		if (im->revents & POLLERR) {
			fprintf (stderr,
				 "Errore di comunicazione con l'interface "
				 "monitor, che si fa? Nel dubbio, taglio la "
				 "corda.\n");
			exit (EXIT_FAILURE);
		}
		if (im->revents & POLLIN) {
			dg = dgram_read (im->fd);
			/* TODO riconfigurazione socket interfacce. */
			dgram_free (dg);
		}

		/*
		 * Spedizione interfaccia corrente.
		 */
		if (current_iface != NULL
		    && (dg = dgram_list_pop (DGRAM_OUTWARD)) != NULL
		    && iface_get_events (current_iface) & POLLOUT) {
			err = iface_write (current_iface, dg);
			/* TODO controllo errore */
		}

		/*
		 * Eventi per tutte le interfacce.
		 */
		for (if_ptr = iface_iterator_get_first (&ii);
		     if_ptr != NULL;
		     if_ptr = iface_iterator_get_next (&ii)) {
			int ev = iface_get_events (if_ptr);

			/* Ricezione. */
			if (ev & POLLIN) {
				dg = iface_read (if_ptr);
				/* TODO  controllo errore */
				dgram_list_add (DGRAM_INWARD, dg);
			}

			/* Spedizione keepalive. */
			if ((ev & POLLOUT)
			    && !(iface_keepalive_left (if_ptr, &now, &left))) {
				assert (tv_cmp (&left, &time_0ms) <= 0);
				dg = dgram_create_keepalive ();
				iface_write (if_ptr, dg);
				/* TODO  controllo errore */
				dgram_free (dg);
			}

			if (ev & POLLERR)
				iface_handle_err (if_ptr);
		}
	}

	return 0;

socket_bound_and_connected_err:
	perror ("socket_bound");
	exit (EXIT_FAILURE);
}
