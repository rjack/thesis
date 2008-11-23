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
	int i;

	/* TODO sposta ste schifezze nel blocco giusto. */
	timeout_t keepalive;
	bool must_send_keepalive = FALSE;

	struct pollfd fds[2 + IFACE_MAX];
	size_t ifaces_used = 0;

	struct pollfd *sp = &fds[0];
	struct pollfd *im = &fds[1];

	/*
	 * Init variabili locali al modulo.
	 */
	program_name = argv[0];
	timeout_set (&keepalive, &time_150ms);

	/*
	 * Opzioni a riga di comando.
	 * TODO SP_BIND_IP, SP_BIND_PORT, IM_BIND_IP, IM_BIND_PORT
	 */
	if (argc != 1) {
		print_usage ();
		exit (EXIT_FAILURE);
	}

	/*
	 * Setup iniziale.
	 */
	gettime (&now);
	timeout_start (&keepalive, &now);

	/* Creazione socket bindati e connessi per IM e SP. */
	sp->fd = socket_bound (SP_BIND_IP, SP_BIND_PORT);

	while (!is_done ()) {
		dgram_t *dg;
		int nready;
		int next_tmout;
		struct timeval min;
		struct timeval left;
		iface_t *current_iface;

		/*
		 * Reset eventi.
		 * Tutti i socket si devono aspettare dati ed errori.
		 */
		sp->events = 0 | POLLIN | POLLERR;
		sp->revents = 0;
		im->events = 0 | POLLIN | POLLERR;
		im->revents = 0;
		iface_foreach_do (iface_set_events,
		                  arg_create (0 | POLLIN | POLLERR,
		                              sizeof(int)));

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

		/* Controllo keepalive. */
		timeout_left (&keepalive, &now, &min);
		if (tv_cmp (&min, &time_0ms) <= 0)
			must_send_keepalive = TRUE;

		dgram_timeout_min (&left);
		tv_min (&min, &min, &left);

		if (must_send_keepalive)
			next_tmout = 0;
		else {
			assert (tv_cmp (&min, &time_0ms) > 0)
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
			iface_set_events (POLLOUT);

		/* Se e' scaduto il keepalive, ogni interfaccia wifi deve
		 * provare a spedirlo. */
		if (must_send_keepalive)
			iface_foreach_do (iface_set_events,
					  arg_create (POLLOUT, sizeof(int)));

		iface_fill_pollfd (&fds[2], &ifaces_used);

		nready = poll (fds, 2 + ifaces_used, next_tmout);
		if (nready == -1) {
			perror ("poll");
			exit (EXIT_FAILURE);
		}

		iface_read_pollfd (&fds[2], ifaces_used);

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
			dgram_write (sp->fd);
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
			iface_monitor_handle_msg (dg);
			dgram_free (dg);
		}

		/*
		 * Spedizione interfaccia corrente.
		 */
		if (current_iface != NULL
		    && iface_get_events (current_iface) & POLLOUT) {
			dg = dgram_list_pop (DGRAM_OUTWARD);
			assert (dg != NULL);
			iface_write (current_iface, dg);
			/* TODO controllo errore */
		}

		/*
		 * Spedizione keepalive.
		 */
		if (must_send_keepalive) {
			dg = dgram_keepalive ();
			iface_foreach_do (iface_if_pollout_write,
			                  arg_create (dg, sizeof(dg)));
			dgram_free (dg);
		}

		/*
		 * Ricezione dati ed errori.
		 */
		iface_foreach_do (iface_if_pollin_read, NULL);
		iface_foreach_do (iface_if_pollerr_handle, NULL);
	}

	return 0;
}
