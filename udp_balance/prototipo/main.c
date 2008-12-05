/*
 * udp_balancer: prototipo.
 */

#define     ULP_PROTO_MAIN


#include <assert.h>
#include <poll.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "crono.h"
#include "dgram.h"
#include "iface.h"
#include "types.h"
#include "util.h"

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

#ifdef NDEBUG
	/* Valori reali. */
	time_30ms.tv_sec = 0;
	time_30ms.tv_usec = 30000;

	time_150ms.tv_sec = 0;
	time_150ms.tv_usec = 150000;
#else
	/* Valori dilatati, per capire che succede. */
	time_30ms.tv_sec = ONE_MILLION - 1;
	time_30ms.tv_usec = 0;

	time_150ms.tv_sec = ONE_MILLION - 1;
	time_150ms.tv_usec = 0;
#endif /* NDEBUG */

	verbose = TRUE;


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
	/* srand (42); */     /* per non piangere debuggando. */
	srand (getpid ());
	iface_init_module ();
	dgram_init_module ();

	/* Creazione socket bindati per IM e SP
	 * FIXME non connettere sp ma ogni sendmsg usa addr ritornato da
	 * FIXME recvmsg. */
	sp->fd = socket_bound_conn (SP_LOC_IP, SP_LOC_PORT,
	                            SP_REM_IP, SP_REM_PORT);
	if (sp->fd == -1)
		goto socket_bound_conn_err;
	im->fd = socket_bound_conn (IM_LOC_IP, IM_LOC_PORT,
	                            IM_REM_IP, IM_REM_PORT);
	if (im->fd == -1)
		goto socket_bound_conn_err;

	while (!is_done ()) {
		dgram_t *dg;
		int nready;
		int next_tmout;
		struct timeval min;
		struct timeval left;
		iface_t *current_iface;
		/* per i cicli */
		iface_iterator_t ii;
		iface_t *if_ptr;

		if (verbose) {
			printf ("inward: ");
			dgram_list_print (DGRAM_INWARD);
			printf ("outward: ");
			dgram_list_print (DGRAM_OUTWARD);
			printf ("unaked: ");
			dgram_list_print (DGRAM_UNACKED);
		}

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
#ifndef NDEBUG
		{
			printf ("Interfaccia d'uscita: ");
			if (current_iface != NULL)
				iface_print (current_iface);
			else
				printf ("nessuna");
			printf ("\n"); fflush (stdout);
		}
#endif /* NDEBUG */

		/*
		 * Pulizia code datagram e calcolo timeout minimo.
		 */
		min.tv_sec = ONE_MILLION;
		min.tv_usec = 0;

		dgram_outward_all_unacked ();
		dgram_purge_all_old ();

		/* Keepalive. */
		for (if_ptr = iface_iterator_get_first (&ii);
		     if_ptr != NULL;
		     if_ptr = iface_iterator_get_next (&ii)) {
			iface_keepalive_left (if_ptr, &left);
			tv_min (&min, &min, &left);
		}

		dgram_timeout_min (&min);

		if (min.tv_sec == ONE_MILLION) {
			/* All'inizio non ci sono interfacce attive e poll si
			 * blocca in attesa di un messaggio di configurazione
			 * da parte dell'interface monitor. */
			next_tmout = -1;
#ifndef NDEBUG
			fprintf (stderr, "poll blocca indefinitamente\n");
#endif /* NDEBUG */
		} else if (tv_cmp (&min, &time_0ms) <= 0) {
			/* C'e' un timeout gia' scaduto, la poll e'
			 * istantanea. */
			next_tmout = 0;
#ifndef NDEBUG
			fprintf (stderr, "poll non blocca\n");
#endif /* NDEBUG */
		} else {
			/* Il timeout piu' prossimo a scadere e' > 0 */
			assert (tv_cmp (&min, &time_0ms) > 0);
			next_tmout = (int)(tv2d (&min, FALSE) * 1000);
			/* next_tmout puo' essere zero per effetto
			 * della conversione microsec -> millisec */
#ifdef NDEBUG
			assert (next_tmout <= 150);
#else
			fprintf (stderr, "poll blocca per %d ms\n", next_tmout);
#endif /* NDEBUG */
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
		     if_ptr = iface_iterator_get_next (&ii))
			if (iface_must_send_keepalive (if_ptr))
				iface_set_events (if_ptr, POLLOUT);

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
			struct timeval now;

			printf ("POLLIN softphone\n");
			dg = dgram_read (sp->fd, NULL, NULL);
			/* TODO controllo errore */
			assert (dg->dg_life_to == NULL);
			dg->dg_life_to = new_timeout (&time_150ms);
			gettime (&now);
			timeout_start (dg->dg_life_to, &now);
			dgram_list_add (DGRAM_OUTWARD, dg);
		}
		if (sp->revents & POLLOUT) {
			printf ("POLLOUT softphone\n");
			dg = dgram_list_pop (DGRAM_INWARD);
			assert (dg != NULL);
			dgram_write (sp->fd, dg, NULL, 0);
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
			char *cmd;
			char *ip;
			char *name;
			printf ("POLLIN interface monitor\n");
			dg = dgram_read (im->fd, NULL, NULL);
			parse_im_msg (&name, &cmd, &ip, dg->dg_data,
			              dg->dg_datalen);
			if (strcmp (cmd, "down") == 0)
				iface_down (name, ip);
			else
				iface_up (name, ip);
			free (name);
			free (cmd);
			free (ip);
			dgram_free (dg);
		}

		/*
		 * Spedizione interfaccia corrente.
		 */
		if (current_iface != NULL
		    && dgram_list_peek (DGRAM_OUTWARD) != NULL
		    && iface_get_events (current_iface) & POLLOUT) {
			struct timeval now;
			printf ("POLLOUT current iface");
			dg = dgram_list_pop (DGRAM_OUTWARD);
			iface_write (current_iface, dg);
			/* TODO controllo errore */
			assert (dg->dg_retry_to == NULL);
			dg->dg_retry_to = new_timeout (&time_30ms);
			gettime (&now);
			timeout_start (dg->dg_retry_to, &now);
			dgram_list_add (DGRAM_UNACKED, dg);
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
				printf ("POLLIN interfaccia\n");
				dg = iface_read (if_ptr);
				/* TODO  controllo errore */
				dgram_list_add (DGRAM_INWARD, dg);
			}

			/* Spedizione keepalive. */
			if ((ev & POLLOUT)
			    && iface_must_send_keepalive (if_ptr)) {
				printf ("POLLOUT interfaccia\n");
				dg = dgram_create_keepalive ();
				iface_write (if_ptr, dg);
				/* TODO controllo errore */
				dgram_free (dg);
			}

			if (ev & POLLERR)
				iface_handle_err (if_ptr);
		}
	}

	return 0;

socket_bound_conn_err:
	perror ("socket_bound_conn");
	exit (EXIT_FAILURE);
}
