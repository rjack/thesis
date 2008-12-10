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

	list_t out = list_create ((f_destroy_t)dgram_destroy,
	                          sizeof(dgram_t));
	list_t in = list_create ((f_destroy_t)dgram_destroy,
	                         sizeof(dgram_t));
	list_t unacked = list_create ((f_destroy_t)dgram_destroy,
	                              sizeof(dgram_t));
	list_t ifaces = list_create ((f_destroy_t)iface_destroy,
	                             sizeof(iface_t));

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
		int i;
		list_t rmvd;
		dgram_t *dg;
		void *args;
		int nready;
		int next_tmout;
		struct timeval min;
		struct timeval left;
		iface_t *current_iface;
		/* per i cicli */
		list_iterator_t lit;
		iface_t *if_ptr;
		struct timeval now;

		if (verbose) {
			printf ("in: ");
			list_foreach_do (in, (f_callback_t)dgram_print, NULL);
			printf ("\n");
			printf ("out: ");
			list_foreach_do (out, (f_callback_t)dgram_print,
			                 NULL);
			printf ("\n");
			printf ("unacked: ");
			list_foreach_do (unacked, (f_callback_t)dgram_print,
			                 NULL);
			printf ("\n");
		}

		/*
		 * Tutti i socket si devono aspettare dati ed errori.
		 */
		sp->events = POLLIN | POLLERR;
		sp->revents = 0;
		im->events = POLLIN | POLLERR;
		im->revents = 0;
		list_foreach_do (ifaces,
		                 (f_callback_t)iface_reset_events, NULL);
		args = my_alloc (sizeof(int));
		*(int *)args = POLLIN | POLLERR;
		list_foreach_do (ifaces, (f_callback_t)iface_set_events,
		                 args);
		free (args);

		current_iface = list_peek (ifaces);
		if (verbose) {
			printf ("Interfaccia d'uscita: ");
			if (current_iface != NULL)
				iface_print (current_iface);
			else
				printf ("nessuna");
			printf ("\n"); fflush (stdout);
		}

		/*
		 * Pulizia code datagram.
		 */

		/* Tutti i datagram che non hanno ricevuto l'ACK vanno
		 * travasati da unacked a out per ritrasmetterli. */
		rmvd = list_remove_if (unacked,
				       (f_compare_t)dgram_must_be_retransmitted,
	                               NULL);
		list_cat (out, rmvd);
		assert (list_is_empty (rmvd));
		list_destroy (rmvd);

		/* Tutti i datagram piu' vecchi di 150ms vanno scartati. */
		rmvd = list_remove_if (out,
		                       (f_compare_t)dgram_must_be_discarded,
		                       NULL);
		list_destroy (rmvd);

		/*
		 * Calcolo timeout minimo.
		 */
		min.tv_sec = ONE_MILLION;
		min.tv_usec = 0;

		/* Confronto min vs. keepalive. */
		for (if_ptr = list_iterator_get_first (ifaces, &lit);
		     if_ptr != NULL;
		     if_ptr = list_iterator_get_next (ifaces, &lit)) {
			iface_keepalive_left (if_ptr, &left);
			tv_min (&min, &min, &left);
		}

		/* Confronto min vs. timeout ack e vita dei datagram delle
		 * code out e unaked. */
		list_foreach_do (out, (f_callback_t)dgram_min_timeout, &min);
		list_foreach_do (unacked, (f_callback_t)dgram_min_timeout,
		                 &min);


		if (min.tv_sec == ONE_MILLION) {
			/* Se non ci sono ifacce attive, timeout indefinito in
			 * attesa di un messaggio dell'interface monitor. */
			next_tmout = -1;
			if (verbose)
				fprintf (stderr,
				         "poll blocca indefinitamente\n");
		} else if (tv_cmp (&min, &time_0ms) <= 0) {
			/* C'e' un timeout gia' scaduto, la poll e'
			 * istantanea. */
			next_tmout = 0;
			if (verbose)
				fprintf (stderr, "poll non blocca\n");
		} else {
			/* Il timeout piu' prossimo a scadere e' > 0 */
			assert (tv_cmp (&min, &time_0ms) > 0);
			next_tmout = (int)(tv2d (&min, FALSE) * 1000);
			/* next_tmout puo' essere zero per effetto
			 * della conversione microsec -> millisec */
#ifdef NDEBUG
			assert (next_tmout <= 150);
#endif /* NDEBUG */
			if (verbose)
				fprintf (stderr, "poll blocca per %d ms\n",
				         next_tmout);
		}

		/*
		 * Impostazione eventi attesi.
		 */

		/* Se ho dati ricevuti dal server, voglio scrivere al
		 * softphone */
		if (list_peek (in) != NULL)
			sp->events |= POLLOUT;

		/* Se ho un'interfaccia wifi attiva e dati dal softphone,
		 * scrivo al server. */
		if (current_iface != NULL && list_peek (out))
			iface_set_events (current_iface, POLLOUT);

		/* POLLOUT se scaduto keepalive. */
		for (if_ptr = list_iterator_get_first (ifaces, &lit);
		     if_ptr != NULL;
		     if_ptr = list_iterator_get_next (ifaces, &lit))
			if (iface_must_send_keepalive (if_ptr))
				iface_set_events (if_ptr, POLLOUT);

		/*
		 * Poll
		 */

		/* Travaso pollfd da interfacce all'array. */
		for (i = 2, if_ptr = list_iterator_get_first (ifaces, &lit);
		     if_ptr != NULL;
		     i++, if_ptr = list_iterator_get_next (ifaces, &lit))
			memcpy (&fds[i], iface_get_pollfd (if_ptr),
			        sizeof(struct pollfd));

		nready = poll (fds, 2 + ifaces_used, next_tmout);
		if (nready == -1) {
			perror ("poll");
			exit (EXIT_FAILURE);
		}

		/* Travaso dall'array alle interfacce. */
		for (i = 2, if_ptr = list_iterator_get_first (ifaces, &lit);
		     if_ptr != NULL;
		     i++, if_ptr = list_iterator_get_next (ifaces, &lit))
			iface_set_pollfd (if_ptr, &fds[i]);

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
			if (verbose)
				printf ("POLLIN softphone\n");
			dg = dgram_read (sp->fd, NULL, NULL);
			/* TODO controllo errore */
			dgram_set_life_timeout (dg);
			list_enqueue (out, dg);
		}
		if (sp->revents & POLLOUT) {
			if (verbose)
				printf ("POLLOUT softphone\n");
			dg = list_dequeue (in);
			assert (dg != NULL);
			dgram_write (sp->fd, dg, NULL, 0);
			/* TODO controllo errore */
			dgram_destroy (dg);
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
			if (verbose)
				printf ("POLLIN interface monitor\n");
			dg = dgram_read (im->fd, NULL, NULL);
			parse_im_msg (&name, &cmd, &ip, dg->dg_data,
			              dg->dg_datalen);
			if (strcmp (cmd, "down") == 0)
				; /* FIXME iface_down (name, ip); */
			else
				; /* FIXME iface_up (name, ip); */
			free (name);
			free (cmd);
			free (ip);
			dgram_destroy (dg);
		}

		/*
		 * Spedizione interfaccia corrente.
		 */
		if (current_iface != NULL
		    && list_peek (out) != NULL
		    && (iface_get_events (current_iface) & POLLOUT)) {
			if (verbose)
				printf ("POLLOUT current iface");
			dg = list_dequeue (out);
			iface_write (current_iface, dg);
			/* TODO controllo errore */
			assert (dg->dg_retry_to == NULL);
			dg->dg_retry_to = new_timeout (&time_30ms);
			gettime (&now);
			timeout_start (dg->dg_retry_to, &now);
			list_enqueue (unacked, dg);
		}

		/*
		 * Eventi per tutte le interfacce.
		 */
		for (if_ptr = list_iterator_get_first (ifaces, &lit);
		     if_ptr != NULL;
		     if_ptr = list_iterator_get_next (ifaces, &lit)) {
			int ev = iface_get_events (if_ptr);

			/* Ricezione. */
			if (ev & POLLIN) {
				if (verbose)
					printf ("POLLIN interfaccia\n");
				dg = iface_read (if_ptr);
				/* TODO controllo errore */
				list_enqueue (in, dg);
			}

			/* Spedizione keepalive. */
			if ((ev & POLLOUT)
			    && iface_must_send_keepalive (if_ptr)) {
				if (verbose)
					printf ("POLLOUT interfaccia\n");
				dg = dgram_create_keepalive ();
				iface_write (if_ptr, dg);
				/* TODO controllo errore */
				dgram_destroy (dg);
			}

			if (ev & POLLERR) {
				dgram_t *dg_err;
				dg_err = iface_handle_err (if_ptr);
				switch (errno) {

				case E_IFACE_FATAL:
					list_iterator_get_next (ifaces, &lit);
					list_remove (if_ptr);
					iface_destroy (if_ptr);
					break;

				/* TED ha confermato la ricezione di dg_err da
				 * parte dell'AP: possiamo scartarlo. */
				case E_IFACE_DG_ACK:
					rmvd = list_remove_if (out, (f_compare_t)dgram_cmp_id, dg_err);
					list_destroy (rmvd);
					rmvd = list_remove_if (unacked, (f_compare_t)dgram_cmp_id, dg_err);
					list_destroy (rmvd);
					break;

				/* TED ha segnalato che dg_err NON e' arrivato
				 * all'AP: se e' ancora tra gli unacked
				 * bisogna ritrasmetterlo. */
				case E_IFACE_DG_NAK:
					rmvd = list_remove_if (unacked, (f_compare_t)dgram_cmp_id, dg_err);
					assert (list_length (rmvd) <= 1);
					assert (list_length (rmvd) >= 0);
					while (!list_is_empty (rmvd))
						list_push (out, list_dequeue (rmvd));
					list_dequeue (rmvd);
					break;
				}
			}
		}

		/*
		 * TODO simulare ted
		 * se TED_FAKE_POSITIVE == TRUE:
		 * 	TED avverte quando un datagram arriva all'AP
		 *
		 * se TED_FAKE_POSITIVE == FALSE:
		 * 	TED avverte quando un datagram NON arriva all'AP
		 */
	}

	return 0;

socket_bound_conn_err:
	perror ("socket_bound_conn");
	exit (EXIT_FAILURE);
}
