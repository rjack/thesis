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
#include "ted_fake.h"
#include "types.h"
#include "util.h"


/****************************************************************************
			       Variabili locali
****************************************************************************/

static const char *program_name = NULL;


/****************************************************************************
				   Costanti
****************************************************************************/

#define     IFACE_MAX       16


/****************************************************************************
			       Funzioni locali
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


static void
find_iface_and_set_suspected (dgram_t *dg, list_t *ifaces)
{
	iface_t *susp;

	assert (dg != NULL);
	assert (ifaces != NULL);

	susp = dg->dg_if_ptr;

	/* Puo' essere NULL perche' tra spedizione datagram e ricezione NAK (o
	 * scadenza timeout) l'interfaccia puo' essere stata tirata giu'
	 * dall'interface manager o da un errore ICMP. */
	if (susp != NULL) {
		assert (list_contains (*ifaces, (f_bool_t)ptr_eq, dg->dg_if_ptr, 0));
		iface_set_suspected (susp);
		if (verbose) {
			iface_print (susp);
			printf (" impostata come sospetta.\n");
		}
	}
}


static iface_t *
select_working_interface (list_t ifaces)
{
	iface_t *selected;

	selected = list_contains (ifaces, (f_bool_t)iface_is_working, NULL, 0);
	if (selected == NULL)
		selected = list_peek (ifaces);

	return selected;
}


/****************************************************************************
				     Main
****************************************************************************/

int
main (const int argc, const char *argv[])
{
	struct pollfd fds[2 + IFACE_MAX];

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
	time_30ms.tv_sec = 3;
	time_30ms.tv_usec = 0;

	time_150ms.tv_sec = 15;
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

	/*
	 * Init TED farlocco.
	 */
	ted_fake_init ();

	while (!is_done ()) {
		int i, len;
		list_t rmvd;
		dgram_t *dg;
		int nready;
		int next_tmout;
		struct timeval min;
		struct timeval left;
		iface_t *current_iface;
		/* per i cicli */
		list_iterator_t lit;
		iface_t *if_ptr;
		struct timeval now;

		/*
		 * Tutti i socket si devono aspettare dati ed errori.
		 */
		sp->events = POLLIN | POLLERR;
		sp->revents = 0;
		im->events = POLLIN | POLLERR;
		im->revents = 0;
		list_foreach_do (ifaces,
		                 (f_callback_t)iface_reset_events, NULL);

		/*
		 * Pulizia code datagram.
		 */

		/* Tutti i datagram piu' vecchi di 150ms vanno scartati. */
		rmvd = list_remove_if (out,
		                       (f_bool_t)dgram_must_discarded,
		                       NULL);
		if (verbose) {
			printf ("Datagram scaduti rimossi da coda out: ");
			list_foreach_do (rmvd, (f_callback_t)dgram_print, NULL);
			printf ("\n");
		}
		list_destroy (rmvd);

		rmvd = list_remove_if (unacked,
		                       (f_bool_t)dgram_must_discarded,
		                       NULL);
		if (verbose) {
			printf ("Datagram scaduti rimossi da coda unacked: ");
			list_foreach_do (rmvd, (f_callback_t)dgram_print, NULL);
			printf ("\n");
		}
		list_destroy (rmvd);

		/* Tutti i datagram che non hanno ricevuto l'ACK vanno
		 * travasati da unacked a out per ritrasmetterli.
		 * Le interfacce sui cui erano stati spediti diventano
		 * "sospette". */
		rmvd = list_remove_if (unacked,
				       (f_bool_t)dgram_must_retry,
	                               NULL);
		list_foreach_do (rmvd,
		                 (f_callback_t)find_iface_and_set_suspected,
		                 &ifaces);
		if (verbose) {
			printf ("Datagram da ritrasmettere per timeout: ");
			list_foreach_do (rmvd, (f_callback_t)dgram_print, NULL);
			printf ("\n");
		}
		list_cat (out, rmvd);
		assert (list_is_empty (rmvd));
		list_destroy (rmvd);

		if (verbose) {
			printf ("in: ");
			list_foreach_do (in, (f_callback_t)print_dot, NULL);
			printf ("\n");
			printf ("out: ");
			list_foreach_do (out, (f_callback_t)print_dot, NULL);
			printf ("\n");
			printf ("unacked: ");
			list_foreach_do (unacked, (f_callback_t)print_dot, NULL);
			printf ("\n");
		}

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
		 * code out e unacked. */
		list_foreach_do (out, (f_callback_t)dgram_min_timeout, &min);
		list_foreach_do (unacked, (f_callback_t)dgram_min_timeout,
		                 &min);

		/* Tirando le somme... */
		if (ted_fake_events_pending ()) {
			next_tmout = 0;
			if (verbose)
				printf ("ted_fake_events_pending ha detto si'\n");
		} else if (min.tv_sec == ONE_MILLION) {
			/* Se non ci sono ifacce attive, timeout indefinito in
			 * attesa di un messaggio dell'interface monitor. */
			next_tmout = -1;
			if (verbose)
				printf ("poll blocca indefinitamente\n");
		} else if (tv_cmp (&min, &time_0ms) <= 0) {
			/* C'e' un timeout gia' scaduto, la poll e'
			 * istantanea. */
			next_tmout = 0;
			if (verbose)
				printf ("poll non blocca\n");
		} else {
			/* Il timeout piu' prossimo a scadere e' > 0 */
			assert (tv_cmp (&min, &time_0ms) > 0);
			next_tmout = (int)(tv2d (&min, FALSE) * 1000);
			/* next_tmout puo' essere zero per effetto
			 * della conversione microsec -> millisec */
			if (verbose)
				printf ("poll blocca per %d ms\n", next_tmout);
		}


		/*
		 * Scelta interfaccia.
		 */
		if (!iface_is_working (current_iface))
			current_iface = select_working_interface (ifaces);
		if (verbose) {
			printf ("current_iface: ");
			if (current_iface != NULL)
				iface_print (current_iface);
			else
				printf ("nessuna");
			printf ("\n");
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

		nready = poll (fds, 2 + list_length (ifaces), next_tmout);
		if (nready == -1) {
			perror ("poll");
			exit (EXIT_FAILURE);
		}

		/* Travaso dall'array alle interfacce. */
		for (i = 2, if_ptr = list_iterator_get_first (ifaces, &lit);
		     if_ptr != NULL;
		     i++, if_ptr = list_iterator_get_next (ifaces, &lit))
			iface_set_pollfd (if_ptr, &fds[i]);

		list_foreach_do (ifaces,
		                 (f_callback_t)ted_fake_set_errqueue_events,
		                 NULL);

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
				printf ("Lettura dal softphone: ");
			dg = dgram_read (sp->fd, NULL, NULL);
			/* TODO controllo errore */
			dgram_set_life_timeout (dg);
			list_enqueue (out, dg);
			if (verbose) {
				dgram_print (dg);
				printf ("\n");
			}
		}
		if (sp->revents & POLLOUT) {
			if (verbose)
				printf ("Scrittura al softphone\n");
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
			iface_t *if_ptr;

			dg = dgram_read (im->fd, NULL, NULL);
			parse_im_msg (&name, &cmd, &ip, dg->dg_data,
			              dg->dg_datalen);
			if (strcmp (cmd, "down") == 0) {
				rmvd = list_remove_if (ifaces, (f_bool_t)iface_has_name, name);
				if (!list_is_empty (rmvd)) {
					assert (list_length (rmvd) == 1);
					if_ptr = list_dequeue (rmvd);
					list_destroy (rmvd);
					iface_destroy (if_ptr);
					list_foreach_do (out, (f_callback_t)dgram_clear_iface_ptr, if_ptr);
					list_foreach_do (unacked, (f_callback_t)dgram_clear_iface_ptr, if_ptr);
					ted_fake_clear_iface_ptr (if_ptr);
					if (if_ptr == current_iface)
						current_iface = select_working_interface (ifaces);
				}
			} else {
				if_ptr = iface_create (name, ip);
				len = list_push (ifaces, if_ptr);
				assert (len != LIST_ERR);
			}
			dgram_destroy (dg);
			free (name);
			free (cmd);
			free (ip);
		}

		/*
		 * Spedizione interfaccia corrente.
		 */
		if (current_iface != NULL
		    && list_peek (out) != NULL
		    && (iface_get_events (current_iface) & POLLOUT)) {
			dg = list_dequeue (out);
			iface_write (current_iface, dg);
			/* TODO controllo errore */
			if (current_iface->if_firmware_positive) {
				if (dg->dg_retry_to == NULL)
					dg->dg_retry_to = timeout_create (&time_30ms);
				gettime (&now);
				timeout_start (dg->dg_retry_to, &now);
			} else
				/* Se il firmware e' negativo ritrasmette solo
				 * quando arriva una notifica dal TED. */
				if (dg->dg_retry_to != NULL)
					dgram_destroy_reply_timeout (dg);

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
				dg = iface_read (if_ptr);
				/* TODO controllo errore */
				list_enqueue (in, dg);
			}

			/* Spedizione keepalive. */
			if ((ev & POLLOUT)
			    && iface_must_send_keepalive (if_ptr)) {
				dg = dgram_create_keepalive ();
				iface_write (if_ptr, dg);
				/* TODO controllo errore */
				dgram_destroy (dg);
			}

			if (ev & POLLERR) {
				int dg_id;

				dg_id = iface_handle_err (if_ptr);
				switch (errno) {

				case E_IFACE_FATAL:
					list_iterator_get_next (ifaces, &lit);
					rmvd = list_remove_if (ifaces, ptr_eq, if_ptr);
					assert (list_length (rmvd) == 1);
					list_destroy (rmvd);
					break;

				/* TED ha confermato la ricezione di dg_id da
				 * parte dell'AP: possiamo scartarlo. */
				case E_IFACE_DG_ACK:
					if (verbose) {
						printf ("Rimozione dgram ACKED su ");
						iface_print (if_ptr);
						printf (": ");
					}
					/* Rimozione da out. */
					rmvd = list_remove_if (out, (f_bool_t)dgram_has_id, &dg_id);
					if (verbose) {
						printf ("   out ");
						list_foreach_do (rmvd, (f_callback_t)dgram_print, NULL);
						printf ("\n");
					}
					list_destroy (rmvd);

					/* Rimozione da unacked. */
					rmvd = list_remove_if (unacked, (f_bool_t)dgram_has_id, &dg_id);
					if (verbose) {
						printf ("   unacked ");
						list_foreach_do (rmvd, (f_callback_t)dgram_print, NULL);
						printf ("\n");
					}
					list_destroy (rmvd);
					break;

				/* TED ha segnalato che dg_err NON e' arrivato
				 * all'AP: se e' ancora tra gli unacked
				 * bisogna ritrasmetterlo e segnare
				 * l'interfaccia che lo aveva spedito come
				 * sospetta. */
				case E_IFACE_DG_NAK:
					rmvd = list_remove_if (unacked, (f_bool_t)dgram_has_id, &dg_id);
					assert (list_length (rmvd) <= 1);
					assert (list_length (rmvd) >= 0);
					while (!list_is_empty (rmvd)) {
						dgram_t *dg;
						dg = list_dequeue (rmvd);
						if (verbose) {
							dgram_print (dg);
							printf (" non e' stato ricevuto, rimesso in out.\n");
						}
						find_iface_and_set_suspected (dg, &ifaces);
						list_push (out, dg);
					}
					list_destroy (rmvd);
					break;
				}
			}
		}
	}

	return 0;

socket_bound_conn_err:
	perror ("socket_bound_conn");
	exit (EXIT_FAILURE);
}
