/*
 * udp_balancer: prototipo.
 */

#define     _POSIX_C_SOURCE     1     /* per getaddrinfo */

#include <assert.h>
#include <errno.h>
#include <linux/types.h>     /* workaround bug ubuntu: serve per errueue.h */
#include <linux/errqueue.h>
#include <netdb.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "types.h"
#include "crono.h"


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
static struct timeval now;

timeout_t keepalive;
bool must_send_keepalive = FALSE;

/* Timeval di comodo. */
const struct timeval time_0ms = { 0, 0 };
const struct timeval time_30ms = { 0, 30000 };
const struct timeval time_150ms = { 0, 150000 };


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
			      Funzioni esportate
****************************************************************************/

int
main (const int argc, const char *argv[])
{
	int i;

	/*
	 * Init variabili locali al modulo.
	 */
	program_name = argv[0];
	timeout_set (&keepalive, &time_150ms);

	/*
	 * Legenda:
	 * SP = softphone, PX = proxy, TED = trasmission error detector,
	 * IM = interface monitor
	 *
	 * Eventi:
	 *
	 * leggo da SP -> dati outward
	 *
	 * leggo da PX -> dati inward
	 *
	 * tmout keepalive -> dati outward
	 *
	 * dati inward -> scrivo a SP
	 *
	 * dati outward -> scrivo a PX
	 *
	 * dati netconf -> fix socket
	 *
	 * tmout qualita' conversazione (150ms) -> pulizia dati vecchi outward
	 *
	 * scrivo a PX -> reset tmout keepalive
	 *             -> datagram nel limbo, con associato tmout 30ms
	 *
	 * tmout limbo -> pacchetto scaduto in outward
	 *                (XXX in coda o in testa?)
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

	while (!is_done ()) {
		int nready;
		int next_tmout;
		struct pollfd *wifi_active_set;
		struct pollfd *wifi_suspected_set;

		wifi_active_set = get_wifi_active_set ();
		wifi_suspected_set = get_wifi_suspected_set ();

		/* Azzera eventi */
		for (i = 0; i < fds_used; i++) {
			fds[i].events = 0;
			fds[i].revents = 0;
		}

		/*
		 * Gestione dei timeout: pulizia code e calcolo timeout minimo.
		 */
		gettime (&now);

		/* Trasferimento in data_out dei pacchetti non confermati da
		 * TED. */
		data_out = list_cat (list_remove_if (must_be_retransmitted,
		                                     &data_unakd),
		                     data_out);

		/* Eliminazione datagram vecchi. */
		data_discarded = list_cat (list_remove_if (must_be_discarded,
		                                           &data_out),
		                           data_discarded);


		/* Calcolo minimo timeout. */
		{
			struct dgram *dg;
			struct timeval min;
			struct timeval left;

			min.tv_sec = ONE_MILLION;
			min.tv_usec = 0;

			/* Data unaked */
			for (dg = data_unakd; dg != NULL; dg = dg->dg_next) {
				assert (dg->dg_life_to != NULL);
				assert (dg->dg_retry_to != NULL);
				timeout_left (dg->dg_life_to, &now, &left);
				if (tv_cmp (&left, &min) < 0)
					min = left;
			}

			/* Data out */
			for (dg = data_out; dg != NULL; dg = dg->dg_next) {
				assert (dg->dg_retry_to == NULL);
				timeout_left (dg->dg_life_to, &now, &left);
				if (tv_cmp (&left, &min) < 0)
					min = left;
			}

			/* Tutti i timeout scaduti sono stati tolti dalle
			 * code, quindi min non puo' essere <= 0 */
			assert (min.tv_sec > 0);
			assert (min.tv_usec >= 0);

			/* Keepalive invece puo' essere scaduto. */
			timeout_left (&keepalive, &now, &left);
			if (tv_cmp (&left, &min) < 0)
				min = left;
			if (tv_cmp (&left, &time_0ms) <= 0)
				must_send_keepalive = TRUE;

			if (min.tv_sec == ONE_MILLION)
				next_tmout = -1;
			else if (tv_cmp (&min, &time_0ms) <= 0)
				next_tmout = 0;
			else {
				next_tmout = (int)(tv2d (&min, FALSE) * 1000);
				assert (next_tmout <= 150);
			}
		}

		/*
		 * Impostazione eventi attesi.
		 */

		/* Se ho dati ricevuti dal server, voglio scrivere al
		 * softphone */
		if (data_in != NULL)
			fds[SP_I].events |= POLLOUT;

		/* Se ho un'interfaccia wifi attiva e dati dal softphone,
		 * scrivo al server. */
		if (data_out != NULL) {
			if (wifi_active_set != NULL)
				wifi_active_set->events |= POLLOUT;
			else if (wifi_suspected_set != NULL)
				wifi_suspected_set->events |= POLLOUT;
		}

		/* Se e' scaduto il keepalive, ogni interfaccia wifi deve
		 * provare a spedirlo. */
		if (must_send_keepalive)
			for (i = CUR_IFACE_I; i < fds_used; i++)
				fds[i].events |= POLLOUT;

		/* Tutti i socket si aspettano dati ed errori. */
		for (i = 0; i < fds_used; i++)
			fds[i].events |= POLLIN | POLLERR;

		nready = poll (fds, fds_used, next_tmout);
		if (nready == -1) {
			perror ("poll");
			exit (EXIT_FAILURE);
		}

		/*
		 * Eventi softphone.
		 */
		if (fds[SP_I].revents & POLLIN) {
			/* leggi datagram da fds[SP_I].fd */
			/* mettilo in data_out */
		}
		if (fds[SP_I].revents & POLLOUT) {
			assert (data_in != NULL);
		}
	}

	return 0;

socket_bound_err:
	return 1;
}
