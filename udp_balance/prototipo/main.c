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

/*
 * Array di struct polldf.
 */
#define     MAXIFACENUM     16

/* Indici importanti */
#define     SP_I                0         /* softphone */
#define     IM_I                1         /* interface monitor */
#define     CURRENT_IFACE_I     2         /* interfaccia in uso */


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

static struct pollfd fds[MAXIFACENUM];
static size_t fds_used = 2;
static size_t fds_len = MAXIFACENUM;

/*
 * Code per i datagram.
 */
/* da softphone a server */
static struct dgram *data_out = NULL;

/* da server a softphone */
static struct dgram *data_in = NULL;

/* spediti, da confermare */
static struct dgram *data_unakd = NULL;

/* msg config interfacce */
static struct dgram *data_iface = NULL;

/* datagram scartati, pronti per essere riutilizzati */
static struct dgram *data_discarded = NULL;

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

static bool
must_be_discarded (struct dgram *dg)
/* Ritorna TRUE se dg e'piu' vecchio di 150ms,
 *         FALSE altrimenti.
 * NON dealloca i timeout. */
{
	struct timeval left;

	timeout_left (dg->dg_life_to, &now, &left);

	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


static bool
must_be_retransmitted (struct dgram *dg)
/* Ritorna TRUE e dealloca dg_retry_to se dg e'piu' vecchio di 30ms,
 *         FALSE altrimenti. */
{
	struct timeval left;

	timeout_left (dg->dg_retry_to, &now, &left);

	if (tv_cmp (&left, &time_0ms) <= 0) {
		free (dg->dg_retry_to);
		dg->dg_retry_to = NULL;
		return TRUE;
	}
	return FALSE;
}


static struct dgram *
list_cat (struct dgram *fst, struct dgram *snd)
/* Ritorna la concatenazione delle due liste fst e snd. */
{
	struct dgram *tail;

	if (fst == NULL)
		return snd;
	if (snd == NULL)
		return fst;

	for (tail = fst; tail->dg_next != NULL; tail = tail->dg_next);
	tail->dg_next = snd;

	return fst;
}


static struct dgram *
list_remove_if (bool (*test)(struct dgram *), struct dgram **lst)
/* Rimuove da lst tutti gli elementi che soddisfano test e li ritorna in una
 * lista. */
{
	struct dgram *cur;
	struct dgram *rmvd = NULL;
	struct dgram **rmvd_tp = &rmvd;
	struct dgram *passd = NULL;
	struct dgram **passd_tp = &passd;

	assert (lst != NULL);

	if (*lst == NULL)
		return NULL;

	for (cur = *lst; cur != NULL; cur = cur->dg_next)
		if (test (cur)) {
			*rmvd_tp = cur;
			rmvd_tp = &cur->dg_next;
		} else {
			*passd_tp = cur;
			passd_tp = &cur->dg_next;
		}

	*rmvd_tp = NULL;
	*passd_tp = NULL;

	*lst = passd;
	return rmvd;
}


static void
collect_garbage (void)
/* Dealloca tutte le strutture dati che sono rimaste in memoria a girarsi i
 * pollici. */
{
	;
}


static fd_t
socket_bound (const char *bind_ip, const char *bind_port)
/* Ritorna un socket AF_INET SOCK_DGRAM e lo binda all'indirizzo e alla porta
 * dati. */
{
	int err;
	fd_t new_sfd;
	struct addrinfo addr_hints;
	struct addrinfo *addr_results;
	struct addrinfo *res;

	/* getaddrinfo hints */
	addr_hints.ai_family = AF_INET;
	addr_hints.ai_socktype = SOCK_DGRAM;
	addr_hints.ai_protocol = IPPROTO_UDP;
	addr_hints.ai_flags = AI_NUMERICSERV;
	addr_hints.ai_next = NULL;
	addr_hints.ai_addr = NULL;
	addr_hints.ai_addrlen = 0;
	addr_hints.ai_canonname = NULL;

	/* getaddrinfo */
	err = getaddrinfo (bind_ip, bind_port, &addr_hints, &addr_results);
	if (err) {
		fprintf (stderr, "getaddrinfo: %s\n", gai_strerror (err));
		goto getaddrinfo_err;
	}

	/* Prova gli addrinfo ritornati. */
	new_sfd = -1;
	res = addr_results;
	while (new_sfd == -1 && res != NULL) {
		assert (res->ai_family == AF_INET);
		assert (res->ai_socktype == SOCK_DGRAM);
		assert (res->ai_protocol == IPPROTO_UDP);

		new_sfd = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP);
		if (new_sfd == -1)
			continue;

		err = bind (new_sfd, res->ai_addr, res->ai_addrlen);
		if (err) {
			close (new_sfd);
			new_sfd = -1;
			res = res->ai_next;
		}
	}
	if (new_sfd == -1)
		goto bind_err;

	freeaddrinfo (addr_results);
	return new_sfd;

bind_err:
	freeaddrinfo (addr_results);
getaddrinfo_err:
	return -1;
}


static void *
my_alloc (size_t nbytes)
/* Alloca nbytes di memoria. Se non riesce dealloca roba inutilizzata e
 * riprova. Se ancora non riesce, esce dal programma. */
{
	void *new;

	assert (nbytes > 0);

	new = malloc (nbytes);
	if (new == NULL) {
		collect_garbage ();
		new = malloc (nbytes);
		if (new == NULL) {
			perror ("Errore allocazione");
			exit (EXIT_FAILURE);
		}
	}

	return new;
}


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
	fds[SP_I].fd = socket_bound (SP_BIND_IP, SP_BIND_PORT);
	fds[IM_I].fd = socket_bound (IM_BIND_IP, IM_BIND_PORT);
	if (fds[SP_I].fd == -1 || fds[IM_I].fd == -1)
		goto socket_bound_err;

	/* TODO setsockopt IP_RECVERR */

	gettime (&now);
	timeout_start (&keepalive, &now);

	while (!is_done ()) {
		int nready;
		int next_tmout;

		/* Azzera eventi */
		for (i = 0; i < fds_used; i++) {
			fds[i].events = 0;
			fds[i].revents = 0;
		}

		/*
		 * Gestione dei timeout: pulizia code e calcolo timeot minimo.
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

		if (data_in != NULL)
			fds[SP_I].events |= POLLOUT;

		if (/* TODO interfaces () != NULL && */ data_out != NULL)
			fds[CURRENT_IFACE_I].events |= POLLOUT;

		nready = poll (fds, fds_used, next_tmout);

		/* Eventi softphone. */
		if (fds[SP_I].revents & POLLIN) {
			/* leggi datagram da fds[SP_I].fd */
			/* aggiungi timestamp */
			/* mettilo in data_in */
		}
	}

	return 0;

socket_bound_err:
	return 1;
}
