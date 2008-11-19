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


/****************************************************************************
			Definizioni di tipo e costanti
****************************************************************************/

#define     MIN(a,b)     ((a) < (b) ? (a) : (b))

#define     ONE_MILLION     1000000

typedef int bool;

#ifdef FALSE
#  undef FALSE
#endif
#define FALSE ((bool)0)

#ifdef TRUE
#  undef TRUE
#endif
#define TRUE (!FALSE)


typedef int fd_t;


/*
 * Array di struct polldf.
 */
#define     MAXIFACENUM     16

/* Indici importanti */
#define     SP_I                0         /* softphone */
#define     IM_I                1         /* interface monitor */
#define     CURRENT_IFACE_I     2         /* interfaccia in uso */

/*
 * Datagram.
 */
struct dgram {
	struct timeval dg_tstamp;  /* istante di arrivo */
	char *dg_data;             /* dati letti da recvmsg */
	size_t dg_datalen;         /* lunghezza dati */
	struct dgram *dg_next;     /* prossimo dgram in coda */
};


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


/****************************************************************************
			  Prototipi funzioni locali
****************************************************************************/

static void *my_alloc (size_t nbytes);
static void print_usage (void);
static bool is_done (void);
static void collect_garbage (void);
static fd_t socket_bound (const char *bind_ip, const char *bind_port);
static void gettime (struct timeval *tv);
static bool must_be_discarded (struct dgram *dg);
static bool must_be_retransmitted (struct dgram *dg);
static struct dgram *list_cat (struct dgram *fst, struct dgram *snd);
static struct dgram *list_remove_if (bool (*test)(struct dgram *),
                                     struct dgram **lst);
static int min_timeout (struct dgram *lst);


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

	while (!is_done ()) {
		int nready;
		int next_tmout;
		struct timeval now;
		struct dgram *dg;

		/* Azzera eventi */
		for (i = 0; i < fds_used; i++) {
			fds[i].events = 0;
			fds[i].revents = 0;
		}

		/*
		 * Gestione dei timeout
		 */

		/* Trasferimento in data_out dei pacchetti non confermati da
		 * TED. */
		data_out = list_cat (list_remove_if (must_be_retransmitted,
		                                     &data_unakd),
		                     data_out);

		/* Eliminazione datagram vecchi. */
		data_discarded = list_cat (list_remove_if (must_be_discarded,
		                                           &data_out),
		                           data_discarded);

		next_tmout = ONE_MILLION;
		next_tmout = MIN (next_tmout, min_timeout (data_out));
		next_tmout = MIN (next_tmout, min_timeout (data_unakd));


		/*
		 * Impostazione eventi attesi.
		 */

		/* se data_in != NULL
		 * 	fds[SP_I].events |= POLLOUT; */

		/* se c'è almeno un'interfaccia attiva && data_out != NULL
		 * 	fds[CURRENT_IFACE_I] |= POLLOUT; */

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


/****************************************************************************
			       Funzioni locali
****************************************************************************/

static bool
must_be_discarded (struct dgram *dg)
{
	return FALSE;
}


static bool
must_be_retransmitted (struct dgram *dg)
{
	return FALSE;
}


static int
min_timeout (struct dgram *lst)
{
	struct dgram *min;
	struct dgram *dg;

	assert (lst != NULL);

	for (dg = lst, min = lst; dg != NULL; dg = dg->dg_next)
		/* TODO */;

	return -1;
}


static struct dgram *
list_cat (struct dgram *fst, struct dgram *snd)
{
	struct dgram *tail;

	assert (fst != NULL);
	assert (snd != NULL);

	for (tail = fst; tail->dg_next != NULL; tail = tail->dg_next);
	tail->dg_next = snd;

	return fst;
}


static struct dgram *
list_remove_if (bool (*test)(struct dgram *), struct dgram **lst)
{
	struct dgram *cur;
	struct dgram *rmvd = NULL;
	struct dgram **rmvd_tp = &rmvd;
	struct dgram *passd = NULL;
	struct dgram **passd_tp = &passd;

	assert (lst != NULL);
	assert (*lst != NULL);

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
gettime (struct timeval *tv)
{
	gettimeofday (tv, NULL);
	while (tv->tv_usec > ONE_MILLION) {
		tv->tv_sec++;
		tv->tv_usec -= ONE_MILLION;
	}
}


static void
collect_garbage (void)
{
	;
}


static fd_t
socket_bound (const char *bind_ip, const char *bind_port)
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
