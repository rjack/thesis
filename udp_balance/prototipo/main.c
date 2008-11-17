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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>


/****************************************************************************
			Definizioni di tipo e costanti
****************************************************************************/

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
 * Canali.
 */

/* Indici canali importanti */
#define     SP_I                0         /* softphone */
#define     IM_I                1         /* interface monitor */
#define     CURRENT_IFACE_I     2


/* XXX C'e' modo di scoprire a runtime la dimensione di allocazione per il
 * XXX msg_control delle struct msghdr e poterlo cosi' allocare dinamicamente
 * XXX senza rischiare un MSG_TRUNC quando si fa una recvmsg con flag MSG_ERRQUEUE?
 * XXX Nell'attesa di scoprirlo uso la costante trovata nel sorgente di
 * XXX traceroute <http://traceroute.sf.net/> */
#define     CONTROLBUFLEN     1024


struct chan {
	/* socket file descriptor */
	fd_t ch_sfd;

	/* indirizzi locale e remoto */
	struct sockaddr_in ch_bind_addr;
	struct sockaddr_in ch_remote_addr;

	/* buffer per campo msg_control di struct msghdr */
	char ch_controlbuf[CONTROLBUFLEN];
};

#define     SP_BIND_IP       "127.0.0.1"
#define     SP_BIND_PORT     "7777"

#define     IM_BIND_IP       "127.0.0.1"
#define     IM_BIND_PORT     "8888"


/****************************************************************************
			       Variabili locali
****************************************************************************/

static const char *program_name = NULL;

struct chan **chans;
size_t chans_num = CURRENT_IFACE_I;


/****************************************************************************
			  Prototipi funzioni locali
****************************************************************************/

static void *my_alloc (size_t nbytes);
static void print_usage (void);
static bool is_done (void);
static void collect_garbage (void);
static fd_t socket_bound (const char *bind_ip, const char *bind_port,
                          struct sockaddr_in *bound_addr);


/****************************************************************************
			      Funzioni esportate
****************************************************************************/

int
main (const int argc, const char *argv[])
{
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

	/*
	 * Allocazione canali per:
	 * - softphone
	 * - interface monitor
	 */
	assert (chans_num == 2);
	chans = my_alloc (chans_num * sizeof(struct chan *));

	chans[SP_I] = my_alloc (sizeof(struct chan));
	chans[IM_I] = my_alloc (sizeof(struct chan));

	chans[SP_I]->ch_sfd = socket_bound (SP_BIND_IP, SP_BIND_PORT,
					    &chans[SP_I]->ch_bind_addr);
	if (chans[SP_I]->ch_sfd == -1)
		goto socket_bound_err;


	while (!is_done ()) {
		break;
	}

	return 0;

socket_bound_err:
	return 1;
}


/****************************************************************************
			       Funzioni locali
****************************************************************************/

static void
collect_garbage (void)
{
	;
}


static fd_t
socket_bound (const char *bind_ip, const char *bind_port,
              struct sockaddr_in *bound_addr)
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

	/* Copia struct sockaddr ritornata */
	assert (res->ai_addrlen == sizeof(struct sockaddr_in));
	memcpy (bound_addr, res->ai_addr, res->ai_addrlen);
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
