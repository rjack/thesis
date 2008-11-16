/*
 * udp_balancer: prototipo.
 */

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
	int ch_sfd;

	/* indirizzi locale e remoto */
	struct sockaddr_in ch_bind_addr;
	struct sockaddr_in ch_remote_addr;

	/* buffer per campo msg_control di struct msghdr */
	char ch_controlbuf[CONTROLBUFLEN];
};


/****************************************************************************
			       Variabili locali
****************************************************************************/

static const char *program_name = NULL;

struct chan **channels;
size_t channels_num = CURRENT_IFACE_I;


/****************************************************************************
			  Prototipi funzioni locali
****************************************************************************/

static void *my_alloc (size_t nbytes);
static void print_usage (void);
static bool is_done (void);
static void collect_garbage (void);
static struct chan *new_chan (const char *ip_bind, const char *port_bind,
		              const char *ip_peer, const char *port_peer);


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
	assert (channels_num == 2);
	channels = (struct chan **)my_alloc (channels_num * sizeof(struct chan *));

	/* TODO: argomenti specificati da riga di comando */
	channels[SP_I] = new_chan ("127.0.0.1", "7777", "127.0.0.1", "8888");
	channels[IM_I] = new_chan ("127.0.0.1", "5656", NULL, NULL);

	while (!is_done ()) {
		break;
	}

	return 0;
}


/****************************************************************************
			       Funzioni locali
****************************************************************************/

static void
collect_garbage (void)
{
	;
}


static struct chan *
new_chan (const char *ip_bind, const char *port_bind,
          const char *ip_peer, const char *port_peer)
{
	struct chan *new_chan;

	/*
	 * Allocazione e inizializzazione.
	 */
	new_chan = my_alloc (sizeof(struct chan));
	memset (new_chan, 0, sizeof(struct chan));

	/*
	 * TODO loop getaddrinfo
	 */

	/*
	 * Socket UDP.
	 */
	new_chan->ch_sfd = socket (AF_INET, SOCK_DGRAM, 0);
	if (new_chan->ch_sfd == -1)
		goto socket_err;

	return new_chan;

socket_err:
	return NULL;
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
