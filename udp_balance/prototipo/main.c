/*
 * udp_balancer: prototipo.
 */

#define     _POSIX_C_SOURCE     1     /* per getaddrinfo */


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

static timeout_t keepalive;
static bool must_send_keepalive = FALSE;

/* Timeval di comodo. */
static const struct timeval time_0ms = { 0, 0 };
static const struct timeval time_30ms = { 0, 30000 };
static const struct timeval time_150ms = { 0, 150000 };

static struct pollfd fds[2 + IFACE_MAX];
static size_t fds_used = 0;

static pollfd *sp = &fds[0];
static pollfd *im = &fds[1];


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

	while (!is_done ()) {
		int nready;
		int next_tmout;
		struct timeval min;
		struct timeval left;
		iface_t *current_iface;

		iface_foreach_do (iface_set_events,
		                  arg_create (0, sizeof(int)));

		current_iface = iface_get_current ();
		if (debug) {
			char ifstr[100];
			iface_to_string (current_iface, ifstr);
			printf ("Using interface %s\n", ifstr);
		}

		/*
		 * Gestione dei timeout: pulizia code e calcolo timeout minimo.
		 */
		gettime (&now);

		dgram_outward_all_unaked ();
		dgram_purge_all_old ();

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
		if (data_in != NULL)
			fds[SP_I].events |= POLLOUT;

		/* Se ho un'interfaccia wifi attiva e dati dal softphone,
		 * scrivo al server. */
		if (data_out != NULL)
			iface

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
