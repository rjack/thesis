/*
 * udp_balancer: prototipo.
 */

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


/****************************************************************************
			     Definizioni di tipo
****************************************************************************/

typedef int bool;

#ifdef FALSE
#undef FALSE
#endif
#define FALSE ((bool)0)

#ifdef TRUE
#undef TRUE
#endif
#define TRUE (!FALSE)


/****************************************************************************
			       Variabili locali
****************************************************************************/

static const char *program_name = NULL;


/****************************************************************************
			  Prototipi funzioni locali
****************************************************************************/

static void print_usage (void);
static bool done (void);


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
	 * Dipendenze eventi (SP = softphone, PX = proxy):
	 *
	 * leggo da SP -> dati outward
	 * leggo da PX -> dati inward
	 * leggo da TED -> dati netconf
	 * timeout keepalive -> outward
	 * dati inward -> scrivo a SP
	 * dati outward -> scrivo a PX
	 * dati netconf -> chiudo / apro socket
	 * timeout 150ms -> tolti pacchetti vecchi da outward
	 */

	if (argc != 1) {
		print_usage ();
		exit (EXIT_FAILURE);
	}

	while (!done ()) {
	}

	return 0;
}


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
done (void)
{
	return FALSE;
}
