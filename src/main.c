#include <stdlib.h>

#include "h/list.h"
#include "h/types.h"

/*******************************************************************************
			       Local variables
*******************************************************************************/

/* Dgram queues. Ordered by timestamp. */
static list_t in_;
static list_t out_;


/*******************************************************************************
			       Local functions
*******************************************************************************/

static void
get_cmd_line_options (void)
{
	/*
	 * Tipo conversazione da simulare
	 * --conversation-listen
	 * --conversation-talk
	 * --conversation-mixed
	 */

	/*
	 * Tipo di movimento da simulare
	 * --movement-slow
	 * --movement-fast
	 * --movement-mixed
	 */

	/*
	 * Interfacce.
	 *
	 * --iface NAME-{ack|nak}
	 * es: --iface iwlan0-ack --iface eth1-nak
	 */

	/*
	 * Reti.
	 *
	 * --net ESSID,WIFI_MIN_ERR,WIRE_ERR,WIRE_DELAY
	 */
}


static void
init_data_struct (void)
{
	/* init code datagram: in out */
}


static bool
is_done (void)
{
	return FALSE;
}


static int
main_loop (void)
{
	/*
	 * Aggiornamento timeout:
	 * lettura timeout minimo tra tutti quelli attivi.
	 */
	// gettime (&now);
	// nexp = tm_min_left_overall (&min, &now);
	// if (nexp == 0)
	// 	poll_timeout = -1;
	// else
	// 	poll_timeout = (int)(tv2d (&min, FALSE) * 1000);


	/*
	 * Controllo timeout scaduti dgram in uscita.
	 */
	// per ogni dgram dentro a out
	// 	se scaduto life timeout
	// 		remove from list
	// 		discard

	/*
	 * Controllo timeout scaduti interfacce.
	 */
	// per ogni interfaccia iface
	// 	iface iface_handle_timeouts


	/*
	 * Valutazione interfaccia migliore.
	 */
	// iface_compute_best_overall ();


	/*
	 * Poll.
	 */
	// pm_fd_zero ();
	//
	// softphone_set_events (!list_is_empty (in))
	// ifmon_set_events ();
	// per ogni interfaccia iface
	// 	iface_set_events (iface, !list_is_empty (out))
	//
	// pm_poll (poll_timeout);


	/*
	 * Gestione eventi softphone.
	 */
	// rev = softphone_get_revents()
	// if rev & POLLIN
	// 	dgram = softphone_read ()
	// 	se !err
	// 		set life timeout dgram
	// 		inorder insert dgram coda out
	// if rev & POLLOUT
	// 	dequeue dgram da coda in
	// 	write dgram socket softphone
	// 	se !err
	// 		discard dgram
	// 	altrimenti
	// 		push dgram coda in
	// if rev & POLLERR
	// 	exit failure


	/*
	 * Gestione eventi interface monitor.
	 */
	// rev = ifmon_get_revents ()
	// if rev & POLLIN
	// 	read dgram
	// 	if !err
	// 		ifmon_parse (name, cmd, ip);
	// 		switch (cmd) {
	// 		case IFMON_CMD_UP:
	// 			iface_up (name, ip);
	// 			break;
	// 		case IFMON_CMD_DOWN:
	// 			iface_down (iface_find (name));
	// 			break;
	// 		default:
	// 			error command not recognized;
	// 		}
	// if rev & POLLERR
	// 	exit failure


	/*
	 * Gestione eventi interfacce.
	 */
	// per ogni interfaccia iface
	// 	se iface POLLIN
	// 		iface read dgram
	// 		if !err && dgram
	//	 		dgram enqueue coda in
	// 	se iface POLLOUT
	// 		assert !empty out
	// 		dgram = dequeue out
	// 		iface_write iface dgram
	// 		if err
	// 			destroy iface
	// 			push out dgram
	// 			continue
	// 	se iface POLLERR
	// 		iface get err
	// 		se errore fatale
	// 			iface destroy
	// 			continue
	// 		altrimenti se ack id
	// 			remove if ha lo stesso id da coda out
	//			if dgram
	//				discard il dgram rimosso
	//			iface set ack id
	//		altrimenti e' un nak
	//			dgram = iface set nak id
	//			if dgram
	//				inorder insert dgram out

	return EXIT_FAILURE;
}


int
main (int argc, const char *argv[])
{
	int err;

	err = get_cmd_line_options ();
	if (err)
		goto fatal_err;

	err = init_data_struct ();
	if (err)
		goto fatal_err;

	/* Modules. */
	err = iface_init ();
	if (err)
		goto fatal_err;

	err = ifmon_init ();
	if (err)
		goto fatal_err;

	err = sim_init ();
	if (err)
		goto fatal_err;

	err = sphone_init ();
	if (err)
		goto fatal_err;

	do {
		err = main_loop ();
	} while (!err && !is_done ());
	if (err)
		goto fatal_err;

	return EXIT_SUCCESS;

fatal_err:
	// TODO err_print
	return EXIT_FAILURE;
}
