#include <stdlib.h>

#include "h/list.h"
#include "h/logger.h"
#include "h/sim.h"
#include "h/to_mgr.h"
#include "h/types.h"
#include "h/util.h"


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
	in_ = list_create (dgram_destroy);
	out_ = list_create (dgram_destroy);
}


static bool
is_done (void)
{
	return FALSE;
}


static int
main_loop (void)
{
	int ev;
	int nready;
	list_t rmvd;
	iface_t iface;
	int poll_timeout;
	struct timeval min;

	/*
	 * Update timeouts.
	 */
	if (tm_min_left_overall (&min, &now) == 0)
		poll_timeout = -1;
	else
		poll_timeout = (int)(tv2d (&min, FALSE) * 1000);


	/*
	 * Let things happen.
	 */
	sim_exec_step ();


	/*
	 * Discard old datagrams.
	 */
	rmvd = list_remove_if (out_, (f_bool_t)dgram_must_be_discarded, NULL);
	list_destroy (rmvd);


	/*
	 * Ifaces timeouts.
	 */
	for (iface = iface_iterator_first ();
	     iface != IFACE_ERROR;
	     iface = iface_iterator_next ())
		iface_handle_timeouts (iface);


	/*
	 * Choose best interface.
	 */
	iface_compute_best_overall ();


	/*
	 * Poll.
	 */
	pm_fd_zero ();

	softphone_set_events (!list_is_empty (in_))
	ifmon_set_events ();
	for (iface = iface_iterator_first ();
	     iface != IFACE_ERROR;
	     iface = iface_iterator_next ())
		iface_set_events (iface, !list_is_empty (out_))

	nready = pm_poll (poll_timeout);
	if (nready == -1) {
		perror ("pm_poll error");
		return -1;
	}

	if (nready == 0)
		return 0;


	/*
	 * Softphone events.
	 */
	ev = sphone_get_revents();

	if (ev & POLLIN) {
		dgram_t *dgram = sphone_read ();
		if (!dgram)
			goto softphone_err;
		dgram_set_life_timeout (dgram);
		inorder_insert (out_, dgram);
	}

	if (ev & POLLOUT) {
		dgram_t *dgram = list_dequeue (in_);
		err = sphone_write (dgram);
		if (err)
			goto softphone_err;
		dgram_destroy (dgram);
	}

	if (ev & POLLERR) {
softphone_err:
		perror ("Softphone communication error");
		return -1;
	}


	/*
	 * Interface monitor events.
	 */
	ev = ifmon_get_revents ();
	assert (!(ev & POLLOUT));

	if (ev & POLLIN) {
		char *name, *cmd, *ip;
		dgram_t *dgram = ifmon_read ();
		if (!dgram)
			goto interface_monitor_err;
		ifmon_parse (dgram_payload (dgram),
		             dgram_payload_len (dgram),
		             &name, &cmd, &ip);
		switch (cmd) {
		case IFMON_CMD_UP:
			iface_up (name, ip);
			break;
		case IFMON_CMD_DOWN:
			iface = iface_find (name);
			if (iface != IFACE_ERROR)
				iface_down (iface);
			break;
		default:
			log_err ("%s is not a valid ifmon command!", cmd);
		}
		dgram_destroy (dgram);
		free (name);
		free (cmd);
		free (ip);
	}

	if (ev & POLLERR) {
interface_monitor_err:
		perror ("Interface monitor communication error");
		return -1;
	}


	/*
	 * Gestione eventi interfacce.
	 */
	for (iface = iface_iterator_first ();
	     iface != IFACE_ERROR;
	     iface = iface_iterator_next ()) {
		ev = iface_get_revents (iface);

		if (ev & POLLIN) {
			dgram_t *dgram = iface_read (iface);
			if (dgram)
				inorder_insert (in_, dgram);
			else
				perror ("Error reading from interface");
		}
	}
	// 	se iface POLLIN
	// 		iface read dgram
	// 		if !err && dgram
	//	 		dgram enqueue coda in
	// 	se iface POLLOUT
	// 		assert !empty out
	// 		dgram = dequeue out
	// 		iface_write iface dgram
	// 		if err
	// 			iface_down
	// 			push out dgram
	// 			continue
	// 	se iface POLLERR
	// 		iface get err
	// 		se errore fatale
	// 			iface_down
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

	return 0;
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

	/*
	 * Main loop.
	 */
	do {
		err = main_loop ();
	} while (!err && !is_done ());
	if (err)
		goto fatal_err;

	return EXIT_SUCCESS;

fatal_err:
	return EXIT_FAILURE;
}
