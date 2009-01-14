#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "h/dgram.h"
#include "h/if_mgr.h"
#include "h/ifmon.h"
#include "h/list.h"
#include "h/logger.h"
#include "h/poll_mgr.h"
#include "h/sim.h"
#include "h/sphone.h"
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

static int
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
	return 0;
}


static int
init_data_struct (void)
{
	in_ = list_create ((f_destroy_t)dgram_destroy);
	out_ = list_create ((f_destroy_t)dgram_destroy);

	if (in_ != LIST_ERR && out_ != LIST_ERR)
		return 0;
	return -1;
}


static void
inorder_insert (list_t data, dgram_t *dg)
{
	/* TODO */
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
	 * Get min timeout for poll.
	 */
	if (tm_min_left_overall (&min) == 0)
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
	rmvd = list_remove_if (out_, (f_bool_t)dgram_life_expired, NULL);
	list_destroy (rmvd);


	/*
	 * Ifaces timeouts.
	 */
	for (iface = iface_iterator_first ();
	     iface != IFACE_ERROR;
	     iface = iface_iterator_next (iface)) {
		list_t rmvd;
		dgram_t *dgram;
		/* Discard old. */
		iface_dgrams_discard (dgram_life_expired);
		/* Send again if retry timeout is expired. */
		rmvd = iface_dgrams_remove (dgram_retry_expired);
		while (dgram = list_dequeue (rmvd))
			inorder_insert (out_, dgram);
		list_destroy (rmvd);
	}


	/*
	 * Choose best interface.
	 */
	iface_compute_best_overall ();


	/*
	 * Poll.
	 */
	pm_fd_zero ();

	sphone_set_events (!list_is_empty (in_));
	ifmon_set_events ();
	for (iface = iface_iterator_first ();
	     iface != IFACE_ERROR;
	     iface = iface_iterator_next (iface))
		iface_set_events (iface, !list_is_empty (out_));

	nready = pm_poll (poll_timeout);
	if (nready == -1) {
		log_err ("pm_poll error: %s", strerror (errno));
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
		int err = sphone_write (dgram);
		if (err)
			goto softphone_err;
		dgram_destroy (dgram);
	}

	if (ev & POLLERR) {
softphone_err:
		log_err ("Softphone communication error: %s",
		         strerror (errno));
		return -1;
	}


	/*
	 * Interface monitor events.
	 */
	ev = ifmon_get_revents ();
	assert (!(ev & POLLOUT));

	if (ev & POLLIN) {
		int cmd;
		char *name, *ip;
		dgram_t *dgram = ifmon_read ();
		if (!dgram)
			goto interface_monitor_err;
		cmd = ifmon_parse (dgram_payload (dgram),
		                   dgram_payload_len (dgram),
		                   &name, &ip);
		switch (cmd) {
		case IFMON_CMD_UP:
			iface_up (name, ip, IFACE_LOCAL_PORT);
			break;
		case IFMON_CMD_DOWN:
			iface = iface_find (name);
			if (iface != IFACE_ERROR)
				iface_down (iface);
			break;
		default:
			log_err ("%d is not a valid ifmon command!", cmd);
		}
		dgram_destroy (dgram);
		free (name);
		free (ip);
	}

	if (ev & POLLERR) {
interface_monitor_err:
		log_err ("Interface monitor communication error: %s",
		         strerror (errno));
		return -1;
	}


	/*
	 * Gestione eventi interfacce.
	 */
	for (iface = iface_iterator_first ();
	     iface != IFACE_ERROR;
	     iface = iface_iterator_next (iface)) {
		ev = iface_get_revents (iface);

		if (ev & POLLIN) {
			dgram_t *dgram = iface_read (iface);
			if (dgram)
				inorder_insert (in_, dgram);
			else {
				log_err ("Error reading from interface: %s",
				         strerror (errno));
				iface_down (iface);
			}
		}

		if (ev & POLLOUT) {
			dgram_t *dgram = list_dequeue (out_);
			int err = iface_write (iface, dgram);
			if (err) {
				log_err ("Error writing on interface: %s",
				         strerror (errno));
				iface_down (iface);
				inorder_insert (out_, dgram);
				continue;
			}
		}

		if (ev & POLLMSG) {
			int err = iface_write_extra ();
			if (err) {
				log_err ("Error writing on interface: %s",
				         strerror (errno));
				iface_down (iface);
				continue;
			}
		}

		if (ev & POLLERR) {
			dgram_id_t id;
			int notice = iface_get_ip_notice (iface, &id);

			switch (notice) {
				dgram_t *dgram;
			case IFACE_NOTICE_ACK :
				dgram = iface_get_acked (iface, id);
				if (dgram)
					dgram_destroy (dgram);
				break;
			case IFACE_NOTICE_NAK :
				dgram = iface_get_nacked (iface, id);
				if (dgram)
					inorder_insert (out_, dgram);
				break;
			case IFACE_NOTICE_ERROR :
				iface_down (iface);
				break;
			default :
				log_err ("Unknown notice: %d", notice);
				return -1;
			}
		}
	}

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
	err = im_init ();
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
