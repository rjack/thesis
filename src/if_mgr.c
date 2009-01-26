#include <assert.h>
#include <string.h>
#include <unistd.h>

#include "h/crono.h"
#include "h/dgram.h"
#include "h/dtable_mgr.h"
#include "h/if_mgr.h"
#include "h/list.h"
#include "h/logger.h"
#include "h/poll_mgr.h"
#include "h/util.h"

/*
 * TODO
 * per ogni interfaccia bisogna capire il tipo di firmware
 *
 * quando riceve la risposta di un probe
 * 	controlla l'id a cui si riferisce
 * 	se il probe relativo non e' stato ackato
 * 		il firmware notifica SOLO I NAK
 */


/*******************************************************************************
				 Local types
*******************************************************************************/

struct full_path_log_entry {
	probe_seqnum_t fpl_seqnum;
	struct timeval fpl_probe_sent_at;
	struct timeval fpl_probe_recv_at;
};


struct first_step_log_entry {
	dgram_id_t fsl_dgram_id;
	dgram_type_t fsl_dgram_type;
	struct timeval fsl_timestamp;
	int fsl_outcome;
};


/*
 * Interface.
 */

#define     IFACE_NAME_LEN         10
#define     IFACE_LOC_IP_LEN       16
#define     IFACE_LOC_PORT_LEN      6

struct iface {
	/* Socket file descriptor. */
	fd_t if_sfd;

	/* Name (e.g. eth0). */
	char if_name[IFACE_NAME_LEN];
	/* Local IP address and local port (socket will bind to this pair). */
	char if_loc_ip[IFACE_LOC_IP_LEN];
	char if_loc_port[IFACE_LOC_PORT_LEN];

	/* Interface quality. */
	vote_t if_vote;

	/* List of sent datagrams. */
	list_t if_sent;
	/* List of log entries, ordered by fsl_timestamp. */
	list_t if_first_step_log;
	/* List of log entries, ordered by fpl_probe_sent_at. */
	list_t if_full_path_log;

	/* Firmware infos. */
	bool if_firmware_ack;
	bool if_firmware_nak;

	/* Probe sequence number. */
	probe_seqnum_t if_probe_seq;
	/* When expires, send probalive. */
	timeout_t if_probe_tmout;

	/* TODO stats. */
};


/*******************************************************************************
			       Local variables
*******************************************************************************/

static struct iface *table_;
static size_t table_len_;
static size_t table_used_;
iface_t best_;


/*******************************************************************************
			       Local functions
*******************************************************************************/

static bool
is_used (struct iface *table, int i)
{
	if (table[i].if_sfd == -1)
		return TRUE;
	return FALSE;
}


static void
set_unused (struct iface *table, int i)
{
	struct iface *iface;

	iface = &(table[i]);

	close (iface->if_sfd);
	iface->if_sfd = -1;
	memset (iface->if_name, '\0', IFACE_NAME_LEN);
	memset (iface->if_loc_ip, '\0', IFACE_LOC_IP_LEN);
	memset (iface->if_loc_port, '\0', IFACE_LOC_PORT_LEN);

	iface->if_vote = VOTE_UNKNOWN;

	list_destroy (iface->if_sent);
	list_destroy (iface->if_first_step_log);
	list_destroy (iface->if_full_path_log);

	iface->if_probe_seq = 0;
	tmout_destroy (iface->if_probe_tmout);
}


static bool
is_valid_handle (iface_t handle)
{
	return dtable_is_valid_handle ((void **)&table_, table_used_, handle,
	                               (use_checker_t)is_used);
}


static int
fsl_cmp (struct first_step_log_entry *fsle_1, struct first_step_log_entry *fsle_2)
{
	return tv_cmp (&(fsle_1->fsl_timestamp), &(fsle_2->fsl_timestamp));
}


static void
fsl_add_success (list_t log, dgram_id_t id, dgram_type_t type, bool success)
{
	struct first_step_log_entry *fsle;

	fsle = my_alloc (sizeof(*fsle));

	gettime (&fsle->fsl_timestamp);
	fsle->fsl_dgram_id = id;
	fsle->fsl_dgram_type = type;
	fsle->fsl_success = success;

	list_inorder_insert (log, fsle, (f_compare_t)fsl_cmp);
}


static vote_t
compute_vote_full_path (list_t full_path_log)
{
	/* TODO */
	return -1;
}


static vote_t
compute_vote_first_step (list_t first_step_log)
{
	/* TODO */
	return -1;
}


static vote_t
compute_vote_total (struct iface *iface)
{
	vote_t full_path_vote;
	vote_t first_step_vote;

	full_path_vote = compute_vote_full_path (iface->if_full_path_log);
	first_step_vote = compute_vote_first_step (iface->if_first_step_log);

	/* TODO return formula_magica (full_path_vote, first_step_vote) */
	return -1;
}


/*******************************************************************************
			      Exported functions
*******************************************************************************/

int
im_init (void)
{
	table_ = NULL;
	table_used_ = 0;
	table_len_ = 0;
	best_ = IFACE_ERROR;

	return 0;
}


iface_t
iface_up (const char *name, const char *ip, const char *port)
{
	iface_t handle;

	assert (name != NULL);
	assert (ip != NULL);
	assert (port != NULL);

	handle = dtable_add ((void **)&table_, &table_len_, &table_used_,
	                     sizeof(*table_), (use_checker_t)is_used);
	if (handle == -1)
		return IFACE_ERROR;

	/* TODO */
	assert (FALSE);
	return IFACE_ERROR;
}



void
iface_down (iface_t handle)
{
	struct iface *iface;

	assert (is_valid_handle (handle));

	iface = &(table_[handle]);

	log ("iface_down %s %s:%s", iface->if_name, iface->if_loc_ip,
	     iface->if_loc_port);

	dtable_remove ((void **)&table_, &table_used_, handle,
	               (unused_setter_t)set_unused);
}


void
iface_compute_best_overall (void)
{
	iface_t cur;
	iface_t new_best;
	vote_t max;
	struct iface *cur_ptr;

	for (cur = iface_iterator_first ();
	     cur != IFACE_ERROR;
	     cur = iface_iterator_next (cur)) {
		cur_ptr = &(table_[cur]);
		cur->if_vote = compute_vote_total (cur_ptr);
		if (cur->if_vote > max) {
			max = cur->if_vote;
			new_best = cur;
		}
	}
	best_ = new_best;
}


iface_t
iface_find (const char *name)
{
	iface_t handle;

	for (handle = iface_iterator_first ();
	     handle != IFACE_ERROR
	     && strncmp (table_[handle].if_name, name, IFACE_NAME_LEN) != 0;
	     handle = iface_iterator_next (handle));

	return handle;
}


iface_t
iface_iterator_first (void)
{
	int i;

	for (i = 0; i < table_used_ && !is_used (table_, i); i++);
	if (i == table_used_)
		return IFACE_ERROR;
	return i;
}


iface_t
iface_iterator_next (iface_t handle)
{
	assert (handle != IFACE_ERROR);

	do {
		handle++;
	} while (handle < table_used_ && !is_used (table_, handle));

	if (handle == table_used_)
		return IFACE_ERROR;
	return handle;
}


fd_t
iface_get_sockfd (iface_t handle)
{
	assert (is_valid_handle (handle));

	return table_[handle].if_sfd;
}


dgram_t *
iface_get_acked (iface_t handle, dgram_id_t id)
{
	dgram_t *acked;
	struct iface *iface;

	iface = &(table_[handle]);

	/* TODO segnare sul log */

	acked = list_remove_one (iface->if_sent, (f_bool_t)dgram_eq_id, &id);
	return acked;
}


dgram_t *
iface_get_nacked (iface_t handle, dgram_id_t id)
{
	dgram_t *nacked;
	struct iface *iface;

	iface = &(table_[handle]);

	/* TODO segnare sul log */

	nacked = list_remove_one (iface->if_sent, (f_bool_t)dgram_eq_id, &id);
	return nacked;
}


int
iface_handle_timeouts (iface_t handle)
{
	/* TODO */
	return -1;
}


void
iface_set_events (iface_t handle, bool something_to_send)
{
	struct iface *iface;

	iface = &(table_[handle]);

	/* Probalive timeout expired or initial probe burst. */
	if (!tmout_left (iface->if_probe_tmout, NULL)
	    || iface->if_probe_seq < PROBE_BURST_MAX)
		something_to_send = TRUE;

	pm_fd_set (table_[handle].if_sfd,
		   POLLIN | POLLERR | (something_to_send ? POLLOUT : 0));
}


int
iface_get_revents (iface_t handle)
{
	int ev;
	struct iface *iface;

	iface = &(table_[handle]);

	ev = pm_fd_get_revents (iface->if_sfd);

	/* Probalive timeout expired or must perform initial probe burst. */
	if (!tmout_left (iface->if_probe_tmout, NULL)
	    || iface->if_probe_seq < PROBE_BURST_MAX)
		ev |= POLLMSG;

	return ev;
}


int
iface_get_ip_notice (iface_t handle, dgram_id_t *id)
{
	/* TODO */
	return -1;
}


dgram_t *
iface_get_dgram (iface_t handle, dgram_id_t dgram_id)
{
	/* TODO */
	return NULL;
}


dgram_t *
iface_read (iface_t handle)
{
	/* TODO */
	return NULL;
}


int
iface_write (iface_t handle, dgram_t *dgram)
{
	/* TODO */
	return -1;
}


int
iface_write_extra (iface_t handle)
{
	/* TODO */
	return -1;
}
