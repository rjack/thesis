#include <assert.h>
#include <string.h>
#include <unistd.h>

#include "h/dtable_mgr.h"
#include "h/if_mgr.h"
#include "h/list.h"
#include "h/poll_mgr.h"


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
	struct timeval fsl_timestamp;
	bool fsl_success;
};


/*
 * Interface.
 */

struct first_step {
	/* Datagrams sent. */
	list_t fs_sent;
	/* List of log entries. */
	list_t fs_log;
};


struct full_path {
	/* Probe sequence number. */
	probe_seqnum_t fp_probe_seq;
	/* When expires, send probalive. */
	timeout_t fp_probe_tmout;
	/* List of log entries. */
	list_t fp_log;
};


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
	/* Auxiliary stuff. */
	struct first_step if_fstep;
	struct full_path if_fpath;
};


/*******************************************************************************
			       Local variables
*******************************************************************************/

static struct iface *table_;
static size_t table_len_;
static size_t table_used_;


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

	list_destroy (iface->if_fstep.fs_sent);
	list_destroy (iface->if_fstep.fs_log);

	iface->if_fpath.fp_probe_seq = 0;
	tmout_destroy (iface->if_fpath.fp_probe_tmout);
	list_destroy (iface->if_fpath.fp_log);
}


static bool
is_valid_handle (iface_t handle)
{
	return dtable_is_valid_handle ((void **)&table_, table_used_, handle,
	                               (use_checker_t)is_used);
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
	assert (is_valid_handle (handle));

	dtable_remove ((void **)&table_, &table_used_, handle,
	               (unused_setter_t)set_unused);
}


void
iface_compute_best_overall (void)
{
	/* TODO */
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
	list_t rmvd;
	dgram_t *acked;
	struct iface *iface;

	iface = &(table_[handle]);

	acked = dgram_list_remove (iface->if_fstep.fs_sent, id);
}


dgram_t *
iface_get_nacked (iface_t handle, dgram_id_t id)
{
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

	/* Probalive timeout expired. */
	if (!tmout_left (iface->if_fpath.fp_probe_tmout, NULL))
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

	/* Probalive timeout expired. */
	if (!tmout_left (iface->if_fpath.fp_probe_tmout, NULL))
		ev |= POLLMSG;

	return ev;
}


int
iface_get_ip_notice (iface_t handle, int *id)
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
