#include <assert.h>

#include "h/dtable_mgr.h"
#include "h/list.h"
#include "h/if_mgr.h"


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
static size_t iface table_used_;


/*******************************************************************************
			       Local functions
*******************************************************************************/

static bool
is_used (struct iface *table, int index)
{
	if (table[i].if_sfd == -1)
		return TRUE;
	return FALSE;
}


static void
set_unused (struct iface *table, int index)
{
	struct iface *iface;

	iface = &(table[index]);

	close (iface->if_sfd);
	iface->if_sfd = -1;
	memset (iface->if_name, '\0', IFACE_NAME_LEN);
	memset (iface->if_loc_ip, '\0', IFACE_LOC_IP_LEN);
	memset (iface->if_loc_port, '\0', IFACE_LOC_PORT_LEN);

	list_destroy (iface->if_fstep.fs_sent);
	list_destroy (iface->if_fstep.fs_log);

	/* XXX che si fa con il fp_probe_seq? Si resetta? */
	tmout_destroy (iface->if_fpath.fp_probe_tmout);
	list_destroy (iface->if_fpath.fp_log);
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
}


iface_t
iface_up (const char *name, const char *ip, const char *port)
{
	iface_t handle;

	assert (name != NULL);
	assert (ip != NULL);
	assert (port != NULL);

	handle = dtable_add ((void **)&table_, &table_len_, &table_used_,
	                     sizeof(*table_), is_used);
	if (handle == -1)
		return IFACE_ERROR;

	/* TODO */
}



void
iface_down (iface_t handle)
{
	assert (is_valid_handle (handle));

	dtable_remove ((void **), &table_used_, handle,
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
	/* TODO */
}


iface_t
iface_iterator_first (void)
{
	/* TODO */
}


iface_t
iface_iterator_next (iface_t handle)
{
	/* TODO */
}


fd_t
iface_get_sockfd (iface_t handle)
{
	/* TODO */
}


int
iface_handle_timeouts (iface_t handle)
{
	/* TODO */
}


void
iface_set_events (iface_t iface, bool something_to_send)
{
	/* TODO */
}


int
iface_get_revents (iface_t iface)
{
	/* TODO */
}


int
iface_get_ip_notice (iface_t iface, int *id)
{
	/* TODO */
}


dgram_t *
iface_get_dgram (iface_t iface, dgram_id_t dgram_id)
{
	/* TODO */
}


dgram_t *
iface_read (iface_t handle)
{
	/* TODO */
}


int
iface_write (iface_t handle, dgram_t *dgram)
{
	/* TODO */
}
