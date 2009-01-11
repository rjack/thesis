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
	/* ACK or NAK firmware. */
	iface_fw_type_t fs_fw_type;
	/* Datagrams sent. */
	list_t fs_sent;
	/* List of log entries. */
	list_t fs_log;
};


struct full_path {
	/* Probe sequence number. */
	probe_seqnum_t fp_probe_seq;
	/* When expires, send probalive. */
	timeout_t fs_probe_tmout;
	/* List of log entries. */
	list_t fp_log;
};


#define     IFACE_ID_NAME_LEN         10
#define     IFACE_ID_LOC_IP_LEN       16
#define     IFACE_ID_LOC_PORT_LEN      6

struct iface {
	/* Socket file descriptor. */
	fd_t if_sfd;
	/* Name (e.g. eth0). */
	char if_name[IFACE_ID_NAME_LEN];
	/* Local IP address and local port (socket will bind to this pair). */
	char if_loc_ip[IFACE_ID_LOC_IP_LEN];
	char if_loc_port[IFACE_ID_LOC_PORT_LEN];
	/* Auxiliary stuff. */
	struct first_step if_fstep;
	struct full_path if_fpath;
};


/*******************************************************************************
			      Exported functions
*******************************************************************************/
