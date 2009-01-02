/*******************************************************************************
				 Local types
*******************************************************************************/

struct log_entry {
	struct timeval le_timestamp;
	bool le_success;
};


/*
 * Interface.
 */

#define     IFACE_ID_NAME_LEN         10
#define     IFACE_ID_LOC_IP_LEN       16
#define     IFACE_ID_LOC_PORT_LEN      6

struct iface {
	/* If TRUE, don't use this interface. */
	bool if_bad;
	/* Name (e.g. eth0). */
	char if_name[IFACE_ID_NAME_LEN];
	/* Local IP address and local port (socket will bind to this pair). */
	char if_loc_ip[IFACE_ID_LOC_IP_LEN];
	char if_loc_port[IFACE_ID_LOC_PORT_LEN];
	/* Socket file descriptor. */
	fd_t if_sfd;
	/* ACK or NAK firmware. */
	iface_fw_type_t if_fw_type;
	/* List of log entries. */
	list_t if_log;
	/* When expires, send probalive. */
	timeout_t if_probalive_tmout;
	/* Datagram to be sent. */
	dgram_t *if_outgoing_dgram;
};


/*******************************************************************************
			      Exported functions
*******************************************************************************/
