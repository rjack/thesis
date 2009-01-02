#ifndef ULB_IFACE_H
#define ULB_IFACE_H

#include "to_mgr.h"
#include "types.h"


/*******************************************************************************
				   Typedefs
*******************************************************************************/

/*
 * Interface log.
 */
typedef struct {
	// TODO
} iface_log_t;


/*
 * Interface.
 */

#define     IFACE_ID_NAME_LEN         10
#define     IFACE_ID_LOC_IP_LEN       16
#define     IFACE_ID_LOC_PORT_LEN      6

typedef     char                   iface_fw_type_t;
#define     FIRMWARE_ACK     ((iface_fw_type_t)'a')
#define     FIRMWARE_NAK     ((iface_fw_type_t)'n')

typedef struct {
	iface_fw_type_t if_fw_type;
	fd_t if_sfd;
	char if_name[IFACE_ID_NAME_LEN];
	char if_loc_ip[IFACE_ID_LOC_IP_LEN];
	char if_loc_port[IFACE_ID_LOC_PORT_LEN];
	timeout_t if_probalive_tmout;
	iface_log_t if_log;
} iface_t;


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

#endif /* ULB_IFACE_H */
