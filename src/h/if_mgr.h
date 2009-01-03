#ifndef ULB_IF_MGR_H
#define ULB_IF_MGR_H

#include "to_mgr.h"
#include "types.h"


/*******************************************************************************
				   Typedefs
*******************************************************************************/

/*
 * Interface descriptor.
 */
typedef     int             iface_t;
#define     IFACE_ERROR     ((iface_t)-1)


/*
 * Firmware type.
 */
typedef     char             iface_fw_type_t;
#define     FIRMWARE_ACK     ((iface_fw_type_t)'a')
#define     FIRMWARE_NAK     ((iface_fw_type_t)'n')


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

iface_t
iface_create (const char *name, const char *ip, const char *port);


iface_t
iface_iterator_first (void);
/*
 * Return
 * 	the first interface handle
 * or	IFACE_ERROR if no interface is available.
 */


iface_t
iface_iterator_next (iface_t handle);
/*
 * Return
 * 	the next interface of the iteration
 * or	IFACE_ERROR if no more interfaces.
 */


void
iface_destroy (iface_t handle);


fd_t
iface_get_sockfd (iface_t handle);


int
iface_handle_timeouts (iface_t handle);
/*
 * Internal timeout-related bookeeping.
 */


dgram_t *
iface_read (iface_t handle);
/*
 * Read one datagram from the interface socket.
 * Return
 * 	the received datagram
 * or	NULL if errors occurred.
 */


int
iface_write (iface_t handle, dgram_t *dgram);
/*
 * Send the given datagram over the interface socket.
 * Return
 * 	 0 if ok
 * or	-1 if error
 */

#endif /* ULB_IF_MGR_H */
