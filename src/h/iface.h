#ifndef ULB_IFACE_H
#define ULB_IFACE_H

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


void
iface_destroy (iface_t handle);


bool
iface_is_bad (iface_t handle);


fd_t
iface_get_sockfd (iface_t handle);


void
iface_set_bad (iface_t handle);


bool
iface_must_send_probalive (iface_t handle);


dgram_t *
iface_get_outgoing_dgram (iface_t handle);


void
iface_set_outgoing_dgram (iface_t handle, dgram_t *dg);


void
iface_set_outgoing_probalive (iface_t handle);


dgram_t *
iface_read (iface_t handle);
/*
 * Read one datagram from the interface socket.
 * Return
 * 	the received datagram
 * or	NULL if errors occurred.
 */


dgram_t *
iface_write (iface_t handle);
/*
 * Send the previously setted outgoing datagram over the interface socket.
 * Return
 * 	the sent datagram.
 * In case of error errno is setted appropriately, else errno is zero.
 */

#endif /* ULB_IFACE_H */
