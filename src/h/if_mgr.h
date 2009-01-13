#ifndef ULB_IF_MGR_H
#define ULB_IF_MGR_H

#include "dgram.h"
#include "to_mgr.h"
#include "types.h"


/*******************************************************************************
				   Typedefs
*******************************************************************************/

/*
 * Probe sequence number.
 */
typedef int probe_seqnum_t;


/*
 * Interface descriptor.
 */
typedef     int             iface_t;
#define     IFACE_ERROR     ((iface_t)-1)


/*
 * TED notices.
 */
#define     IFACE_NOTICE_ACK       0
#define     IFACE_NOTICE_NAK       1
#define     IFACE_NOTICE_ERROR     2


/*
 * Local port.
 */
#define     IFACE_LOCAL_PORT     "5555"


/*******************************************************************************
			     Function prototypes
*******************************************************************************/


int
im_init (void);


iface_t
iface_up (const char *name, const char *ip, const char *port);


void
iface_down (iface_t handle);


void
iface_compute_best_overall (void);


iface_t
iface_find (const char *name);


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


fd_t
iface_get_sockfd (iface_t handle);


dgram_t *
iface_get_acked (iface_t iface, dgram_id_t id);


dgram_t *
iface_get_nacked (iface_t iface, dgram_id_t id);


int
iface_handle_timeouts (iface_t handle);
/*
 * Internal timeout-related bookeeping.
 */


void
iface_set_events (iface_t iface, bool something_to_send);


int
iface_get_revents (iface_t iface);


int
iface_get_ip_notice (iface_t iface, dgram_id_t *id);


dgram_t *
iface_get_dgram (iface_t iface, dgram_id_t dgram_id);


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
