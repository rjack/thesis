#ifndef ULB_SPHONE_H
#define ULB_SPHONE_H

#include "types.h"


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

int
sphone_init (void);
/*
 * Initialize softphone module.
 */


int
sphone_get_revents (void);
/*
 * Get events returned by poll.
 */


void
sphone_set_events (bool something_to_write);
/*
 * Tell the softphone if there's something to write, so it can set expected
 * poll events appropriately.
 */


dgram_t *
sphone_read (void);
/*
 * Read a datagram from the softphone.
 * Return
 * 	the pointer to the read datagram,
 * or	NULL if errors occurred.
 */


int
sphone_write (dgram_t *dgram);
/*
 * Write the given dgram to the softphone.
 *
 * Return
 * 	 0
 * or	-1 if errors occurred.
 */


#endif /* ULB_SPHONE_H */
