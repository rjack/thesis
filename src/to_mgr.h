#ifndef ULB_TO_MGR_H
#define ULB_TO_MGR_H

#include <sys/time.h>


/*******************************************************************************
				   Typedefs
*******************************************************************************/

/*
 * Timeout handle.
 */
typedef int timeout_t;


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

timeout_t
tmout_create (const struct timeval *value);
/*
 * Create a new timeout.
 * Return
 * 	its descriptor,
 * or	-1 in case of error.
 */


void
tmout_destroy (timeout_t handle);
/*
 * Destroy the given timeout.
 */


void
tmout_set (timeout_t handle, const struct timeval *max);
/*
 * Set max as the given timeout value.
 */


void
tmout_start (timeout_t handle, const struct timeval *now);
/*
 * Start ticking the given timeout, using now as reference.
 */


bool
tmout_left (timeout_t handle, const struct timeval *now,
            struct timeval *result);
/*
 * If result is not NULL, fill it with the time left before the given timeout
 * expires, using now as reference.
 * NOTE: result value for expired timeouts can be zero or negative.
 * Returns
 * 	TRUE if timeout is not expired yet
 * or	FALSE if timeout is expired.
 */


void
tmout_print (timeout_t handle);
/*
 * Print a representation of the given timeout to standard output.
 */


void
tm_mgr_garbage_collect (void);
/*
 * Free unused module memory.
 */


void
tm_min_left_overall (struct timeval *min_result, const struct timeval *now);
/*
 * Update all timeouts and fill min_result with the shortest remaining time.
 * NOTE: if some timeouts have expired, min_result value will be zero or
 *       negative.
 */

#endif /* ULB_TO_MGR_H */
