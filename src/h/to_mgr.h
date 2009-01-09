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

#define     TIMEOUT_ERROR     ((timeout_t)-1)


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

timeout_t
tmout_create (const struct timeval *value);
/*
 * Create a new timeout.
 * Return
 * 	its descriptor,
 * or	TIMEOUT_ERROR in case of error.
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
tmout_start (timeout_t handle);
/*
 * Start ticking the given timeout.
 */


bool
tmout_left (timeout_t handle struct timeval *result);
/*
 * If result is not NULL, fill it with the time left before the given timeout
 * expires.
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


int
tm_min_left_overall (struct timeval *min_result);
/*
 * Update all timeouts and fill min_result with the shortest remaining time.
 * NOTE: if some timeouts have expired, min_result value will be zero or
 *       negative.
 * Returns
 * 	the number of the timeouts that have expired: if zero, min_result
 * 	value is unspecified.
 */

#endif /* ULB_TO_MGR_H */
