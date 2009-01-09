#ifndef ULB_POLL_MGR_H
#define ULB_POLL_MGR_H

#include "types.h"


/****************************************************************************
			     Function prototypes
****************************************************************************/

void
pm_fd_zero (void);
/*
 * Same as FD_ZERO.
 */


void
pm_fd_set (fd_t fd, int ev);
/*
 * Same as FD_SET, but use POLLIN, POLLOUT and POLLER as ev value.
 */


int
pm_fd_get_revents (fd_t fd);
/*
 * After pm_poll, get returned events associated with fd.
 */


int
pm_poll (int tmout);
/*
 * Poll on the setted file descriptors.
 */

#endif /* ULB_POLL_MGR_H */
