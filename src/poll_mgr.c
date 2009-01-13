#include <errno.h>
#include <poll.h>
#include <sys/types.h>

#include "h/types.h"


/****************************************************************************
			       Variabili locali
****************************************************************************/

#define     MAX_FDS     1024


static struct pollfd fds_[MAX_FDS];
static size_t nfds_ = 0;


/****************************************************************************
			       Funzioni locali
****************************************************************************/

static int
search_fd (int fd)
/*
 * Search fds_ for fd.
 * Return
 * 	fd index,
 * or	-1 if fd not found.
 */
{
	int i;

	for (i = 0; i < nfds_ && fds_[i].fd != fd; i++);

	if (i == nfds_)
		return -1;     /* not found */
	return i;
}


static int
add_fd (int fd)
/*
 * Add fd to fds_, returning its index.
 */
{
	fds_[nfds_].fd = fd;
	fds_[nfds_].events = 0;
	fds_[nfds_].revents = 0;

	nfds_++;
	return nfds_ - 1;
}


/****************************************************************************
			      Exported functions
****************************************************************************/

void
pm_fd_zero (void)
{
	int i;

	for (i = 0; i < nfds_; i++) {
		fds_[i].fd = -1;
		fds_[i].events = 0;
		fds_[i].revents = 0;
	}
	nfds_ = 0;
}


void
pm_fd_set (fd_t fd, int ev)
{
	int i;

	i = add_fd (fd);
	fds_[i].events = ev;
}


int
pm_fd_get_events (fd_t fd)
{
	int i;

	i = search_fd (fd);
	return fds_[i].events;
}


int
pm_fd_get_revents (fd_t fd)
{
	int i;

	i = search_fd (fd);
	return fds_[i].revents;
}


int
pm_poll (int tmout)
{
	int nready;

	do {
		nready = poll (fds_, nfds_, tmout);
	} while (nready == -1 && errno == EINTR);

	return nready;
}
