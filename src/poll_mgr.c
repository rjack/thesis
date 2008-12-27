#include <errno.h>
#include <poll.h>
#include <sys/types.h>


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
/* Ricerca fd nell'array e ritorna l'indice, oppure -1 se non lo trova. */
{
	int i;

	for (i = 0; i < nfds_ && fds_[i].fd != fd; i++);

	if (i == nfds_)
		return -1;     /* not found */
	return i;
}


static int
add_fd (int fd)
/* Aggiunge fd all'array e ritorna il suo indice. */
{
	fds_[nfds_].fd = fd;
	fds_[nfds_].events = 0;
	fds_[nfds_].revents = 0;

	nfds_++;
	return nfds_ - 1;
}


/****************************************************************************
			      Funzioni esportate
****************************************************************************/

void
pm_fd_zero (void)
/* Svuota l'array dei file descriptor. */
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
pm_fd_set (int fd, int ev)
/* Aggiunge fd all'array con gli eventi specificati. */
{
	int i;
	
	i = add_fd (fd);
	fds_[i].events = ev;
}


int
pm_fd_get_events (int fd)
/* Ritorna gli eventi precedentemente impostati a fd. */
{
	int i;

	i = search_fd (fd);
	return fds_[i].events;
}


int
pm_fd_get_revents (int fd)
/* Ritorna gli eventi impostati da poll a fd. */
{
	int i;

	i = search_fd (fd);
	return fds_[i].revents;
}


int
pm_poll (int tmout)
/* Esegue poll con il timeout specificato. */
{
	int nready;

	do {
		nready = poll (fds_, nfds_, tmout);
	} while (nready == -1 && errno == EINTR);

	return nready;
}
