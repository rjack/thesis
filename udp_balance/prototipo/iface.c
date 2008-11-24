#include <assert.h>
#include <stdio.h>
#include <unistd.h>

#include "types.h"


static iface_t ifaces[IFACE_MAX];
static size_t ifaces_used = 0;


int
iface_up (const char *name, const char *bind_ip, const char *bind_port)
{
	/* TODO iface_up */

	return 0;
}


int
iface_down (const char *name, const char *bind_ip, const char *bind_port)
{
	/* TODO iface_down */

	return 0;
}


iface_t *
iface_iterator_get_first (iface_iterator_t *ii_ptr)
{
	assert (ii_ptr != NULL);

	*ii_ptr = 0;
	if (*ii_ptr == ifaces_used) {
		*ii_ptr = -1;
		return NULL;
	}

	return &ifaces[*ii_ptr];
}


iface_t *
iface_iterator_get_next (iface_iterator_t *ii_ptr)
{
	assert (ii_ptr != NULL);
	assert (*ii_ptr >= -1);
	assert (*ii_ptr < ifaces_used);

	if (*ii_ptr < 0)
		return NULL;

	(*ii_ptr)++;
	if (*ii_ptr == ifaces_used) {
		*ii_ptr = -1;
		return NULL;
	}

	return &ifaces[*ii_ptr];
}


int
iface_get_events (iface_t *iface)
{
	assert (iface != NULL);

	return iface->if_pfd.revents;
}


void
iface_set_events (iface_t *iface, int e)
{
	assert (iface != NULL);

	iface->if_pfd.events |= e;
}


void
iface_reset_events (iface_t *iface)
{
	assert (iface != NULL);

	iface->if_pfd.events = 0;
	iface->if_pfd.revents = 0;
}


iface_t *
iface_get_current (void)
{
	if (ifaces_used > 0)
		return &ifaces[ifaces_used - 1];
	return NULL;
}


char *
iface_to_string (iface_t *iface, char *str)
{
	int nbytes;

	assert (iface != NULL);
	assert (str != NULL);

	nbytes = sprintf (str, "%s %s:%s",
			  iface->if_name, iface->if_bind_ip, iface->if_pfd);
	if (iface->if_suspected)
		sprintf (str + nbytes, " [s]");
	return str;
}


void
iface_fill_pollfd (struct pollfd *pfd, size_t *pfd_used)
{
	int i;
	iface_t *if_ptr;
	iface_iterator_t ii;

	for (i = 0, if_ptr = iface_iterator_get_first (&ii);
	     if_ptr != NULL;
	     i++, if_ptr = iface_iterator_get_next (&ii))
		pfd[i] = iface->if_pfd;

	*pfd_used = ifaces_used;
}


void
iface_read_pollfd (struct pollfd *pfd)
{
	int i;
	iface_t *if_ptr;
	iface_iterator_t ii;

	for (i = 0, if_ptr = iface_iterator_get_first (&ii);
	     if_ptr != NULL;
	     i++, if_ptr = iface_iterator_get_next (&ii))
		iface->if_pfd.revents = pfd[i].revents;
}


int
iface_write (iface_t *iface, dgram_t *dg)
{
	/* TODO iface_write */

	return 0;
}


int
iface_if_pollout_write (iface_t *iface, dgram_t *dg)
{
	/* TODO iface_if_pollout_write */

	return 0;
}


int
iface_if_pollin_read (iface_t *iface, void *discard)
{
	/* TODO iface_if_pollin_read */

	return 0;
}


int
iface_if_pollerr_handle (iface_t *iface, void *discard)
{
	/* TODO iface_if_pollerr_handle */

	return 0;
}
