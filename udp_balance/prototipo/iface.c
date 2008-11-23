#include <assert.h>
#include <unistd.h>

#include "types.h"


static iface_t ifaces[IFACE_MAX];
static size_t iface_num = 0;


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


/*
 * TODO iface_get_iterator
 *
iface_iterator_t *
iface_get_iterator (void)
{
	return NULL;
}
*/


int
iface_get_events (iface_t *iface, int ev)
{
	/* TODO iface_get_events */

	return 0;
}


int
iface_set_events (iface_t *iface, int ev)
{
	/* TODO iface_set_events */

	return 0;
}


iface_t *
iface_get_current (void)
{
	/* TODO iface_get_current */

	return NULL;
}


void
iface_to_string (iface_t *iface)
{
	/* TODO iface_to_string */
}


void
iface_fill_pollfd (struct pollfd *pfd, size_t *pfd_used)
{
	/* TODO iface_fill_pollfd */
}


void
iface_read_pollfd (struct pollfd *pfd, size_t *pfd_used)
{
	/* TODO iface_read_pollfd */
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
