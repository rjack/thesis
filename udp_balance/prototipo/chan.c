#include <assert.h>
#include <errno.h>

#include "types.h"
#include "util.h"


wifi_link_t *
wifi_link_create (const char *iface_name, const char *bind_ip,
             const char *bind_port)
{
	int errno_s;
	chan_t *new_wfl;

	assert (iface_name != NULL);
	assert (bind_ip != NULL);
	assert (bind_port != NULL);

	new_wfl = my_alloc (sizeof(chan_t));
	new_wfl->ch_sock = socket_bound (bind_ip, bind_port);
	if (new->ch_sock == -1)
		goto socket_bound_err;

	new_wfl->ch_iface = my_strdup (iface_name);
	new_wfl->ch_bind_ip = my_strdup (bind_ip);
	new_wfl->ch_bind_port = my_strdup (bind_port);
	new_wfl->ch_suspected = FALSE;

	return new_wfl;

socket_bound_err:
	errno_s = errno;
	free (new_wfl);
	errno = errno_s;
	return NULL;
}
