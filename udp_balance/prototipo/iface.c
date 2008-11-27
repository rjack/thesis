#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "crono.h"
#include "list.h"
#include "types.h"
#include "util.h"


struct match_iface_args {
	const char *mia_name;
	const char *mia_bind_ip;
	const char *mia_bind_port;
};


static list_node_t *ifaces;
static size_t ifaces_len;


static bool
match_iface (void *ifp, void *argsp)
{
	iface_t *if_ptr = (iface_t *)ifp;
	struct match_iface_args *args = (struct match_iface_args *)argsp;

	if (if_ptr->if_name == args->mia_name
	    && if_ptr->if_bind_ip == args->mia_bind_ip
	    && if_ptr->if_bind_port == args->mia_bind_port)
		return TRUE;
	return FALSE;
}


void
iface_init_module (void)
{
	ifaces = list_create ();
	ifaces_len = 0;
}


int
iface_up (const char *name, const char *bind_ip)
{
	iface_t *if_ptr;

	assert (ifaces_len <= IFACE_MAX);
	if (ifaces_len == IFACE_MAX) {
		fprintf (stderr,
			 "Impossibile aggiungere un'interfaccia di rete, "
			 "numero massimo = %d\n", IFACE_MAX);
		exit (EXIT_FAILURE);
	}

	if_ptr = my_alloc (sizeof(iface_t));

	if_ptr->if_suspected = FALSE;
	if_ptr->if_name = my_strdup (name);
	if_ptr->if_bind_ip = my_strdup (bind_ip);
	if_ptr->if_bind_port = my_strdup (PX_LOC_PORT);
	if_ptr->if_pfd.fd = socket_bound_conn (bind_ip, PX_LOC_PORT,
					       PX_REM_IP, PX_REM_PORT);
	if_ptr->if_pfd.events = 0;
	if_ptr->if_pfd.revents = 0;
	timeout_set (&if_ptr->if_keepalive, &time_150ms);
	timeout_start (&if_ptr->if_keepalive, &now);

	list_enqueue (&ifaces, new_node (if_ptr));
	ifaces_len++;

	/* TODO controllo errore socket_bound_conn */
	return 1;
}


void
iface_down (const char *name, const char *bind_ip, const char *bind_port)
{
	iface_t *if_ptr;
	list_node_t *node_ptr;
	struct match_iface_args args;

	args.mia_name = name;
	args.mia_bind_ip = bind_ip;
	args.mia_bind_port = bind_port;

	node_ptr = list_contains (ifaces, &match_iface, &args);
	list_remove (&ifaces, node_ptr);
	if_ptr = node_ptr->n_ptr;

	free (if_ptr->if_name);
	free (if_ptr->if_bind_ip);
	free (if_ptr->if_bind_port);

	free (node_ptr);
}


iface_t *
iface_iterator_get_first (iface_iterator_t *ii_ptr)
{
	assert (ii_ptr != NULL);
	*ii_ptr = list_head (ifaces);
	if (*ii_ptr == NULL)
		return NULL;
	return (*ii_ptr)->n_ptr;
}


iface_t *
iface_iterator_get_next (iface_iterator_t *ii_ptr)
{
	assert (ii_ptr != NULL);

	if (*ii_ptr == NULL)
		return NULL;

	*ii_ptr = list_next (*ii_ptr);
	if (*ii_ptr == list_head (ifaces)) {
		*ii_ptr = NULL;
		return NULL;
	}

	return (*ii_ptr)->n_ptr;
}


int
iface_get_events (iface_t *iface)
{
	assert (iface != NULL);

	return iface->if_pfd.revents;
}


bool
iface_keepalive_left (iface_t *if_ptr, struct timeval *now,
                      struct timeval *result)
{
	timeout_left (&if_ptr->if_keepalive, now, result);
	if (tv_cmp (result, &time_0ms) <= 0)
		return FALSE;
	return TRUE;
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
	if (!list_is_empty (ifaces)) {
		list_node_t *head = list_head (ifaces);
		return head->n_ptr;
	}
	return NULL;
}


char *
iface_to_string (iface_t *iface, char *str)
{
	int nbytes;

	assert (iface != NULL);
	assert (str != NULL);

	nbytes = sprintf (str, "%s %s:%s",
	                  iface->if_name, iface->if_bind_ip,
	                  iface->if_bind_port);
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
		pfd[i] = if_ptr->if_pfd;

	*pfd_used = ifaces_len;
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
		if_ptr->if_pfd.revents = pfd[i].revents;
}


int
iface_write (iface_t *iface, dgram_t *dg)
{
	/* TODO iface_write */
	/* TODO reset keepalive */

	return 0;
}


dgram_t *
iface_read (iface_t *iface)
{
	/* TODO iface_read */

	return NULL;
}


int
iface_handle_err (iface_t *iface)
{
	/* TODO iface_handle_err */

	return 0;
}
