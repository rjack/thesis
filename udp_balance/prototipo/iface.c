#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "crono.h"
#include "dgram.h"
#include "list.h"
#include "types.h"
#include "util.h"


struct match_iface_args {
	const char *mia_name;
	const char *mia_loc_ip;
};


static list_node_t *ifaces;
static size_t ifaces_len;


static bool
match_iface (void *ifp, void *argsp)
{
	iface_t *if_ptr = (iface_t *)ifp;
	struct match_iface_args *args = (struct match_iface_args *)argsp;

	if (strcmp (if_ptr->if_name, args->mia_name) == 0
	    && strcmp (if_ptr->if_loc_ip, args->mia_loc_ip) == 0)
		return TRUE;
	return FALSE;
}


void
iface_init_module (void)
{
	ifaces = list_create ();
	ifaces_len = 0;
}


bool
iface_must_send_keepalive (const iface_t *if_ptr)
{
	return if_ptr->if_must_send_keepalive;
}


int
iface_up (const char *name, const char *loc_ip)
{
	struct timeval now;
	iface_t *if_ptr;

	gettime (&now);

	assert (ifaces_len <= IFACE_MAX);
	if (ifaces_len == IFACE_MAX) {
		fprintf (stderr,
			 "Impossibile aggiungere un'interfaccia di rete, "
			 "numero massimo = %d\n", IFACE_MAX);
		exit (EXIT_FAILURE);
	}

	if_ptr = my_alloc (sizeof(iface_t));

	if_ptr->if_suspected = FALSE;
	if_ptr->if_must_send_keepalive = FALSE;
	if_ptr->if_name = my_strdup (name);
	if_ptr->if_loc_ip = my_strdup (loc_ip);
	if_ptr->if_loc_port = my_strdup (PX_LOC_PORT);
	if_ptr->if_pfd.fd = socket_bound_conn (loc_ip, PX_LOC_PORT,
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
iface_down (const char *name, const char *loc_ip)
{
	iface_t *if_ptr;
	list_node_t *node_ptr;
	struct match_iface_args args;

	args.mia_name = name;
	args.mia_loc_ip = loc_ip;

	node_ptr = list_contains (ifaces, &match_iface, &args);
	list_remove (&ifaces, node_ptr);
	ifaces_len--;
	if_ptr = node_ptr->n_ptr;

	free (if_ptr->if_name);
	free (if_ptr->if_loc_ip);
	free (if_ptr->if_loc_port);

	free (if_ptr);

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
iface_get_events (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	return if_ptr->if_pfd.revents;
}


void
iface_keepalive_left (iface_t *if_ptr, struct timeval *result)
{
	struct timeval now;

	gettime (&now);

	timeout_left (&if_ptr->if_keepalive, &now, result);
	if (tv_cmp (result, &time_0ms) <= 0)
		if_ptr->if_must_send_keepalive = TRUE;
	else
		if_ptr->if_must_send_keepalive = FALSE;
}


void
iface_set_events (iface_t *if_ptr, int e)
{
	assert (if_ptr != NULL);

	if_ptr->if_pfd.events |= e;
}


void
iface_reset_events (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	if_ptr->if_pfd.events = 0;
	if_ptr->if_pfd.revents = 0;
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


void
iface_print (iface_t *if_ptr)
{
	assert (if_ptr != NULL);

	printf ("%s %s:%s [%c%c] ",
		if_ptr->if_name, if_ptr->if_loc_ip, if_ptr->if_loc_port,
		if_ptr->if_suspected ? 's' : '-',
		if_ptr->if_must_send_keepalive ? 'k' : '-');
	printf ("keepalive");
	timeout_print (&if_ptr->if_keepalive);
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


ssize_t
iface_write (iface_t *if_ptr, dgram_t *dg)
{
	ssize_t nsent;
	struct timeval now;

	assert (if_ptr != NULL);
	assert (dg != NULL);

	nsent = dgram_write_getID (if_ptr->if_pfd.fd, dg, NULL, 0);
	if (nsent == -1)
		return -1;

	assert (nsent == dg->dg_datalen);

	/* reset timeout keepalive */
	gettime (&now);
	timeout_start (&if_ptr->if_keepalive, &now);
	if_ptr->if_must_send_keepalive = FALSE;

	return nsent;
}


dgram_t *
iface_read (iface_t *if_ptr)
{
	dgram_t *dg;

	assert (if_ptr != NULL);

	dg = dgram_read (if_ptr->if_pfd.fd, NULL, NULL);

	return dg;
}


int
iface_handle_err (iface_t *if_ptr)
{
	fprintf (stderr, "Errore!\n");

	return 0;
}
