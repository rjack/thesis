#include <assert.h>
#include <stdlib.h>
#include <unistd.h>

#include "crono.h"
#include "list.h"
#include "util.h"
#include "types.h"

/*
 * Code per i datagram.
 */
/* da softphone a server */
static list_node_t *data_out;

/* da server a softphone */
static list_node_t *data_in;

/* spediti, da confermare */
static list_node_t *data_unaked;

/* datagram scartati, pronti per essere riutilizzati */
static list_node_t *data_discarded;


static bool
dgram_must_be_discarded (dgram_t *dg)
/* Ritorna TRUE se dg e'piu' vecchio di 150ms,
 *         FALSE altrimenti.
 * NON dealloca i timeout. */
{
	struct timeval left;

	timeout_left (dg->dg_life_to, &now, &left);

	if (tv_cmp (&left, &time_0ms) <= 0)
		return TRUE;
	return FALSE;
}


static bool
must_be_retransmitted (dgram_t *dg)
/* Ritorna TRUE e dealloca dg_retry_to se dg e'piu' vecchio di 30ms,
 *         FALSE altrimenti. */
{
	struct timeval left;

	timeout_left (dg->dg_retry_to, &now, &left);

	if (tv_cmp (&left, &time_0ms) <= 0) {
		free (dg->dg_retry_to);
		dg->dg_retry_to = NULL;
		return TRUE;
	}
	return FALSE;
}


void
dgram_init_module (void)
{
	data_out = list_create ();
	data_in = list_create ();
	data_unaked = list_create ();
	data_discarded = list_create ();
}


void
dgram_outward_all_unacked (struct timeval *now)
{
	/* TODO dgram_outward_all_unacked */
}


void
dgram_purge_all_old (struct timeval *now)
{
}


void
dgram_timeout_min (struct timeval *result)
{
}


dgram_t *
dgram_list_peek (int list)
{
	return NULL;
}


void
dgram_list_add (int list, dgram_t *dg)
{
}


dgram_t *
dgram_list_pop (int list)
{
	return NULL;
}


dgram_t *
dgram_read (fd_t sfd)
{
	return NULL;
}


int
dgram_write (fd_t sfd, dgram_t *dg)
{
	return 0;
}


void
dgram_free (dgram_t *dg)
{
}


dgram_t *
dgram_create_keepalive (void)
{
	return NULL;
}
