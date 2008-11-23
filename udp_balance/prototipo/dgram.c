#include <assert.h>
#include <stdlib.h>
#include <unistd.h>

#include "crono.h"
#include "types.h"

/*
 * Code per i datagram.
 */
/* da softphone a server */
static dgram_t *data_out = NULL;

/* da server a softphone */
static dgram_t *data_in = NULL;

/* spediti, da confermare */
static dgram_t *data_unakd = NULL;

/* msg config interfacce */
static dgram_t *data_iface = NULL;

/* datagram scartati, pronti per essere riutilizzati */
static dgram_t *data_discarded = NULL;


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


static dgram_t *
list_cat (dgram_t *fst, dgram_t *snd)
/* Ritorna la concatenazione delle due liste fst e snd. */
{
	dgram_t *tail;

	if (fst == NULL)
		return snd;
	if (snd == NULL)
		return fst;

	for (tail = fst; tail->dg_next != NULL; tail = tail->dg_next);
	tail->dg_next = snd;

	return fst;
}


static dgram_t *
list_remove_if (bool (*test)(dgram_t *), dgram_t **lst)
/* Rimuove da lst tutti gli elementi che soddisfano test e li ritorna in una
 * lista. */
{
	dgram_t *cur;
	dgram_t *rmvd = NULL;
	dgram_t **rmvd_tp = &rmvd;
	dgram_t *passd = NULL;
	dgram_t **passd_tp = &passd;

	assert (lst != NULL);

	if (*lst == NULL)
		return NULL;

	for (cur = *lst; cur != NULL; cur = cur->dg_next)
		if (test (cur)) {
			*rmvd_tp = cur;
			rmvd_tp = &cur->dg_next;
		} else {
			*passd_tp = cur;
			passd_tp = &cur->dg_next;
		}

	*rmvd_tp = NULL;
	*passd_tp = NULL;

	*lst = passd;
	return rmvd;
}
