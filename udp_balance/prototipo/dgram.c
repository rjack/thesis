/*
 * Code per i datagram.
 */
/* da softphone a server */
static struct dgram *data_out = NULL;

/* da server a softphone */
static struct dgram *data_in = NULL;

/* spediti, da confermare */
static struct dgram *data_unakd = NULL;

/* msg config interfacce */
static struct dgram *data_iface = NULL;

/* datagram scartati, pronti per essere riutilizzati */
static struct dgram *data_discarded = NULL;


static bool
dgram_must_be_discarded (struct dgram *dg)
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
must_be_retransmitted (struct dgram *dg)
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


static struct dgram *
list_cat (struct dgram *fst, struct dgram *snd)
/* Ritorna la concatenazione delle due liste fst e snd. */
{
	struct dgram *tail;

	if (fst == NULL)
		return snd;
	if (snd == NULL)
		return fst;

	for (tail = fst; tail->dg_next != NULL; tail = tail->dg_next);
	tail->dg_next = snd;

	return fst;
}


static struct dgram *
list_remove_if (bool (*test)(struct dgram *), struct dgram **lst)
/* Rimuove da lst tutti gli elementi che soddisfano test e li ritorna in una
 * lista. */
{
	struct dgram *cur;
	struct dgram *rmvd = NULL;
	struct dgram **rmvd_tp = &rmvd;
	struct dgram *passd = NULL;
	struct dgram **passd_tp = &passd;

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
