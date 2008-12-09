#include <assert.h>
#include <stdlib.h>
#include <unistd.h>

#include "list.h"
#include "types.h"


/****************************************************************************
				     Tipi
****************************************************************************/

struct list_info {
	list_node_t *li_tail_ptr;
	void (*li_node_value_destroy)(void *);
	size_t li_node_value_size;
	int li_list_len;
};


/****************************************************************************
			       Variabili locali
****************************************************************************/

/* Array di liste, indicizzato dagli handler di tipo list_t.
 * Invariante: tutti gli slot usati all'inizio dell'array. */
static struct list_info *db;
static size_t db_size;
static size_t db_used;


/****************************************************************************
			       Funzioni locali
****************************************************************************/

#ifndef NDEBUG
static bool
module_ok (void)
{
	bool ok = TRUE;

	if (db_used < 0)
		ok = FALSE;
	if (db_size < 0)
		ok = FALSE;

	if (db == NULL) {
		if (db_size != 0)
			ok = FALSE;
		if (db_used != 0)
			ok = FALSE;
	} else {
		if (db_size == 0)
			ok = FALSE;
		if (db_used > db_size)
			ok = FALSE;
	}

	return ok;
}
endif /* NDEBUG */


/****************************************************************************
			      Funzioni esportate
****************************************************************************/

list_t
list_create (void (*node_value_destroy)(void *), size_t node_value_size)
/*
 * Crea una nuova lista e ritorna il suo handler, oppure LIST_ERR se fallisce.
 * node_value_destroy e' la funzione per deallocare il valore degli elementi
 * puntati dai nodi della lista, node_value_size e' la dimensione di ogni
 * elemento.
 */
{
	list_t new_handle;
	struct list_info *new_list_info;

	assert (module_ok ());

	if ((db_size - db_used) == 0) {
		struct list_info *new_db;
		new_db = realloc (db, (db_size + 1)
						 * sizeof(struct list_info));
		if (new_db == NULL)
			return LIST_ERR;
		db = new_db;
		db_size++;
	}

	new_handle = db_used;
	db_used++;

	new_list_info = &(db[new_handle]);
	new_list_info->li_tail_ptr = NULL;
	new_list_info->li_node_value_destroy = node_value_destroy;
	new_list_info->li_node_value_size = node_value_size;
	new_list_info->li_list_len = 0;

	return new_handle;
}


void
list_destroy (list_t lst)
/* 
 * Libera la memoria associata ad ogni oggetto contenuto nella lista indicata
 * e alla lista stessa.
 * XXX non rialloca db, per quello c'e' list_garbage_collect.
 */
{
	void *element;
	void (*my_free)(void *);

	assert (module_ok ());
	assert (list_is_valid (lst));

	/* Svuotamento lista */
	my_free = db[lst].li_node_value_destroy;
	while ((element = list_dequeue (lst)) != NULL)
		my_free (element);

	/* Rimozione da db. */
	db_used--;
	if (db_used != 0)
		memcpy (&(db[lst]), &(db[db_used]),
		        sizeof(*db));
}


void
list_garbage_collect (void)
/*
 * Libera spazio: rialloca db se e' piu' grande del necessario o lo dealloca
 * se vuoto.
 */
{
	struct list_info *new_db;

	assert (module_ok ());

	if (db_size == db_used)
		return;

	if (db_used == 0) {
		free (db);
		db = NULL;
		db_size = 0;
	} else {
		new_db = realloc (db, db_used * sizeof(*db));
		if (new_db != NULL) {
			db = new_db;
			db_size = db_used;
		}
	}
}


bool
list_is_valid (list_t lst)
/*
 * Paranoia.
 */
{
	assert (module_ok ());

	if (lst != LIST_ERR
	    && (lst >= 0 || lst < db_used))
		return TRUE;
	return FALSE;
}


bool
list_is_empty (list_t lst)
/*
 * Ritorna TRUE se la lista e' vuota, FALSE altrimenti.
 */
{
	assert (module_ok ());
	assert (list_is_valid (lst));

	if (db[lst].li_tail_ptr == NULL)
		return TRUE;
	return FALSE;
}


void *
list_peek (list_t lst)
/*
 * Ritorna l'elemento puntato dal nodo in testa alla lista, oppure NULL se la
 * lista e' vuota.
 */
{
	assert (module_ok ());
	assert (list_is_valid (lst));

	if (!list_is_empty (lst))
		return db[lst].li_tail_ptr->n_next->n_ptr;
	return NULL;
}


bool
list_length (list_t lst)
/*
 * Ritorna la lunghezza della lista.
 */
{
	assert (module_ok ());
	assert (list_is_valid (lst));

	return db[lst].li_list_len;
}


void *
list_iterator_get_first (list_t lst, list_iterator_t *lit)
/*
 * Ritorna l'elemento in testa a lst e inizializza lit.
 */
{
	void *head;

	assert (module_ok ());
	assert (list_is_valid (lst));

	if (list_is_empty (lst)) {
		*lit = NULL;
		return NULL;
	}

	*lit = db[lst].li_tail_ptr->n_next;
	return (*lit)->n_ptr;
}


void *
list_iterator_get_last (list_t lst, list_iterator_t *lit)
/*
 * Ritorna l'elemento in coda a lst e inizializza lit.
 */
{
	void *tail;

	assert (module_ok ());
	assert (list_is_valid (lst));

	if (list_is_empty (lst)) {
		*lit = NULL;
		return NULL;
	}

	*lit = db[lst].li_tail_ptr;
	return (*lit)->n_ptr;
}


void *
list_iterator_get_next (list_t lst, list_iterator_t *lit)
/*
 * Ritorna l'elemento successivo secondo lo stato di lit, oppure NULL se e'
 * stata raggiunta la fine della lista.
 */
{
	assert (lit != NULL);
	assert (module_ok ());
	assert (list_is_valid (lst));

	if (*lit == NULL)
		return NULL;

	*lit = (*lit)->n_next;
	if (*lit == db[lst].li_tail_ptr->n_next) {
		*lit = NULL;
		return NULL;
	}

	return (*lit)->n_ptr;
}


void *
list_iterator_get_prev (list_t lst, list_iterator_t *lit)
/*
 * Ritorna l'elemento precedente secondo lo stato di lit, oppure NULL se e'
 * stata raggiunta la testa della lista.
 */
{
	assert (lit != NULL);
	assert (module_ok ());
	assert (list_is_valid (lst));

	if (*lit == NULL)
		return NULL;

	*lit = (*lit)->n_prev;
	if (*lit == db[lst].li_tail_ptr) {
		*lit = NULL;
		return NULL;
	}

	return (*lit)->n_ptr;
}


void *
list_contains (list_t lst, f_compare_t my_cmp, void *term, int mode)
/*
 * Ritorna il primo elemento che soddisfi la funzione my_cmp, NULL se la lista
 * e' vuota o nessun elemento soddisfa la funzione data.
 * Se la flag LIST_SCAN_BACKWARD e' impostata in mode, comincia a cercare
 * dalla fine della lista.
 */
{
	void *element;
	list_iterator_t lit;

	assert (module_ok ());
	assert (list_is_valid (lst));
	assert (my_cmp != NULL);

	element = (mode & LIST_SCAN_BACKWARD) ?
	          list_iterator_get_last (lst, &lit) :
		  list_iterator_get_first (lst, &lit);

	while (element != NULL && !my_cmp (element, term))
		element = (mode & LIST_SCAN_BACKWARD) ?
		           list_iterator_get_prev (lst, &lit) :
		           list_iterator_get_next (lst, &lit);
	return element;
}


static void
list_insert (list_node_t **tp, list_node_t *new)
{
	list_node_t *head = list_head (*tp);
	if (head == NULL) {
		new->n_next = new;
		new->n_prev = new;
		*tp = new;
	} else {
		/* new <-> head */
		head->n_prev = new;
		new->n_next = head;

		/* tail <-> new */
		(*tp)->n_next = new;
		new->n_prev = *tp;
	}
}


list_node_t *
list_remove (list_node_t **tp, list_node_t *ptr)
{
	/* Remove ptr from tp's list return it. */

	if (list_node_is_last (*tp, ptr))
		*tp = list_create ();
	else {
		/* Update tp when removing tail. */
		if (*tp == ptr)
			*tp = ptr->n_prev;
		ptr->n_prev->n_next = ptr->n_next;
		ptr->n_next->n_prev = ptr->n_prev;
	}
	ptr->n_next = NULL;
	ptr->n_prev = NULL;
	return ptr;
}


void
list_push (list_node_t **tp, list_node_t *new_head)
{
	list_insert (tp, new_head);
}


void
list_enqueue (list_node_t **tp, list_node_t *new_tail)
{
	list_insert (tp, new_tail);
	*tp = new_tail;
}


list_node_t *
list_dequeue (list_node_t **tp)
/* FIXME deve deallocare il list_node_t e ritornare il n_ptr */
{
	list_node_t *head = list_head (*tp);
	if (head != NULL)
		head = list_remove (tp, head);
	return head;
}


void
list_inorder_insert (list_node_t **tp, list_node_t *ptr,
                     int (*cmpfun)(void *, void *))
{
	list_node_t *cur = list_head (*tp);

	if (cur == NULL || cmpfun (ptr->n_ptr, (*tp)->n_ptr) > 0)
		list_enqueue (tp, ptr);
	else {
		while (cmpfun (ptr->n_ptr, cur->n_ptr) > 0)
			cur = list_next (cur);

		ptr->n_prev = cur->n_prev;
		cur->n_prev->n_next = ptr;
		cur->n_prev = ptr;
		ptr->n_next = cur;
	}
}


list_node_t *
list_remove_if (list_node_t **tp, bool (*cmpfun) (void *, void *), void *args)
{
	list_node_t *cur;
	list_node_t *nxt;
	list_node_t *rmq;

	rmq = list_create ();
	cur = list_head (*tp);
	while (!list_is_empty (*tp) && cur != NULL) {
		if (list_next (cur) == list_head (*tp))
			nxt = NULL;
		else
			nxt = list_next (cur);
		if (cmpfun (cur->n_ptr, args)) {
			list_remove (tp, cur);
			list_enqueue (&rmq, cur);
		}
		cur = nxt;
	}
	return rmq;
}


void *
list_fold_left (list_node_t *tp, void * (*fun)(void *, void *),
                void *initial_value)
{
	list_node_t *node;
	list_node_t *head;
	void *accumulator;

	head = list_head (tp);
	node = head;

	accumulator = initial_value;
	if (head != NULL)
		do {
			accumulator = fun (accumulator, node->n_ptr);
			node = list_next (node);
		} while (node != head);

	return accumulator;
}


list_node_t *
list_cat (list_node_t *tp_0, list_node_t *tp_1)
{
	list_node_t *head[2];

	if (list_is_empty (tp_0))
		return tp_1;
	if (list_is_empty (tp_1))
		return tp_0;

	head[0] = list_head (tp_0);
	head[1] = list_head (tp_1);

	tp_0->n_next = head[1];
	head[1]->n_prev = tp_0;

	tp_1->n_next = head[0];
	head[0]->n_prev = tp_1;

	return tp_1;
}


void
list_foreach_do (list_node_t *tp, void (*fun)(void *, void *), void *args)
{
	list_node_t *head;
	list_node_t *node;

	head = list_head (tp);
	node = head;

	if (head != NULL)
		do {
			fun (node->n_ptr, args);
			node = list_next (node);
		} while (node != head);
}
