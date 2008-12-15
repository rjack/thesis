#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "list.h"
#include "types.h"


/****************************************************************************
				     Tipi
****************************************************************************/

struct list_info {
	struct list_node *li_tail_ptr;
	f_destroy_t li_node_value_destroy;
	size_t li_node_value_size;
	int li_list_len;
};


/****************************************************************************
			       Variabili locali
****************************************************************************/

/* Array di liste, indicizzato dagli handler di tipo list_t. */
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
#endif /* NDEBUG */


static bool
list_info_is_used (struct list_info *li)
{
	assert (li != NULL);

	if (li->li_tail_ptr == NULL
	    && li->li_list_len == 0
	    && li->li_node_value_destroy == NULL
	    && li->li_node_value_size == 0)
		return FALSE;
	return TRUE;

}


static void
list_info_set_unused (struct list_info *li)
{
	assert (li != NULL);

	memset (li, 0, sizeof(*li));
}


static struct list_node *
list_node_remove (list_t lst, struct list_node *ptr)
{
	struct list_info *linfo;

	assert (module_ok ());
	assert (list_is_valid (lst));

	linfo = &(db[lst]);

	if (linfo->li_list_len == 1)
		linfo->li_tail_ptr = NULL;
	else {
		/* Aggiorna il tail pointer se rimuove la coda. */
		if (ptr == linfo->li_tail_ptr)
			linfo->li_tail_ptr = linfo->li_tail_ptr->n_prev;
		ptr->n_prev->n_next = ptr->n_next;
		ptr->n_next->n_prev = ptr->n_prev;
	}
	ptr->n_next = NULL;
	ptr->n_prev = NULL;

	linfo->li_list_len--;
	return ptr;
}


static struct list_node *
list_node_create (void *element)
{
	struct list_node *new_node;

	new_node = malloc (sizeof(*new_node));
	if (new_node == NULL)
		return NULL;

	new_node->n_ptr = element;
	return new_node;
}


static void *
list_node_destroy (struct list_node *node)
{
	void *element;

	element = node->n_ptr;
	free (node);

	return element;
}


static int
list_node_insert (list_t lst, struct list_node *new_node)
/*
 * Inserisce il nuovo nodo tra la testa e la coda.
 * Ritorna la nuova lunghezza di lst.
 */
{
	assert (module_ok ());
	assert (list_is_valid (lst));
	assert (new_node != NULL);

	if (list_is_empty (lst)) {
		new_node->n_next = new_node;
		new_node->n_prev = new_node;
		db[lst].li_tail_ptr = new_node;
	} else {
		/* new_node <-> head */
		db[lst].li_tail_ptr->n_next->n_prev = new_node;
		new_node->n_next = db[lst].li_tail_ptr->n_next;

		/* tail <-> new_node */
		db[lst].li_tail_ptr->n_next = new_node;
		new_node->n_prev = db[lst].li_tail_ptr;
	}
	db[lst].li_list_len++;

	return db[lst].li_list_len;
}


static int
list_node_push (list_t lst, struct list_node *new_node)
{
	return list_node_insert (lst, new_node);
}


static int
list_node_enqueue (list_t lst, struct list_node *new_node)
{
	int len;

	len = list_node_insert (lst, new_node);
	db[lst].li_tail_ptr = db[lst].li_tail_ptr->n_next;

	return len;
}


/****************************************************************************
			      Funzioni esportate
****************************************************************************/

list_t
list_create (f_destroy_t node_value_destroy, size_t node_value_size)
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

	/* Ricerca list_info inutilizzato. */
	for (new_handle = 0;
	     new_handle < db_used && list_info_is_used (&(db[new_handle]));
	     new_handle++);

	/* Tutti utilizzati, aggiunta in coda. */
	if (new_handle == db_used) {
		/* Niente spazio in coda, riallocazione db. */
		if ((db_size - db_used) == 0) {
			struct list_info *new_db;
			new_db = realloc (db, (db_size + 1)
					       * sizeof(struct list_info));
			if (new_db == NULL)
				return LIST_ERR;
			db = new_db;
			db_size++;
		}
		db_used++;
	}

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
	list_info_set_unused (&(db[lst]));

	if (lst == db_used - 1)
		db_used--;
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
list_contains (list_t lst, f_bool_t my_test, void *term, int mode)
/*
 * Ritorna il primo elemento che soddisfi la funzione my_test, NULL se la lista
 * e' vuota o nessun elemento soddisfa la funzione data.
 * Se la flag LIST_SCAN_BACKWARD e' impostata in mode, comincia a cercare
 * dalla fine della lista.
 */
{
	void *element;
	list_iterator_t lit;

	assert (module_ok ());
	assert (list_is_valid (lst));
	assert (my_test != NULL);

	element = (mode & LIST_SCAN_BACKWARD) ?
	          list_iterator_get_last (lst, &lit) :
		  list_iterator_get_first (lst, &lit);

	while (element != NULL && !my_test (element, term))
		element = (mode & LIST_SCAN_BACKWARD) ?
		           list_iterator_get_prev (lst, &lit) :
		           list_iterator_get_next (lst, &lit);
	return element;
}


int
list_push (list_t lst, void *head_element)
{
	struct list_node *new_node;

	new_node = list_node_create (head_element);
	if (new_node == NULL)
		return LIST_ERR;
	return list_node_push (lst, new_node);
}


int
list_enqueue (list_t lst, void *tail_element)
{
	struct list_node *new_node;

	new_node = list_node_create (tail_element);
	if (new_node == NULL)
		return LIST_ERR;
	return list_node_enqueue (lst, new_node);
}


void *
list_dequeue (list_t lst)
{
	struct list_node *rmvd;

	assert (module_ok ());
	assert (list_is_valid (lst));

	if (list_is_empty (lst))
		return NULL;

	rmvd = list_node_remove (lst, db[lst].li_tail_ptr->n_next);
	assert (rmvd != NULL);

	return list_node_destroy (rmvd);
}


void
list_inorder_insert (list_t lst, void *new_element, f_compare_t my_cmp)
/*
 * Inserisce in lst un nuovo nodo che punta a new_element, in modo che la
 * lista risultante sia ordinata secondo la funzione my_cmp.
 */
{
	assert (module_ok ());
	assert (list_is_valid (lst));
	assert (my_cmp != NULL);

	if (list_is_empty (lst)
	    || my_cmp (new_element, db[lst].li_tail_ptr->n_ptr) > 0)
		list_enqueue (lst, new_element);
	else {
		struct list_node *cur;
		struct list_node *new_node;

		cur = db[lst].li_tail_ptr->n_next;

		while (my_cmp (new_element, cur->n_ptr) > 0)
			cur = cur->n_next;

		new_node = list_node_create (new_element);
		new_node->n_prev = cur->n_prev;
		cur->n_prev->n_next = new_node;
		cur->n_prev = new_node;
		new_node->n_next = cur;
		db[lst].li_list_len++;
	}
}


list_t
list_remove_if (list_t lst, f_bool_t my_test, void *args)
/*
 * Ritorna una nuova lista formata da tutti gli elementi rimossi da lst che
 * hanno soddisfatto la funzione my_test.
 */
{
	struct list_node *cur;
	struct list_node *nxt;
	list_t rmvd;

	assert (module_ok ());
	assert (list_is_valid (lst));
	assert (my_test != NULL);

	rmvd = list_create (db[lst].li_node_value_destroy,
	                    db[lst].li_node_value_size);

	if (!list_is_empty (lst))
		cur = db[lst].li_tail_ptr->n_next;
	else
		cur = NULL;
	while (!list_is_empty (lst) && cur != NULL) {
		if (cur->n_next == db[lst].li_tail_ptr->n_next)
			nxt = NULL;
		else
			nxt = cur->n_next;
		if (my_test (cur->n_ptr, args)) {
			/* XXX brutto! list_enqueue alloca un nodo,
			 * list_node_remove ritorna il nodo, quindi dobbiamo
			 * distruggerlo per poi ricrearlo! */
			list_node_remove (lst, cur);
			list_node_enqueue (rmvd, cur);
		}
		cur = nxt;
	}
	return rmvd;
}


void *
list_fold_left (list_t lst, void * (*fun)(void *, void *),
                void *initial_value)
{
	void *accumulator;
	void *element;
	list_iterator_t lit;

	assert (module_ok ());
	assert (list_is_valid (lst));
	assert (fun != NULL);

	accumulator = initial_value;

	for (element = list_iterator_get_first (lst, &lit);
	     element != NULL;
	     element = list_iterator_get_next (lst, &lit))
		accumulator = fun (accumulator, element);

	return accumulator;
}


void
list_cat (list_t lst_0, list_t lst_1)
/*
 * Travasa il contenuto di lst_1 in coda a lst_0.
 */
{
	assert (module_ok ());
	assert (list_is_valid (lst_0));
	assert (list_is_valid (lst_1));
	assert (db[lst_0].li_node_value_size == db[lst_1].li_node_value_size);

	while (!list_is_empty (lst_1))
		list_enqueue (lst_0, list_dequeue (lst_1));
}


void
list_foreach_do (list_t lst, void (*fun)(void *, void *), void *args)
/*
 * Chiama fun su ogni elemento di lst.
 */
{
	void *element;
	list_iterator_t lit;

	assert (module_ok ());
	assert (list_is_valid (lst));
	assert (fun != NULL);

	for (element = list_iterator_get_first (lst, &lit);
	     element != NULL;
	     element = list_iterator_get_next (lst, &lit))
		fun (element, args);
}
