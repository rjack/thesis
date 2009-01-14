#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "h/dtable_mgr.h"
#include "h/list.h"
#include "h/types.h"


/****************************************************************************
				     Tipi
****************************************************************************/

struct list {
	struct list_node *li_tail_ptr;
	f_destroy_t li_node_value_destroyer;
	int li_list_len;
};


/****************************************************************************
			       Variabili locali
****************************************************************************/

/* Array di liste, indicizzato dagli handler di tipo list_t. */
static struct list *table_ = NULL;
static size_t table_len_ = 0;
static size_t table_used_ = 0;


/****************************************************************************
			       Funzioni locali
****************************************************************************/


static bool
is_used (struct list *li)
{
	assert (li != NULL);

	if (li->li_tail_ptr == NULL
	    && li->li_list_len == 0
	    && li->li_node_value_destroyer == NULL)
		return FALSE;
	return TRUE;

}


static void
set_unused (struct list *li)
{
	assert (li != NULL);

	memset (li, 0, sizeof(*li));
}


static struct list_node *
list_node_remove (list_t lst, struct list_node *ptr)
{
	struct list *li;

	li = &(table_[lst]);

	if (li->li_list_len == 1)
		li->li_tail_ptr = NULL;
	else {
		/* Update tail pointer if removing tail. */
		if (ptr == li->li_tail_ptr)
			li->li_tail_ptr = li->li_tail_ptr->n_prev;
		ptr->n_prev->n_next = ptr->n_next;
		ptr->n_next->n_prev = ptr->n_prev;
	}
	ptr->n_next = NULL;
	ptr->n_prev = NULL;

	li->li_list_len--;
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
 * Insert new_node between head and tail.
 * Return
 * 	the new list len.
 */
{
	struct list *li;

	assert (new_node != NULL);

	li = &(table_[lst]);

	if (list_is_empty (lst)) {
		new_node->n_next = new_node;
		new_node->n_prev = new_node;
		li->li_tail_ptr = new_node;
	} else {
		/* new_node <-> head */
		li->li_tail_ptr->n_next->n_prev = new_node;
		new_node->n_next = li->li_tail_ptr->n_next;

		/* tail <-> new_node */
		li->li_tail_ptr->n_next = new_node;
		new_node->n_prev = li->li_tail_ptr;
	}
	li->li_list_len++;

	return li->li_list_len;
}


static int
list_node_push (list_t lst, struct list_node *new_node)
{
	return list_node_insert (lst, new_node);
}


static int
list_node_enqueue (list_t lst, struct list_node *new_node)
{
	int new_len;
	struct list *li;

	li = &(table_[lst]);

	new_len = list_node_insert (lst, new_node);
	li->li_tail_ptr = li->li_tail_ptr->n_next;

	return new_len;
}


/****************************************************************************
			      Exported functions
****************************************************************************/

list_t
list_create (f_destroy_t node_value_destroyer)
{
	list_t new_handle;
	struct list *new_list;

	if (node_value_destroyer == NULL)
		return LIST_ERR;

	new_handle = dtable_add ((void **)&table_, &table_len_, &table_used_,
	                         sizeof(*table_), (use_checker_t)is_used);
	if (new_handle == -1)
		return LIST_ERR;

	new_list = &(table_[new_handle]);
	new_list->li_tail_ptr = NULL;
	new_list->li_node_value_destroyer = node_value_destroyer;
	new_list->li_list_len = 0;

	return new_handle;
}


void
list_destroy (list_t lst)
{
	void *element;
	void (*my_free)(void *);

	/* Empty list. */
	my_free = table_[lst].li_node_value_destroyer;
	while ((element = list_dequeue (lst)) != NULL)
		my_free (element);

	dtable_remove ((void **)&table_, &table_used_, lst,
	               (unused_setter_t)set_unused);
}


void
list_garbage_collect (void)
{
	dtable_clear ((void **)&table_, &table_len_, &table_used_,
	              sizeof(*table_));
}


bool
list_is_empty (list_t lst)
{
	if (table_[lst].li_tail_ptr == NULL)
		return TRUE;
	return FALSE;
}


void *
list_peek (list_t lst)
{
	if (!list_is_empty (lst))
		return table_[lst].li_tail_ptr->n_next->n_ptr;
	return NULL;
}


int
list_length (list_t lst)
{
	return table_[lst].li_list_len;
}


void *
list_iterator_get_first (list_t lst, list_iterator_t *lit)
{
	if (list_is_empty (lst)) {
		*lit = NULL;
		return NULL;
	}

	*lit = table_[lst].li_tail_ptr->n_next;
	return (*lit)->n_ptr;
}


void *
list_iterator_get_last (list_t lst, list_iterator_t *lit)
{
	if (list_is_empty (lst)) {
		*lit = NULL;
		return NULL;
	}

	*lit = table_[lst].li_tail_ptr;
	return (*lit)->n_ptr;
}


void *
list_iterator_get_next (list_t lst, list_iterator_t *lit)
{
	assert (lit != NULL);

	if (*lit == NULL)
		return NULL;

	*lit = (*lit)->n_next;
	if (*lit == table_[lst].li_tail_ptr->n_next) {
		*lit = NULL;
		return NULL;
	}

	return (*lit)->n_ptr;
}


void *
list_iterator_get_prev (list_t lst, list_iterator_t *lit)
{
	assert (lit != NULL);

	if (*lit == NULL)
		return NULL;

	*lit = (*lit)->n_prev;
	if (*lit == table_[lst].li_tail_ptr) {
		*lit = NULL;
		return NULL;
	}

	return (*lit)->n_ptr;
}


void *
list_find (list_t lst, f_bool_t my_test, void *term, int mode)
{
	void *element;
	list_iterator_t lit;

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

	if (list_is_empty (lst))
		return NULL;

	rmvd = list_node_remove (lst, table_[lst].li_tail_ptr->n_next);
	assert (rmvd != NULL);

	return list_node_destroy (rmvd);
}


void
list_inorder_insert (list_t lst, void *new_element, f_compare_t my_cmp)
{
	struct list *li;

	assert (my_cmp != NULL);

	li = &(table_[lst]);

	if (list_is_empty (lst)
	    || my_cmp (li->li_tail_ptr->n_ptr, new_element) < 0)
		list_enqueue (lst, new_element);
	else {
		struct list_node *cur;
		struct list_node *new_node;

		cur = li->li_tail_ptr->n_next;

		while (my_cmp (cur->n_ptr, new_element) < 0)
			cur = cur->n_next;

		new_node = list_node_create (new_element);
		new_node->n_prev = cur->n_prev;
		cur->n_prev->n_next = new_node;
		cur->n_prev = new_node;
		new_node->n_next = cur;
		li->li_list_len++;
	}
}


list_t
list_remove_if (list_t lst, f_bool_t my_test, void *args)
{
	void *value;
	list_t rmvd;
	list_iterator_t lit;

	rmvd = list_create (table_[lst].li_node_value_destroyer);

	value = list_iterator_get_first (lst, &lit);
	while (value != NULL)
		if (my_test (value, args)) {
			/* list_iterator_t == node pointer. */
			struct list_node *selected = lit;
			value = list_iterator_get_next (lst, &lit);
			list_node_remove (lst, selected);
			list_node_enqueue (rmvd, selected);
		} else
			value = list_iterator_get_next (lst, &lit);
	return rmvd;
}


void *
list_remove_one (list_t lst, f_bool_t my_test, void *args)
{
	void *value;
	list_iterator_t lit;

	for (value = list_iterator_get_first (lst, &lit);
	     value != NULL && !my_test (value, args);
	     value = list_iterator_get_next (lst, &lit));

	if (value != NULL)
		return (list_node_destroy (list_node_remove (lst, lit)));
	return NULL;
}


void *
list_fold_left (list_t lst, void * (*fun)(void *, void *),
                void *initial_value)
{
	void *accumulator;
	void *element;
	list_iterator_t lit;

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
{
	while (!list_is_empty (lst_1))
		list_enqueue (lst_0, list_dequeue (lst_1));
}


void
list_foreach_do (list_t lst, void (*fun)(void *, void *), void *args)
{
	void *element;
	list_iterator_t lit;

	assert (fun != NULL);

	for (element = list_iterator_get_first (lst, &lit);
	     element != NULL;
	     element = list_iterator_get_next (lst, &lit))
		fun (element, args);
}
