#include <unistd.h>

#include "types.h"


list_node_t *
list_create (void)
{
	return NULL;
}


bool
list_is_empty (list_node_t *tp)
{
	return (tp == NULL);
}


list_node_t *
list_next (list_node_t *node)
{
	return node->n_next;
}


list_node_t *
list_head (list_node_t *tp)
{
	list_node_t *head = NULL;
	if (!list_is_empty (tp))
		head = list_next (tp);
	return head;
}


bool
list_node_is_last (list_node_t *tp, list_node_t *ptr)
{
	return (tp == ptr
	        && ptr->n_next == ptr
	        && ptr->n_prev == ptr);
}


list_node_t *
list_contains (list_node_t *tp, bool (*cmpfun)(void *, void *), void *term)
{
	bool found = FALSE;
	list_node_t *i = list_head (tp);
	if (i != NULL)
		while (!(found = cmpfun (i->n_ptr, term)) && i != tp)
			i = list_next (i);
	if (i != NULL && found)
		return i;
	return NULL;
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
{
	list_node_t *head = list_head (*tp);
	if (head != NULL)
		head = list_remove (tp, head);
	return head;
}


void
list_inorder_insert (list_node_t **tp, list_node_t *ptr, int (*cmpfun)(void *, void *))
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
list_fold_left (list_node_t *tp, void * (*fun)(void *, void *), void *initial_value)
{
	list_node_t *node_i;
	list_node_t *head;
	void *accumulator;

	head = list_head (tp);
	node_i = head;

	accumulator = initial_value;
	if (head != NULL)
		do {
			accumulator = fun (accumulator, node_i->n_ptr);
			node_i = list_next (node_i);
		} while (node_i != head);

	return accumulator;
}
