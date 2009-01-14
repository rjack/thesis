#ifndef ULB_LIST_H
#define ULB_LIST_H

#include "types.h"


/****************************************************************************
				   Typedefs
****************************************************************************/

typedef int list_t;


struct list_node {
	void *n_ptr;
	struct list_node *n_next;
	struct list_node *n_prev;
};


typedef struct list_node* list_iterator_t;


typedef int (*f_compare_t)(void *, void *);
typedef bool (*f_bool_t)(void *, void *);
typedef void (*f_destroy_t)(void *);
typedef void (*f_callback_t)(void *, void *);


/****************************************************************************
				   Defines
****************************************************************************/

#define     LIST_ERR                        -1

#define     LIST_SCAN_FORWARD                0
#define     LIST_SCAN_BACKWARD             0x1


/****************************************************************************
			     Function prototypes
****************************************************************************/

list_t
list_create (f_destroy_t node_value_destroy);
/*
 * Create a new list. The node_value_destroy function is used to free the
 * element of the list, and cannot be NULL.
 * Return
 * 	the new list handle
 * or	LIST_ERR if errors occurred.
 */


void
list_destroy (list_t lst);
/*
 * Empty and destroy the given list. Operations on a destroyed list will have
 * undefined results.
 *
 * The elements of the list are freed using the node_value_destroy function
 * originally passed to list_create.
 */


void
list_garbage_collect (void);
/*
 * Free unused module memory.
 */


bool
list_is_empty (list_t lst);
/*
 * Return
 * 	TRUE if lst is empty,
 * or	FALSE otherwise.
 */


void *
list_peek (list_t lst);
/*
 * Return
 * 	the first element of lst without removing it from the list,
 * or	NULL if lst is empty.
 */


int
list_length (list_t lst);
/*
 * Return
 * 	the number of elements in lst.
 */


void *
list_iterator_first (list_t lst, list_iterator_t *lit);
/*
 * Get the first element of lst and initialize lit status.
 * Return
 * 	a pointer to the first element of lst,
 * or	NULL if lst is empty.
 */


void *
list_iterator_last (list_t lst, list_iterator_t *lit);
/*
 * Get the last element of lst and initialize lit status.
 * Return
 * 	a pointer to the last element of lst,
 * or	NULL if lst is empty.
 */


void *
list_iterator_next (list_t lst, list_iterator_t *lit);
/*
 * Get the next element of the iteration over lst.
 * Return
 * 	the pointer of the next element
 * or	NULL if no more elements.
 */


void *
list_iterator_prev (list_t lst, list_iterator_t *lit);
/*
 * Get the prev element of the iteration over lst.
 * Return
 * 	the pointer of the next element
 * or	NULL if no more elements.
 */


void *
list_find (list_t lst, f_bool_t my_test, void *term, int mode);
/*
 * Scan lst calling `my_test (element, term)' on each of lst element, until
 * my_test return TRUE.
 * mode can be set to LIST_SCAN_FORWARD and LIST_SCAN_BACKWARD.
 *
 * Return
 * 	the element satisfing my_test
 * or	NULL if there's no such element, or lst is empty.
 */

int
list_push (list_t lst, void *head_element);
/*
 * Insert head_element as lst first element.
 */


int
list_enqueue (list_t lst, void *tail_element);
/*
 * Insert tail_element as lst last element.
 */


void *
list_dequeue (list_t lst);
/*
 * Return
 * 	the first element of lst removing it from lst,
 * or	NULL if lst is empty.
 */


void
list_inorder_insert (list_t lst, void *new_element, f_compare_t my_cmp);
/*
 * Insert new_element into lst, scanning the list and calling `my_cmp
 * (element, new_element)' for each element of list.
 *
 * my_cmp (a, b) must return -1, 0 or 1 if a is respectively less, equal or
 * greater than b.
 */


list_t
list_remove_if (list_t lst, f_bool_t my_test, void *args);
/*
 * Remove all of the lst elements that satisfy `my_test (element, args)'.
 * Return
 * 	a new list containing the removed elements
 * NOTE: the returned list can be empty if no elements satisfied my_test.
 * NOTE: the list must be destroyed with list_destroy.
 */


void *
list_remove_one (list_t lst, f_bool_t my_test, void *args);
/*
 * Remove the first element of lst that satisfies `my_test (element, args)'.
 *
 * Return
 * 	the value that satisfied my_test
 * or	NULL fi list is empty or no such element found.
 */


void
*list_fold_left (list_t lst, void *(*fun)(void *, void *),
                 void *initial_value);
/*
 * Left folding. God help us.
 */


void
list_cat (list_t lst_0, list_t lst_1);
/*
 * Dequeue all lst_1 elements and enqueue them in lst_0.
 */


void
list_foreach_do (list_t lst, void (*fun)(void *, void *), void *args);
/*
 * Call `fun (element, args)' for each of the list elements.
 */

#endif /* ULB_LIST_H */
