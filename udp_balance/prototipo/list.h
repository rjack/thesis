#ifndef ULB_PROTO_LIST_H
#define ULB_PROTO_LIST_H

#include "types.h"


/****************************************************************************
				     Tipi
****************************************************************************/

/* Handler per liste. */
typedef int list_t;


typedef struct list_node {
	void *n_ptr;
	struct list_node *n_next;
	struct list_node *n_prev;
} list_node_t;


typedef list_node_t * list_iterator_t;


typedef int (*f_compare_t)(void *, void *);


/****************************************************************************
				   Costanti
****************************************************************************/

#define     LIST_ERR                        -1

#define     LIST_SCAN_BACKWARD             0x1
#define     LIST_SCAN_STOP_ON_FIRST        0x2


/****************************************************************************
				  Prototipi
****************************************************************************/

/* list.c */
list_t list_create(void (*node_value_destroy)(void *), size_t node_value_size);
void list_destroy(list_t lst);
void list_garbage_collect(void);
bool list_is_valid(list_t lst);
bool list_is_empty(list_t lst);
void *list_peek(list_t lst);
bool list_length(list_t lst);
void *list_iterator_get_first(list_t lst, list_iterator_t *lit);
void *list_iterator_get_last(list_t lst, list_iterator_t *lit);
void *list_iterator_get_next(list_t lst, list_iterator_t *lit);
void *list_iterator_get_prev(list_t lst, list_iterator_t *lit);
void *list_contains(list_t lst, f_compare_t my_cmp, void *term, int mode);
int list_push(list_t lst, void *head_element);
int list_enqueue(list_t lst, void *tail_element);
void *list_dequeue(list_t lst);
void list_inorder_insert(list_t lst, void *new_element, f_compare_t my_cmp);
list_t list_remove_if(list_t lst, f_compare_t my_cmp, void *args);
void *list_fold_left(list_t lst, void *(*fun)(void *, void *), void *initial_value);
void list_cat(list_t lst_0, list_t lst_1);
void list_foreach_do(list_t lst, void (*fun)(void *, void *), void *args);

#endif /* ULB_PROTO_LIST_H */
