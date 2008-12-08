#ifndef ULB_PROTO_LIST_H
#define ULB_PROTO_LIST_H

#include "types.h"


typedef int list_t;


typedef struct list_node {
	void *n_ptr;
	struct list_node *n_next;
	struct list_node *n_prev;
} list_node_t;


#define     LIST_ERR     ((list_t)-1)


/* list.c */
list_t list_create(void (*node_value_destroy)(void *), size_t node_value_size);
void list_destroy(list_t ls);
bool list_is_empty(list_node_t *tp);
list_node_t *list_next(list_node_t *node);
list_node_t *list_head(list_node_t *tp);
bool list_node_is_last(list_node_t *tp, list_node_t *ptr);
list_node_t *list_contains(list_node_t *tp, bool (*cmpfun)(void *, void *), void *term);
list_node_t *list_remove(list_node_t **tp, list_node_t *ptr);
void list_push(list_node_t **tp, list_node_t *new_head);
void list_enqueue(list_node_t **tp, list_node_t *new_tail);
list_node_t *list_dequeue(list_node_t **tp);
void list_inorder_insert(list_node_t **tp, list_node_t *ptr, int (*cmpfun)(void *, void *));
list_node_t *list_remove_if(list_node_t **tp, bool (*cmpfun)(void *, void *), void *args);
void *list_fold_left(list_node_t *tp, void *(*fun)(void *, void *), void *initial_value);
list_node_t *list_cat(list_node_t *tp_0, list_node_t *tp_1);
void list_foreach_do(list_node_t *tp, void (*fun)(void *, void *), void *args);


#endif /* ULB_PROTO_LIST_H */
