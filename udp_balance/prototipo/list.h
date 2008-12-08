#ifndef ULB_PROTO_LIST_H
#define  ULB_PROTO_LIST_H

#include "types.h"


typedef int list_t;


typedef struct list_node {
	void *n_ptr;
	struct list_node *n_next;
	struct list_node *n_prev;
} list_node_t;


#define     LIST_ERR     (list_t)-1)


#endif /* ULB_PROTO_LIST_H */
