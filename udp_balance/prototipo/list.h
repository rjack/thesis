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


typedef bool (*)(void *, void *) f_compare_t;


/****************************************************************************
				   Costanti
****************************************************************************/

#define     LIST_ERR              ((list_t)-1)

#define     LIST_SCAN_BACKWARD             0x1
#define     LIST_SCAN_STOP_ON_FIRST        0x2


/****************************************************************************
				  Prototipi
****************************************************************************/


#endif /* ULB_PROTO_LIST_H */
