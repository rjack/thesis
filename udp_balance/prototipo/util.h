#ifndef ULB_PROTO_UTIL_H
#define ULB_PROTO_UTIL_H

#include "types.h"

char *my_strdup(const char *str);
list_node_t *new_node(void *ptr);
fd_t socket_bound_and_connected (const char *bind_ip, const char *bind_port,
                                 const char *conn_ip, const char *conn_port);
void *my_alloc(size_t nbytes);
void collect_garbage(void);

#endif /* ULB_PROTO_UTIL_H */
