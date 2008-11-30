#ifndef ULB_PROTO_UTIL_H
#define ULB_PROTO_UTIL_H

#include "types.h"

char *my_strdup(const char *str);
list_node_t *new_node(void *ptr);
timeout_t *new_timeout (const struct timeval *value);
fd_t socket_bound_conn (const char *loc_ip, const char *loc_port,
                        const char *rem_ip, const char *rem_port);
void *my_alloc(size_t nbytes);
void collect_garbage(void);

#endif /* ULB_PROTO_UTIL_H */
