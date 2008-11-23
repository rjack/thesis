#ifndef ULB_PROTO_UTIL_H
#define ULB_PROTO_UTIL_H

#include "types.h"

char *my_strdup(const char *str);
fd_t socket_bound(const char *bind_ip, const char *bind_port);
void *my_alloc(size_t nbytes);
void collect_garbage(void);

#endif /* ULB_PROTO_UTIL_H */
