#ifndef ULB_PROTO_UTIL_H
#define ULB_PROTO_UTIL_H

#include "types.h"

void print_dot (void);
bool ptr_eq(void *ptr_1, void *ptr_2);
char *my_strdup(const char *str);
char *my_strncpy(char *dest, const char *src, size_t nbytes);
bool parse_im_msg(char **ifname_result, char **cmd_result, char **ip_result, const char *msg, size_t msg_len);
fd_t socket_bound_conn(const char *loc_ip, const char *loc_port, const char *rem_ip, const char *rem_port);
void *my_alloc(size_t nbytes);
void collect_garbage(void);

#endif /* ULB_PROTO_UTIL_H */
