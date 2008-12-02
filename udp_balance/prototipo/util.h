#ifndef ULB_PROTO_UTIL_H
#define ULB_PROTO_UTIL_H

#include "types.h"

bool parse_im_msg (char **ifname_result, char **cmd_result, char **ip_result, const char *msg, size_t msg_len);
char *my_strdup(const char *str);
fd_t socket_bound_conn (const char *loc_ip, const char *loc_port, const char *rem_ip, const char *rem_port);
list_node_t *new_node(void *ptr);
ssize_t sendmsg_getID_fake (int sfd, const struct msghdr *msg, int flags, int *id_result);
timeout_t *new_timeout (const struct timeval *value);
void *my_alloc(size_t nbytes);
void collect_garbage(void);

#endif /* ULB_PROTO_UTIL_H */
