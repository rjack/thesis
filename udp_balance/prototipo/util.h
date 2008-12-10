#ifndef ULB_PROTO_UTIL_H
#define ULB_PROTO_UTIL_H

#include "types.h"

/* util.c */
char *my_strdup(const char *str);
char *my_strncpy(char *dest, const char *src, size_t nbytes);
timeout_t *new_timeout(const struct timeval *value);
ssize_t sendmsg_getID_fake(int sfd, const struct msghdr *msg, int flags, int *id_result);
bool parse_im_msg(char **ifname_result, char **cmd_result, char **ip_result, const char *msg, size_t msg_len);
fd_t socket_bound_conn(const char *loc_ip, const char *loc_port, const char *rem_ip, const char *rem_port);
void *my_alloc(size_t nbytes);
void collect_garbage(void);

#endif /* ULB_PROTO_UTIL_H */
