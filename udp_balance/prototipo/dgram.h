#ifndef ULB_PROTO_DGRAM_H
#define ULB_PROTO_DGRAM_H

#include <sys/time.h>
#include <netinet/in.h>

#include "types.h"

dgram_t *dgram_create(void);
dgram_t *dgram_create_keepalive(void);
dgram_t *dgram_list_peek(int list_id);
dgram_t *dgram_list_pop(int list_id);
dgram_t *dgram_read(fd_t sfd, struct sockaddr_in *src_addr_result, socklen_t *src_addr_result_len);
ssize_t dgram_write(fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr, socklen_t rem_addr_len);
void dgram_free(dgram_t *dg);
void dgram_init_module(void);
void dgram_list_add(int list_id, dgram_t *dg);
void dgram_outward_all_unacked(void);
void dgram_purge_all_old(void);
void dgram_timeout_min(struct timeval *min_result);
void dgram_print (const dgram_t *dg);

#endif /* ULB_PROTO_DGRAM_H */
