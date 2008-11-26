#ifndef ULB_PROTO_DGRAM_H
#define ULB_PROTO_DGRAM_H

#include <sys/time.h>

#include "types.h"

dgram_t *dgram_create_keepalive(void);
dgram_t *dgram_list_peek(int list);
dgram_t *dgram_list_pop(int list);
dgram_t *dgram_read(fd_t sfd);
int dgram_write(fd_t sfd, dgram_t *dg);
void dgram_free(dgram_t *dg);
void dgram_init_module (void);
void dgram_list_add(int list, dgram_t *dg);
void dgram_outward_all_unacked(struct timeval *now);
void dgram_purge_all_old(struct timeval *now);
void dgram_timeout_min(struct timeval *result, const struct timeval *now);

#endif /* ULB_PROTO_DGRAM_H */
