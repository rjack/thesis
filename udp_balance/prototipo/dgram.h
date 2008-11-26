#ifndef ULB_PROTO_DGRAM_H
#define ULB_PROTO_DGRAM_H

#include <sys/time.h>

#include "types.h"

#define DGRAM_INWARD 0
#define DGRAM_OUTWARD 1

void dgram_outward_all_unacked(struct timeval *now);
void dgram_purge_all_old(struct timeval *now);
void dgram_timeout_min(struct timeval *result);
dgram_t *dgram_list_peek(int list);
void dgram_list_add(int list, dgram_t *dg);
dgram_t *dgram_list_pop(int list);
dgram_t *dgram_read(fd_t sfd);
int dgram_write(fd_t sfd, dgram_t *dg);
void dgram_free(dgram_t *dg);
dgram_t *dgram_create_keepalive(void);

#endif /* ULB_PROTO_DGRAM_H */
