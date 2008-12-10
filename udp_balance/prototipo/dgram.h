#ifndef ULB_PROTO_DGRAM_H
#define ULB_PROTO_DGRAM_H

#include <sys/time.h>
#include <netinet/in.h>

#include "types.h"
#include "crono.h"

/*
 * Datagram.
 */
typedef struct dgram {
	int dg_id;                 /* id univoco indicato da sendmsg_getID */
	char *dg_data;             /* dati letti da recvmsg */
	size_t dg_datalen;         /* lunghezza dati */
	timeout_t *dg_life_to;     /* tempo di vita del datagram */
	timeout_t *dg_retry_to;    /* timeout di ritrasmissione */
	void *dg_info;
} dgram_t;


/* dgram.c */
bool dgram_must_be_discarded(dgram_t *dg, struct timeval *now);
bool dgram_must_be_retransmitted(dgram_t *dg, struct timeval *now);
dgram_t *dgram_create(void);
void dgram_discard(int id);
void dgram_outward(int id);
void dgram_outward_all_unacked(void);
void dgram_purge_all_old(void);
void dgram_min_timeout(dgram_t *dg, struct timeval *min_result);
dgram_t *dgram_read(fd_t sfd, struct sockaddr_in *src_addr_result, socklen_t *src_addr_result_len);
ssize_t dgram_write_getID(fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr, socklen_t rem_addr_len);
ssize_t dgram_write(fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr, socklen_t rem_addr_len);
void dgram_print(const dgram_t *dg);
void dgram_destroy(dgram_t *dg);
dgram_t *dgram_create_keepalive(void);

#endif /* ULB_PROTO_DGRAM_H */
