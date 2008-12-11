#ifndef ULB_PROTO_DGRAM_H
#define ULB_PROTO_DGRAM_H

#include <sys/time.h>
#include <netinet/in.h>

#include "types.h"
#include "crono.h"
#include "iface_id.h"


/****************************************************************************
				     Tipi
****************************************************************************/

/*
 * Datagram.
 */
typedef struct dgram {
	int dg_id;                 /* id univoco indicato da sendmsg_getID */
	char *dg_data;             /* dati letti da recvmsg */
	size_t dg_datalen;         /* lunghezza dati */
	timeout_t *dg_life_to;     /* tempo di vita del datagram */
	timeout_t *dg_retry_to;    /* timeout di ritrasmissione */
	iface_id_t *dg_iface_id;   /* interfaccia di spedizione. */
} dgram_t;


/****************************************************************************
				  Prototipi
****************************************************************************/

int dgram_cmp_id(dgram_t *dg, int *id);
bool dgram_must_be_discarded(dgram_t *dg);
bool dgram_must_be_retransmitted(dgram_t *dg);
void dgram_destroy_reply_timeout(dgram_t *dg);
dgram_t *dgram_create(void);
void dgram_min_timeout(dgram_t *dg, struct timeval *min_result);
void dgram_set_life_timeout(dgram_t *dg);
dgram_t *dgram_read(fd_t sfd, struct sockaddr_in *src_addr_result, socklen_t *src_addr_result_len);
ssize_t dgram_write_getID(fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr, socklen_t rem_addr_len);
ssize_t dgram_write(fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr, socklen_t rem_addr_len);
void dgram_print(const dgram_t *dg);
void dgram_destroy(dgram_t *dg);
dgram_t *dgram_create_keepalive(void);

#endif /* ULB_PROTO_DGRAM_H */
