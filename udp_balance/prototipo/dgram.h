#ifndef ULB_PROTO_DGRAM_H
#define ULB_PROTO_DGRAM_H

#include <sys/time.h>
#include <netinet/in.h>

#include "types.h"
#include "crono.h"
#include "iface_type.h"


/****************************************************************************
				     Tipi
****************************************************************************/

/*
 * Datagram.
 */
typedef struct dgram {
	int dg_id;                      /* id indicato da sendmsg_getID */
	char *dg_data;                  /* dati letti da recvmsg */
	size_t dg_datalen;              /* lunghezza dati */
	timeout_t *dg_life_to;          /* tempo di vita del datagram */
	timeout_t *dg_retry_to;         /* timeout di ritrasmissione */
	iface_t *dg_if_ptr;             /* interfaccia di spedizione. */
	/* XXX quando si distrugge un'interfaccia, bisogna scorrere tutte le
	 * XXX liste di datagram per NULLare anche questo puntatore! */
} dgram_t;


/****************************************************************************
				  Prototipi
****************************************************************************/

bool dgram_has_id(dgram_t *dg, int *id);
bool dgram_must_discarded(dgram_t *dg);
bool dgram_must_retry(dgram_t *dg);
void dgram_destroy_reply_timeout(dgram_t *dg);
dgram_t *dgram_create(void);
void dgram_clear_iface_ptr (dgram_t *dg, iface_t *if_ptr);
void dgram_min_timeout(dgram_t *dg, struct timeval *min_result);
void dgram_set_life_timeout(dgram_t *dg);
dgram_t *dgram_read(fd_t sfd, struct sockaddr_in *src_addr_result, socklen_t *src_addr_result_len);
ssize_t dgram_write_getID(fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr, socklen_t rem_addr_len);
ssize_t dgram_write(fd_t sfd, dgram_t *dg, struct sockaddr_in *rem_addr, socklen_t rem_addr_len);
void dgram_print(const dgram_t *dg);
void dgram_destroy(dgram_t *dg);
dgram_t *dgram_create_keepalive(void);

#endif /* ULB_PROTO_DGRAM_H */
