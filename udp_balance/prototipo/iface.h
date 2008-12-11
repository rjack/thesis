#ifndef ULB_PROTO_IFACE_H
#define ULB_PROTO_IFACE_H

#include <errno.h>

#include "crono.h"
#include "dgram.h"
#include "iface_id.h"
#include "list.h"
#include "types.h"


/*
 * Interfaccia.
 */
typedef struct {
	bool if_suspected;
	bool if_must_send_keepalive;
	struct pollfd if_pfd;
	timeout_t if_keepalive;
	iface_id_t if_id;
} iface_t;


/*
 * Codici errno ritornati da iface_handle_err.
 * XXX riutilizza errno non correlati al programma.
 */
#define     E_IFACE_FATAL      EISDIR
#define     E_IFACE_DG_ACK     ECHILD
#define     E_IFACE_DG_NAK     EDQUOT

int iface_cmp_id(iface_t *if_ptr, iface_id_t *id);
bool iface_must_send_keepalive(const iface_t *if_ptr);
iface_t *iface_create(const char *name, const char *loc_ip);
void iface_destroy(iface_t *if_ptr);
int iface_get_events(iface_t *if_ptr);
void iface_set_suspected(iface_t *if_ptr);
void iface_keepalive_left(iface_t *if_ptr, struct timeval *result);
void iface_set_events(iface_t *if_ptr, int e);
void iface_reset_events(iface_t *if_ptr);
void iface_print(iface_t *if_ptr);
struct pollfd *iface_get_pollfd(iface_t *if_ptr);
void iface_set_pollfd(iface_t *if_ptr, struct pollfd *pfd);
ssize_t iface_cmsg_write(iface_t *if_ptr, void *data, size_t data_len);
ssize_t iface_write(iface_t *if_ptr, dgram_t *dg);
dgram_t *iface_read(iface_t *if_ptr);
void iface_id_destroy(iface_id_t *if_id);
int iface_handle_err(iface_t *if_ptr);

#endif /* ULB_PROTO_IFACE_H */
