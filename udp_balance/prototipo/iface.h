#ifndef ULB_PROTO_IFACE_H
#define ULB_PROTO_IFACE_H

#include "types.h"

bool iface_must_send_keepalive(const iface_t *if_ptr);
dgram_t *iface_read(iface_t *if_ptr);
iface_t *iface_get_current(void);
iface_t *iface_iterator_get_first(iface_iterator_t *ii_ptr);
iface_t *iface_iterator_get_next(iface_iterator_t *ii_ptr);
int iface_get_events(iface_t *if_ptr);
int iface_handle_err(iface_t *if_ptr);
int iface_up(const char *name, const char *loc_ip);
ssize_t iface_write(iface_t *if_ptr, dgram_t *dg);
void iface_down(const char *name, const char *loc_ip);
void iface_fill_pollfd(struct pollfd *pfd, size_t *pfd_used);
void iface_init_module(void);
void iface_keepalive_left(iface_t *if_ptr, struct timeval *result);
void iface_print(iface_t *if_ptr);
void iface_read_pollfd(struct pollfd *pfd);
void iface_reset_events(iface_t *if_ptr);
void iface_set_events(iface_t *if_ptr, int e);

#endif /* ULB_PROTO_IFACE_H */
