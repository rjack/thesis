#ifndef ULB_PROTO_IFACE_H
#define ULB_PROTO_IFACE_H

void iface_init_module(void);
int iface_up(const char *name, const char *loc_ip, const char *loc_port);
void iface_down(const char *name, const char *loc_ip, const char *loc_port);
iface_t *iface_iterator_get_first(iface_iterator_t *ii_ptr);
iface_t *iface_iterator_get_next(iface_iterator_t *ii_ptr);
int iface_get_events(iface_t *iface);
bool iface_keepalive_left(iface_t *if_ptr, struct timeval *now, struct timeval *result);
void iface_set_events(iface_t *iface, int e);
void iface_reset_events(iface_t *iface);
iface_t *iface_get_current(void);
char *iface_to_string(iface_t *iface, char *str);
void iface_fill_pollfd(struct pollfd *pfd, size_t *pfd_used);
void iface_read_pollfd(struct pollfd *pfd);
int iface_write(iface_t *iface, dgram_t *dg);
dgram_t *iface_read(iface_t *iface);
int iface_handle_err(iface_t *iface);

#endif /* ULB_PROTO_IFACE_H */
