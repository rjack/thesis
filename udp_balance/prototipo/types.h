#ifndef ULP_PROTO_TYPES_H
#define ULP_PROTO_TYPES_H

#define     MIN(a,b)     ((a) < (b) ? (a) : (b))

#define     ONE_MILLION     1000000


/*
 * Booleani.
 */
typedef int bool;

#ifdef FALSE
#  undef FALSE
#endif
#define FALSE ((bool)0)

#ifdef TRUE
#  undef TRUE
#endif
#define TRUE (!FALSE)


/*
 * File descriptor.
 */
typedef int fd_t;


/*
 * Canali.
 */
typedef struct {
	fd_t ch_sock;
	char *ch_iface;
	char *ch_bind_ip;
	char *ch_bind_port;
	bool ch_suspected;
} wifi_link_t;


/*
 * Cronometro.
 */
typedef struct {
	struct timeval cr_elapsed;
	struct timeval cr_start;
} crono_t;


/*
 * Timeout.
 */
typedef struct timeout_t {
	/* Durata del timeout e cronometro. */
	struct timeval to_maxval;
	crono_t to_crono;
} timeout_t;


/*
 * Datagram.
 */
struct dgram {
	timeout_t *dg_life_to;     /* tempo di vita del datagram */
	timeout_t *dg_retry_to;    /* timeout di ritrasmissione */
	char *dg_data;             /* dati letti da recvmsg */
	size_t dg_datalen;         /* lunghezza dati */
	struct dgram *dg_next;     /* prossimo dgram in coda */
};


#endif /* ULP_PROTO_TYPES_H */
