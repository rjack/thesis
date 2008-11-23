#ifndef ULP_PROTO_TYPES_H
#define ULP_PROTO_TYPES_H

#define     MIN(a,b)     ((a) < (b) ? (a) : (b))
#define     MAX(a,b)     ((a) > (b) ? (a) : (b))

#define     ONE_MILLION     1000000

/* Per semplicita', max 16 interfacce wifi. */
#define     IFACE_MAX     16


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
 * Interfaccia.
 */
typedef struct {
	bool if_suspected;
	int if_id;
	char *if_iface;
	char *if_bind_ip;
	char *if_bind_port;
	struct pollfd if_pfd;
} iface_t;


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
typedef struct {
	int dg_id;                 /* id univoco indicato da sendmsg_getID */
	char *dg_data;             /* dati letti da recvmsg */
	size_t dg_datalen;         /* lunghezza dati */
	struct dgram *dg_next;     /* prossimo dgram in coda */
	timeout_t *dg_life_to;     /* tempo di vita del datagram */
	timeout_t *dg_retry_to;    /* timeout di ritrasmissione */
} dgram_t;


#endif /* ULP_PROTO_TYPES_H */
