#ifndef ULP_PROTO_TYPES_H
#define ULP_PROTO_TYPES_H

#include <poll.h>
#include <sys/time.h>


#define     SP_LOC_IP       "127.0.0.1"
#define     SP_LOC_PORT     "5555"

/* FIXME in realta' l'IP del softphone e' quello da cui ricevo i datagram? */
#define     SP_REM_IP       "127.0.0.1"
#define     SP_REM_PORT     "5556"


#define     IM_LOC_IP       "127.0.0.1"
#define     IM_LOC_PORT     "6666"
#define     IM_REM_IP       "127.0.0.1"
#define     IM_REM_PORT     "6667"

/* PX_LOC_IP e' quello dell'interfaccia di rete. */
#define     PX_LOC_PORT     "8888"
#define     PX_REM_IP       "127.0.0.1"
#define     PX_REM_PORT     "8889"



#define     ONE_MILLION     1000000

#define     IFACE_MAX       16

#define     DGRAM_INWARD    0
#define     DGRAM_OUTWARD   1

#define     MIN(a,b)     ((a) < (b) ? (a) : (b))
#define     MAX(a,b)     ((a) > (b) ? (a) : (b))

#define     ARRAYLEN(a)      (sizeof((a))/sizeof(*(a)))

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
typedef struct dgram {
	int dg_id;                 /* id univoco indicato da sendmsg_getID */
	char *dg_data;             /* dati letti da recvmsg */
	size_t dg_datalen;         /* lunghezza dati */
	timeout_t *dg_life_to;     /* tempo di vita del datagram */
	timeout_t *dg_retry_to;    /* timeout di ritrasmissione */
} dgram_t;


/*
 * Interfaccia.
 */
typedef struct {
	bool if_suspected;
	char *if_name;
	char *if_loc_ip;
	char *if_loc_port;
	struct pollfd if_pfd;
	timeout_t if_keepalive;
} iface_t;


/*
 * Liste.
 */
typedef struct list_node {
	void *n_ptr;
	struct list_node *n_next;
	struct list_node *n_prev;
} list_node_t;


typedef list_node_t * iface_iterator_t;


/****************************************************************************
			      Variabili globali
****************************************************************************/

#ifdef ULP_PROTO_MAIN
#  define EXTERN_IF_NOT_MAIN
#else
#  define EXTERN_IF_NOT_MAIN extern
#endif

EXTERN_IF_NOT_MAIN struct timeval now;
EXTERN_IF_NOT_MAIN struct timeval time_0ms;
EXTERN_IF_NOT_MAIN struct timeval time_30ms;
EXTERN_IF_NOT_MAIN struct timeval time_150ms;
EXTERN_IF_NOT_MAIN bool debug;


#endif /* ULP_PROTO_TYPES_H */
