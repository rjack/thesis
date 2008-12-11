#ifndef ULP_PROTO_TYPES_H
#define ULP_PROTO_TYPES_H

#include <poll.h>


/****************************************************************************
				     Tipi
****************************************************************************/

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


/****************************************************************************
				   Costanti
****************************************************************************/

#define     SP_LOC_IP       "127.0.0.1"
#define     SP_LOC_PORT     "5555"

/* FIXME in realta' l'IP del softphone e' quello da cui ricevo i datagram? */
#define     SP_REM_IP       "127.0.0.1"
#define     SP_REM_PORT     "5556"


#define     IM_LOC_IP       "127.0.0.1"
#define     IM_LOC_PORT     "6666"
#define     IM_REM_IP       "127.0.0.1"
#define     IM_REM_PORT     "6667"

/* l'indirizzo ip locale per le comunicazioni con il proxy e' quello
 * di ogni interfaccia di rete. */
#define     PX_LOC_PORT     "8888"
#define     PX_REM_IP       "127.0.0.1"
#define     PX_REM_PORT     "8889"

#define     ONE_MILLION     1000000

#define     CONTROLBUFLEN   1024

#define     MIN(a,b)        ((a) < (b) ? (a) : (b))
#define     MAX(a,b)        ((a) > (b) ? (a) : (b))

#define     ARRAYLEN(a)     (sizeof((a))/sizeof(*(a)))

#define     IP_NOTIFY       4242


/****************************************************************************
			      Variabili globali
****************************************************************************/

#ifdef ULP_PROTO_MAIN
#  define EXTERN_IF_NOT_MAIN
#else
#  define EXTERN_IF_NOT_MAIN extern
#endif

EXTERN_IF_NOT_MAIN struct timeval time_0ms;
EXTERN_IF_NOT_MAIN struct timeval time_30ms;
EXTERN_IF_NOT_MAIN struct timeval time_150ms;
EXTERN_IF_NOT_MAIN bool verbose;


#endif /* ULP_PROTO_TYPES_H */
