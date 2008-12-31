#ifndef ULB_PROTO_IFACE_TYPE_H
#define ULB_PROTO_IFACE_TYPE_H

/* Header a parte per evitare dipendenze circolari tra dgram.h e iface.h */

#include "crono.h"
#include "types.h"


#define     IFACE_ID_NAME_LEN         10
#define     IFACE_ID_LOC_IP_LEN       16
#define     IFACE_ID_LOC_PORT_LEN      6

/*
 * Interfaccia.
 */
typedef struct {
	unsigned int if_id;              /* Identificativo. */
	char if_name[IFACE_ID_NAME_LEN];
	char if_loc_ip[IFACE_ID_LOC_IP_LEN];
	char if_loc_port[IFACE_ID_LOC_PORT_LEN];
	bool if_firmware_positive;       /* TRUE: ack quando AP riceve; FALSE:
	                                    nak quando AP non riceve. */
	bool if_must_send_keepalive;     /* TRUE: deve mandare un keepalive. */
	bool if_suspected;               /* TRUE: ifaccia scarsa. */
	struct pollfd if_pfd;
	timeout_t if_keepalive;
} iface_t;


#endif /* ULB_PROTO_IFACE_TYPE_H */
