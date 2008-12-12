#ifndef ULB_PROTO_IFACE_ID_H
#define ULB_PROTO_IFACE_ID_H

/* Header a parte per evitare dipendenze circolari tra iface.h e dgram.h. */

/*
 * Identificativo interfaccia.
 */
#define     IFACE_ID_NAME_LEN         10
#define     IFACE_ID_LOC_IP_LEN       16
#define     IFACE_ID_LOC_PORT_LEN      6
struct iface_id {
	char ii_name[IFACE_ID_NAME_LEN];
	char ii_loc_ip[IFACE_ID_LOC_IP_LEN];
	char ii_loc_port[IFACE_ID_LOC_PORT_LEN];
};

#endif /* ULB_PROTO_IFACE_ID_H */
