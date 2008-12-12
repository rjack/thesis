#ifndef ULB_PROTO_TED_FAKE_H
#define ULB_PROTO_TED_FAKE_H

#include "types.h"


/****************************************************************************
				     Tipi
****************************************************************************/

/*
 * Struttura sock_notify_msg farlocca.
 */
struct sock_notify_msg {
	bool nm_ack;               /* TRUE -> ack; FALSE -> nak. */
	int nm_dgram_id;           /* dgram a cui la notifica si riferisce. */
	struct iface_id nm_iface_id;    /* if su cui e' stato spedito il dg. */
};


/****************************************************************************
				   Costanti
****************************************************************************/

#define     FAIL_PERCENT    30

#define     SENDMSG_FAKE_ERR     (-2)

/* TRUE simula un TED che conferma l'avvenuta ricezione da parte dell'AP,
 * FALSE simula un TED che segnala il fallimento della spedizione. */
#define     TED_FAKE_POSITIVE     TRUE


void ted_init(void);
ssize_t sendmsg_getID_fake(int sfd, const struct msghdr *msg, int flags, int *id_result);
void ted_set_acked(dgram_t *dg, bool acked);
void ted_run(list_t ifaces);

#endif /* ULB_PROTO_TED_FAKE_H */
