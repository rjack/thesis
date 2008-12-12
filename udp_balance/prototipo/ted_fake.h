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


/****************************************************************************
				  Prototipi
****************************************************************************/

void ted_fake_init(void);
ssize_t sendmsg_getID_fake(int sfd, const struct msghdr *hdr, int flags, int *id_result);
void ted_fake_set_acked(dgram_t *dg, bool acked);
struct sock_notify_msg *ted_fake_get_notify(iface_t *if_ptr);
void ted_fake_set_errqueue_events(iface_t *if_ptr);

#endif /* ULB_PROTO_TED_FAKE_H */
