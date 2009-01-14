#ifndef ULB_DGRAM_H
#define ULB_DGRAM_H

#include <sys/types.h>

#include "to_mgr.h"


/*******************************************************************************
				   Typedefs
*******************************************************************************/

/*
 * Datagrams.
 */

typedef     char                    dgram_type_t;
typedef     unsigned int            dgram_id_t;
typedef     unsigned int            dgram_probalive_seq_t;

#define     DGRAM_TYPE_DATA         ((dgram_type_t)'d')
#define     DGRAM_TYPE_PROBALIVE    ((dgram_type_t)'p')


typedef struct {
	/* Data or probalive. */
	dgram_type_t dg_type;
	/* Assigned by sendmsg_getID */
	dgram_id_t dg_id;
	/* Carried data. */
	char *dg_payload;
	size_t dg_payload_len;
	/* Discarded when this expires (TIMEOUT_ERROR if unused). */
	timeout_t dg_life_to;
	/* Sent again when this expires (TIMEOUT_ERROR if unused). */
	timeout_t dg_retry_to;
} dgram_t;


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

char *
dgram_payload (dgram_t *dg);


size_t
dgram_payload_len (dgram_t *dg);


bool
dgram_life_expired (dgram_t *dg);


bool
dgram_retry_expired (dgram_t *dg);


bool
dgram_eq (dgram_t *dg_1, dgram_t *dg_2);


bool
dgram_eq_id (dgram_t *dg, dgram_id_t *id);


dgram_t *
dgram_create (dgram_type_t type);


void
dgram_destroy (dgram_t *dg);


int
dgram_set_id (dgram_t *dg, dgram_id_t *id);


int
dgram_set_payload (dgram_t *dg, char *pld, size_t pld_len);


int
dgram_set_life_timeout (dgram_t *dg);


int
dgram_set_retry_timeout (dgram_t *dg);


dgram_type_t
dgram_get_type (dgram_t *dg);


dgram_t *
dgram_read (fd_t sfd);


int
dgram_write (dgram_t *dg, fd_t sfd);

#endif /* ULB_DGRAM_H */
