#ifndef ULB_IFMON_H
#define ULB_IFMON_H

#include "dgram.h"


/*******************************************************************************
				   Defines
*******************************************************************************/

#define     IFMON_CMD_DOWN     0
#define     IFMON_CMD_UP       1


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

int
ifmon_init (void);


void
ifmon_set_events (void);


int
ifmon_get_revents (void);


int
ifmon_parse (char *pld, size_t pld_len, char **name_result, char **ip_result);


dgram_t *
ifmon_read (void);

#endif /* ULB_IFMON_H */
