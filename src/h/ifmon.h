#ifndef ULB_IFMON_H
#define ULB_IFMON_H


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

void
ifmon_init (fd_t sockfd);


void
ifmon_set_net (const char *essid, int wifi_err, int wire_err, int wire_delay);


void
ifmon_set_iface (const char *name);


void
ifmon_set_movement (int movement_code);


void
ifmon_exec_step (void);


#endif /* ULB_IFMON_H */
