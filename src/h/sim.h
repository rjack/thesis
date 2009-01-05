#ifndef ULB_SIM_H
#define ULB_SIM_H


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

void
sim_init (fd_t sockfd);


void
sim_set_net (const char *essid, int wifi_err, int wire_err, int wire_delay);


void
sim_set_iface (const char *name);


int
sim_get_link_quality (const char *iface_name);


void
sim_set_link_quality (const char *iface_name, int quality);


void
sim_exec_step (void);


#endif /* ULB_SIM_H */
