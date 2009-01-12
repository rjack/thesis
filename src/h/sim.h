#ifndef ULB_SIM_H
#define ULB_SIM_H


/*******************************************************************************
				   Typedefs
*******************************************************************************/

/*
 * Firmware type.
 */
typedef     char             fw_type_t;
#define     FIRMWARE_ACK     ((fw_type_t)'a')
#define     FIRMWARE_NAK     ((fw_type_t)'n')


/*******************************************************************************
			     Function prototypes
*******************************************************************************/

int
sim_init (void);


void
sim_net_create (const char *essid, int wifi_err, int wire_err, int wire_rtt);
/*
 * Create a new net path.
 * wifi_err: inherent AP error percent (e.g. interference)
 * wire_err: wired net error percent
 * wire_delay: AP-Proxy round trip time.
 */


void
sim_iface_create (const char *name, fw_type_t firmware_type);


int
sim_get_link_quality (const char *iface_name, const char *essid);
/*
 * Get link quality between the interface with the given name and the access
 * point with the given essid.
 *
 * Value between 0 and 100.
 */


void
sim_set_link_quality (const char *iface_name, const char *essid, int quality);
/*
 * Set the link quality between the interface with the given name and the
 * access point with the given essid.
 *
 * Quality must be between 0 and 100.
 */


void
sim_get_wire_err (const char *essid);


void
sim_set_wire_err (const char *essid, int err);


void
sim_exec_step (void);


#endif /* ULB_SIM_H */
