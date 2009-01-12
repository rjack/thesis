#include "h/list.h"
#include "h/sim.h"


list_t ifaces_;
list_t paths_;


int
sim_init (void)
{
	/* TODO */
}


void
sim_path_create (const char *essid, int wifi_err, int wire_err, int wire_rtt)
{
	/* TODO */
}


void
sim_iface_create (const char *name, fw_type_t firmware_type)
{
	/* TODO */
}


int
sim_get_link_quality (const char *iface_name, const char *essid)
{
	/* TODO */
}


void
sim_set_link_quality (const char *iface_name, const char *essid, int quality)
{
	/* TODO */
}


void
sim_get_wire_err (const char *essid)
{
	/* TODO */
}


void
sim_set_wire_err (const char *essid, int err)
{
	/* TODO */
}


void
sim_exec_step (void)
{
	/* TODO */
}


#endif /* ULB_SIM_H */
