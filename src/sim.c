#include "h/list.h"
#include "h/sim.h"


/*******************************************************************************
			       Local variables
*******************************************************************************/

static list_t ifaces_;
static list_t paths_;


/*******************************************************************************
			      Exported functions
*******************************************************************************/

int
sim_init (void)
{
	/* TODO */
	return -1;
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
	return -1;
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
