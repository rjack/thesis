static void
get_cmd_line_options (void)
{
	/*
	 * Tipo conversazione da simulare
	 * - parla molto ulb
	 * - parla molto proxy
	 * - mixed
	 */
}


static void
init_data_struct (void)
{
	/* init code datagram: in out ifconf sent unacked */

	/* init lista di interfacce di rete */
}


static void
init_modules (void)
{
	/* Per ora non serve */
}


static void
init_net (void)
{
	/* socket con phone */
	/* socket con interface manager */
}


static int
main_loop (void)
{
	return EXIT_FAILURE;
}


int
main (int argc, const char *argv[])
{
	get_cmd_line_options ();
	init_data_struct ();
	init_modules ();
	init_net ();

	return main_loop ();
}
