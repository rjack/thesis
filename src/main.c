/*******************************************************************************
			       Local variables
*******************************************************************************/

/* Dgram queues. Ordered by timestamp. */
static list_t in_;
static list_t out_;
static list_t ifconf_;


/*******************************************************************************
			       Local functions
*******************************************************************************/

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
	/* init code datagram: in out ifconf */

	/* init lista di interfacce di rete */
}


static void
init_modules (void)
{
	/* Per ora nessuno */
}


static bool
is_done (void)
{
	return FALSE;
}


static void
init_net (void)
{
	/* socket per phone */
	/* socket per interface manager */
}


static int
main_loop (void)
{
	/*
	 * Esecuzione messaggi interface monitor.
	 */
	// while !vuota coda ifconf
	// 	dgram dequeue ifconf
	// 	cmd = dgram get payload
	// 	ifmon do cmd


	/*
	 * Controllo timeout: lettura timeout minimo tra tutti quelli attivi.
	 */
	// gettime (&now);
	// nexp = tm_min_left_overall (&min, &now);
	// if (nexp == 0)
	// 	poll_timeout = -1;
	// else
	// 	poll_timeout = (int)(tv2d (&min, FALSE) * 1000);


	/*
	 * Controllo timeout scaduti dgram in uscita.
	 */
	// per ogni dgram dentro a out
	// 	se scaduto life timeout
	// 		remove from list
	// 		discard

	/*
	 * Controllo timeout scaduti interfacce.
	 */
	// per ogni interfaccia iface
	// 	iface iface_handle_timeouts


	/*
	 * Valutazione interfaccia migliore e assegnazione prossimo dgram da
	 * spedire.
	 */
	// best_iface = iface_get_best_overall ();
	// se best_iface && out non e' vuota,
	// 	dg = dequeue out
	// 	iface_load best_iface dg


	/*
	 * Poll.
	 */
	// pm_fd_zero ();
	//
	// softphone POLLIN | POLLERR
	// se !vuota coda in
	// 	softphone POLLOUT
	//
	// ifmon POLLIN | POLLERR
	//
	// per ogni interfaccia iface
	// 	iface POLLIN | POLLERR
	// 	se iface ha un dgram assegnato in uscita
	// 		iface POLLOUT
	// pm_poll (poll_timeout);


	/*
	 * Gestione eventi softphone.
	 */
	// se softphone POLLIN
	// 	read dgram from socket softphone
	// 	se !err
	// 		set life timeout dgram
	// 		inorder insert dgram coda out
	// se softphone POLLOUT
	// 	dequeue dgram da coda in
	// 	write dgram socket softphone
	// 	se !err
	// 		discard dgram
	// 	altrimenti
	// 		push dgram coda in
	// se softphone POLLERR
	// 	exit failure


	/*
	 * Gestione eventi interface monitor.
	 */
	// se ifmon POLLIN
	// 	read dgram
	// 	if !err
	// 		enqueue dgram ifconf
	// se ifmon POLLERR
	// 	exit failure


	/*
	 * Gestione eventi interfacce.
	 */
	// per ogni interfaccia iface
	// 	se iface POLLIN
	// 		iface read dgram
	// 		if !err && dgram
	//	 		dgram enqueue coda in
	// 	se iface POLLOUT
	// 		iface deve avere dgram impostato in uscita
	// 		erro = 0;
	// 		iface write
	// 		if err
	// 			dgram = iface set bad
	// 			if dgram
	// 				inorder insert dgram coda out
	// 			continue
	// 	se iface POLLERR
	// 		iface get err
	// 		se errore fatale
	// 			dgram = iface set bad
	// 			if dgram
	// 				inorder insert dgram coda out
	// 		altrimenti se ack id
	// 			remove if ha lo stesso id da coda out
	//			discard il dgram rimosso
	//			iface set ack id
	//		altrimenti e' un nak
	//			dgram = iface set nak id
	//			if dgram
	//				inorder insert dgram out

	return EXIT_FAILURE;
}


int
main (int argc, const char *argv[])
{
	get_cmd_line_options ();
	init_data_struct ();
	init_modules ();
	init_net ();

	while (!is_done ())
		main_loop ();

	return EXIT_SUCCESS;
}
