/*******************************************************************************
			       Local variables
*******************************************************************************/

/* Dgram queues. Ordered by timestamp */
static list_t in_;
static list_t out_;
static list_t sent_;
static list_t unacked_;


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
	/* init code datagram: in out ifconf sent unacked */

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
	 * Disattivazione interfacce bad.
	 */
	// per ogni interfaccia
	// 	se interfaccia bad
	// 		rimuovi interfaccia
	// 		distruggi interfaccia


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
	 * Rimozione dgram scaduti.
	 */
	// per ogni dgram dentro a out, sent e unacked
	// 	se scaduto life timeout
	// 		remove from list
	// 		discard

	/*
	 * Travaso da unacked a out per i dgram che non hanno ricevuto l'ACK.
	 */
	// per ogni dgram dentro a unacked
	// 	se scaduto retry timeout
	// 		remove from unacked
	// 		inorder insert dentro a out

	/*
	 * Travaso da sent a out per i dgram che hanno ricevuto il NAK.
	 */
	// per ogni dgram dentro a sent
	// 	se ricevuto NAK associato al suo ID
	// 		remove from sent
	// 		inorder insert dentro a out


	/*
	 * Valutazione interfaccia migliore e assegnazione prossimo dgram da
	 * spedire.
	 */
	// best_iface = choose_best_iface ();
	// se out non e' vuota,
	// 	dg = dequeue out
	// 	iface_set_outgoing_dgram best_iface dg


	/*
	 * Impostazione probalive.
	 */
	// per ogni interfaccia
	// 	se scaduto probalive timeout e iface non deve spedire nulla
	// 		iface_set_outgoing_dgram new probalive


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
	// pm_poll


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
	// 		if !err
	// 			dgram enqueue coda in
	// 	se iface POLLOUT
	// 		iface deve avere dgram impostato in uscita
	// 		dgram = iface write
	// 		if err
	// 			iface set bad
	// 			continue
	// 		else if dgram non e' un probalive
	// 			if iface ACKosa
	// 				dgram set retry timeout
	// 				dgram enqueue coda unacked
	// 			else iface NAKosa
	// 				dgram enqueue coda sent
	// 		else e' un probalive
	// 			discard dgram
	// 	se iface POLLERR
	// 		iface handle err
	// 		se errore fatale
	// 			iface set bad
	// 		altrimenti se ack id
	// 			remove if ha lo stesso id da coda unacked
	// 			remove if ha lo stesso id da coda out
	//			discard tutti i dgram rimossi
	//		altrimenti e' un nak
	//			remove if ha lo stesso id da coda sent
	//			inorder insert dgram out

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
