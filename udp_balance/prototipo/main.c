/* udp_balancer: prototipo
 *
 * Che si deve fare?
 *
 * Da qui al mondo:
 * - ricevere dati da softphone
 * - capire se i dati sono SIP o RTP
 * - selezionare la porta d'uscita (simula interfaccia) migliore
 * - spedire i dati sulla porta scelta
 *
 * Dal mondo a qui:
 * - ricevere dati da tutte le interfacce
 * - mandarli al softphone
 *
 * Invarianti:
 * - i dati spediti si mantengono per ritrasmissioni
 *   - separare urgenti (RTP) da non urgenti (SIP)
 * - si rimuovono i dati RTP piu' vecchi di 150ms
 */
