;;; Variabili globali.

(defparameter *log* t)
(defparameter *wifi-interfaces* (make-hash-table :test #'equal))
(defparameter *access-points* (make-hash-table :test #'equal))
(defparameter *events* ())
(defparameter *ulb* nil)
(defparameter *proxy* nil)
(defparameter *sendmsg-current-id* -1)
(defparameter *now* 0)
(defparameter *wmem* 2048) ;; dimensione dei socket-send-buffer

(defparameter *codec-bw* nil) ;; assegnato in fondo

(defparameter *ping-burst-length* 5)
(defparameter *ping-interval* 250) ;; millisecondi
(defparameter *proxy-source-expiring-time* (* 2 *ping-interval*))

;; TODO controllare
(defparameter *rtp-payload-min-size* 300)
(defparameter *rtp-payload-max-size* 700)

(defparameter *wpa-supplicant-error-rate-activation-threshold* 20
  "Se error-rate di un link wifi e' sotto questa soglia, l'interfaccia puo'
  venire attivata.")


;;; Accesso alle variabili globali

(defun add-events (&rest new-events)
  "Aggiunge i new-events agli *events* e riordina il tutto per istante di
  esecuzione.
  Ritorna gli eventi come multiple-values."
  (setf *events*
	(nconc *events* new-events))
  (sort *events* #'< :key #'exec-at)
  (values-list new-events))


(defun reschedule (event at)
  (setf *events*
	(remove event *events*))
  (setf (exec-at event) at)
  (add-events event))


(defun cancel (&rest events)
  (dolist (ev events)
    (setf *events*
	  (remove ev *events*))))


(defun access-point-by (id)
  (gethash id *access-points*))


(defun wifi-interface-by (id)
  (gethash id *wifi-interfaces*))


(defun wifi-interface-associated-with (ap)
  (loop for wi being the hash-values in *wifi-interfaces*
        do (if (eql (associated-ap wi) ap)
             (return wi))
        finally (return nil)))


;;; Utilita'


(defun random-between (a b)
  "Numero casuale x tale che a <= x <= b"
  (+ a (random (- (1+ b) a))))


(defun hash->list (ht)
  (loop for v being the hash-values in ht
	collecting v))


(defun percentp (n)
  (and (>= n 0) (<= n 100)))


(defun generate-sendmsg-id ()
  (format nil "sendmsg-id-~d" (incf *sendmsg-current-id*)))


(defun firmware-ack-p (fw)
  (or (equal fw "full")
      (equal fw "ack")))


(defun firmware-nak-p (fw)
  (or (equal fw "full")
      (equal fw "nak")))


;;; Macro

(defmacro new (name &rest body)
  "Per scrivere (new class) invece di (make-instance 'class)"
  `(make-instance ',name ,@body))


;;; Funzioni per mantenere coerenza nelle unita' di misura:
;;; temporali: tutte in millisecondi.
;;; di banda: tutte in bytes per millisecondi.

(defun msecs (ms)
  ms)


(defun secs (s)
  (* s (msecs 1000)))


(defun mins (m)
  (* m (secs 60)))


(defun bits-per-second (bps)
  (/ (/ bps 8) 1000))


(defun kilobits-per-second (kbps)
  (* kbps (bits-per-second (expt 10 3))))


(defun megabits-per-second (mbps)
  (* mbps (kilobits-per-second (expt 10 3))))


(defun transmission-delta-time (nbytes bandwidth)
  (floor (float (/ nbytes bandwidth))))


;;; Classe base degli oggetti simulati.

(defclass identified ()
  ((id
     :initarg :id
     ;; no initform, deve essere unbound
     :reader id
     :documentation "Identificativo dell'oggetto. Per ogni sottoclasse ha un
     diverso significato.")))


(defmethod add ((ht hash-table) (obj identified))
  "Aggiunge obj all'hash table ht usando il suo id come chiave."
  (setf (gethash (id obj) ht) obj))


;;; Pacchetti

;;; Queste classi rappresentano pacchetti veri, ovvero pacchetti che
;;; transitano sulla rete.

(defclass packet (identified)
  ((payload
     :initarg :payload
     :initform (error ":payload mancante")
     :accessor payload
     :documentation "Istanza di una sottoclasse di packet.")

   (overhead-size
     :initform (error "overhead-size non specificata dalla sottoclasse")
     :reader overhead-size
     :documentation "Dimensione dell'overhead del pacchetto, in byte.")))


(defclass data-packet (packet)
  ((payload
     :initarg :payload-size
     :initform (error ":payload-size mancante")
     :documentation "Dimensione del data-packet, in byte.")

   (overhead-size
     :initform 0)))


(defclass rtp-packet (packet)
  ((overhead-size
     :initform 12)))


(defclass udp-packet (packet)
  ((id
     :initarg :id
     :initform nil
     :accessor id
     :documentation "Campo ID nell'header UDP che sarebbe usato per la
     frammentazione dei datagram ma che viene sfruttato d sendmsg-getid")

   (addr
     :initarg :addr
     :initform nil
     :accessor addr
     :documentation "Il mittente del pacchetto.")

   (overhead-size
     :initform 8)))

(defmethod print-object ((pkt udp-packet) (s stream))
  (format s "[udp-packet id:~a addr:~a size:~a]" (id pkt) (addr pkt) (size pkt)))


(defclass ipv4-packet (packet)
  ((overhead-size
     :initform 20)))


(defclass wifi-frame (packet)
  ((overhead-size
     :initform 34)))


(defclass ping-packet (udp-packet)
  ((payload
     :initarg nil
     :initform nil)

   (score
     :initarg :score
     :initform (error ":score mancante")
     :reader score
     :documentation "Voto che ULB ha assegnato all'interfaccia che sta
     spedendo questo ping, da comunicare al Proxy Server.")

   (sequence-number
     :initarg :sequence-number
     :initform (error ":sequence-number mancante")
     :reader sequence-number
     :documentation "Numero di sequenza del ping. Nella realta' e' contenuto
     nel payload del pacchetto UDP.")))


(defmethod print-object ((ping ping-packet) (s stream))
  (format s "[ping-packet id:~a addr:~a score:~a sequence-number:~a]"
	  (id ping) (addr ping) (score ping) (sequence-number ping)))


(defun make-udp-packet-list (duration)
  (let ((nbytes (* duration *codec-bw*)))
    (labels ((next-packet-size ()
		(random-between *rtp-payload-min-size*
				*rtp-payload-max-size*))
	     (helper (nbytes-left packet-size ls-acc)
		(if (>= packet-size nbytes-left)
		  (cons (new udp-packet
			     :payload (new data-packet
					   :payload-size nbytes-left))
			ls-acc)
		  (helper (- nbytes-left packet-size)
			  (next-packet-size)
			  (cons (new udp-packet
				     :payload (new data-packet
						   :payload-size packet-size))
				ls-acc)))))
      (helper nbytes (next-packet-size) nil))))



;;; Queste classi sono la rappresentazione software dei pacchetti da parte dei
;;; programmi.

(defclass ulb-struct-datagram (identified)
  ((id
     :initarg nil
     :initform nil
     :accessor id
     :documentation "assegnato da sendmsg-getid")

   (end-of-life-event
     :accessor end-of-life-event
     :documentation "Riferimento all'evento che rimuove questo pacchetto
     dall'ULB")

   (send-again-event
     :initarg :send-again-event
     :initform (error ":send-again-event mancante")
     :accessor send-again-event
     :documentation "Riferimento all'evento che pone questo pacchetto
     nuovamente nella coda di spedizione dall'ULB")

   (data
     :initarg :data
     :initform (error ":data mancante")
     :reader data
     :documentation "Riferimento al pacchetto vero e proprio.")))


(defmethod print-object ((struct ulb-struct-datagram) (s stream))
  (format s "[ulb-struct-datagram id:~a]" (id struct)))


(defclass ulb-struct-ping (ulb-struct-datagram)
  ;; Lo slot data punta a un istanza ping-packet che contiene numero di
  ;; sequenza e voto.

  ((end-of-life-event
     :initarg nil
     :initform nil)

   (send-again-event
     :initarg nil
     :initform nil)

   (data
     :initarg nil
     :initform nil)))


(defmethod print-object ((struct ulb-struct-ping) (s stream))
  (format s "[ulb-struct-ping id:~a data:~a]" (id struct) (data struct)))


(defmethod initialize-instance :after ((dgram ulb-struct-datagram) &key)
  (when (not (slot-boundp dgram 'end-of-life-event))
    (setf (end-of-life-event dgram)
	  (add-events (new event :exec-at (+ *now* (msecs 150))
			         :action (lambda ()
					   (purge *ulb* (id dgram))))))))


(defmethod initialize-instance :after ((ping ulb-struct-ping) &key wifi-interface)
  (let ((seq (incf (current-ping-seqnum wifi-interface))))
    (setf (slot-value ping 'data)
          (new ping-packet :score (score wifi-interface)
                           :sequence-number seq))))


;; Metodi sui pacchetti

(defmethod size ((pkt packet))
  (+ (overhead-size pkt)
     (size (payload pkt))))


(defmethod size ((ping ping-packet))
  (+ (overhead-size ping)
     8))    ;; contiene score e seqnum, due interi da 4 byte.


(defmethod size ((data data-packet))
  (payload data))


;;; Classi per l'Udp Load Balancer

(defclass first-hop-outcome ()
  ((dgram-id
     :initarg :dgram-id
     :initform (error ":dgram-id mancante")
     :reader dgram-id
     :documentation "ID del datagram, sarebbe assegnato da sendmsg-getid")

   (timestamp
     :initarg :timestamp
     :initform (error ":timestamp mancante")
     :reader timestamp
     :documentation "L'istante di creazione di questo first-hop-outcome")

   (value
     :initarg :value
     :initform (error ":value mancante")
     :reader value
     :documentation "Valore dell'outcome: ack oppure nak")))
   ;; TODO ack se TED dice ack oppure se ulb sa che ifaccia dice solo NAK ed
   ;; e' scaduto un timeout.
   ;; TODO nak se TED dice nak oppure se ulb sa che ifaccia dice solo ACK ed
   ;; e' scaduto un timeout.


(defmethod print-object ((fho first-hop-outcome) (s stream))
  (format s "[first-hop-outcome dgram-id:~a timestamp:~a value:~a]"
	  (dgram-id fho) (timestamp fho) (value fho)))


(defclass full-path-outcome ()
  ((sequence-number
     :initarg :sequence-number
     :initform (error ":sequence-number mancante")
     :reader sequence-number
     :documentation "Numero di sequenza del ping a cui si riferisce questo
     full-path-outcome.")

   (ping-sent-at
     :initform *now*
     :reader ping-sent-at
     :documentation "Istante di spedizione del ping.")

   (ping-recv-at
     :accessor ping-recv-at
     :documentation "Istante di ricezione del ping di risposta.")))


(defmethod print-object ((fpo full-path-outcome) (s stream))
  (format s "[full-path-outcome sequence-number:~a ping-sent-at:~a ping-recv-at:~a]"
	  (sequence-number fpo) (ping-sent-at fpo) (ping-recv-at fpo)))


(defclass ulb-wifi-interface (identified)
  ;; id rappresenta il nome dell'interfaccia.

  ((wifi-interface
     :initarg :wifi-interface
     :initform (error "wifi-interface mancante")
     :reader wifi-interface
     :documentation "Riferimento all'interfaccia wifi.")

   (firmware-detected
     :initform nil
     :accessor firmware-detected
     :documentation "ack, nak oppure full. Indica cio' che ULB ha dedotto
     del firmware della scheda, osservando le notifiche e i ping ricevuti.")

   (sent-datagrams
     :initform ()
     :accessor sent-datagrams
     :documentation "Gli ulb-struct-datagram (ping compresi) spediti da
     un'interfaccia vengono accodati qui in attesa di un ACK o di un
     end-of-life-event che li scarti, oppure di un NAK o di un
     send-again-event che li ritrasmetta.")

   (send-ping-event
     :initform nil
     :accessor send-ping-event
     :documentation "Riferimento all'evento che spedira' il prossimo ping su
     questa interfaccia.")

   (current-ping-seqnum
     :initform -1
     :accessor current-ping-seqnum
     :documentation "Numero di sequenza dell'ultimo ping spedito su questa
     interfaccia.")

   (score
     :initform 0
     :accessor score
     :documentation "Voto dell'interfaccia")

   (first-hop-log
     :initform nil
     :accessor first-hop-log
     :documentation "Lista di first-hop-outcome.")

   (full-path-log
     :initform nil
     :accessor full-path-log
     :documentation "Lista di full-path-outcome.")))


(defmethod print-object ((uwi ulb-wifi-interface) (s stream))
  (format s "[ulb-wifi-interface id:~a fw-det:~a current-ping-seqnum:~a score:~a]"
	  (id uwi) (firmware-detected uwi) (current-ping-seqnum uwi) (score uwi)))


(defmethod weighted-value ((fho first-hop-outcome))
    (let ((time-ago (- *now* (timestamp fho)))
	  (value (if (equal "ack" (value fho))
		   1
		   -1)))
      (* value
	 (cond
	   ((< time-ago 25) 1)
	   ((< time-ago 50) (/ 8 10))
	   ((< time-ago 100) (/ 6 10))
	   ((< time-ago 200) (/ 4 10))
	   ((< time-ago 400) (/ 2 10))
	   (t 0)))))


(defmethod weight ((fpo full-path-outcome))
  (let ((delta (- *now* (ping-sent-at fpo))))
    (cond ((< delta 250) 1)
	  ((< delta 500) (/ 1 2))
	  ((< delta 750) (/ 1 4))
	  (t 0))))


(defmethod compute-score ((uwi ulb-wifi-interface))
  ;;(inspect uwi)
  (labels ((first-hop-score (outcomes)
              (reduce #'+ (mapcar #'weighted-value outcomes)))
	   (full-path-score (outcomes)
              (let ((forgive t)
		    (result-score 0))
		(dolist (fpo outcomes)
		  (incf result-score
			(cond
			  ((slot-boundp fpo 'ping-recv-at) (setf forgive nil)
							   (weight fpo))
			  (t (if forgive
			       0
			       (- (weight fpo)))))))
		result-score)))
    (+ (* (/ 6 10) (first-hop-score (first-hop-log uwi)))
       (* (/ 4 10) (full-path-score (full-path-log uwi))))))


(defmethod nak-firmware-detected ((uwi ulb-wifi-interface))
  "Quando ulb riceve un nak, ricorda che il firmware può spedire nak"
  (with-accessors ((fw firmware-detected)) uwi
    (cond ((null fw) (setf fw "nak"))
	  ((equal "ack" fw) (setf fw "full"))
	  (t nil))))


(defmethod ack-firmware-detected ((uwi ulb-wifi-interface))
  "Quando ulb riceve un ack, ricorda che il firmware può spedire ack"
  (with-accessors ((fw firmware-detected)) uwi
    (cond ((null fw) (setf fw "ack"))
	  ((equal "nak" fw) (setf fw "full"))
	  (t nil))))


(defclass udp-load-balancer ()
  ((active-wifi-interfaces
     :initform (make-hash-table :test #'equal)
     :accessor active-wifi-interfaces
     :documentation "Hash table di riferimenti a istanze di
     ulb-wifi-interface, con gli id delle interfacce come chiavi.")

   (outgoing-datagrams
     :initform ()
     :accessor outgoing-datagrams
     :documentation "Coda di ulb-struct-datagram (no ping!) da spedire
     sull'interfaccia con voto migliore.")

   (urgent-datagrams
     :initform ()
     :accessor urgent-datagrams
     :documentation "Idem come outgoing-datagrams, ma ospita i datagram da
     rispedire")))


(defmethod best-interface ((ulb udp-load-balancer))
  (with-accessors ((ifaces active-wifi-interfaces)) ulb
    (let ((lst (hash->list ifaces)))
      (dolist (iface lst)
	(setf (score iface)
	      (compute-score iface)))
      (find (reduce #'max
		    lst
		    :key #'score)
	    lst
	    :key #'score))))


(defmethod purge ((ulb udp-load-balancer) (id string))
  "Cerca il datagram con il dato id in tutte le strutture dati di ulb e lo rimuove."
  (error "TODO purge id ~a" id))


;;; Net link

(defclass net-link (identified)
  ;; Id rappresenta l'id della destinazione

  ((delay
     :accessor delay
     :documentation "Latenza di questo link, cioe' rtt / 2.")

   (bandwidth
     :accessor bandwidth
     :documentation "Banda di questo link.")

   (error-rate
     :accessor error-rate
     :documentation "Percentuale d'errore su questo link, da 0 a 100")))


;;; Access point

(defclass access-point (identified)
  ;; Id rappresenta l'essid dell'access point

  ((net-links
     :initform (make-hash-table :test #'equal)
     :reader net-links
     :documentation "Hash table di net-link, con id della destinazione come
     chiave.")))


(defmethod print-object ((ap access-point) (s stream))
  (format s "[access-point id:~a]" (id ap)))


(defmethod wpa-supplicant-would-activate ((nl net-link) (ap access-point))
  (and (bandwidth nl)
       (> (bandwidth nl) 0)
       (< (error-rate nl)
          *wpa-supplicant-error-rate-activation-threshold*)))


;;; Interfaccia wireless

(defclass wifi-interface (identified)
  ;; id rappresenta il nome dell'interfaccia

  ((firmware
     :initarg :firmware
     :initform (error ":firmware mancante")
     :reader firmware
     :documentation "ack, nak oppure full. Rappresenta le effettive
     capacita' del firmware della sheda wireless.")

   (socket-send-buffer
     :initform nil
     :accessor socket-send-buffer
     :documentation "Lista di udp-packet spediti dall'ulb su questa
     interfaccia.")

   (associated-ap
     :initform nil
     :accessor associated-ap
     :documentation "Riferimento all'access-point associato, nil se
     l'interfaccia e' inattiva.")))


(defmethod print-object ((wi wifi-interface) (s stream))
  (format s "[wifi-interface id:~a fw:~a associated-ap:~a]"
	  (id wi) (firmware wi) (associated-ap wi)))


(defmethod notify-nak ((uwi ulb-wifi-interface) (pkt udp-packet))
  (format *log* "~&notify-nak ~a ~a" uwi pkt)
  ;; Ora sappiamo che interfaccia riceve nak: annotiamolo.
  (nak-firmware-detected uwi)
  ;; Recupera datagram.
  (let ((struct-dgram (find (id pkt) (sent-datagrams uwi) :key #'id)))
    ;; Rimuovilo dalla lista di quelli spediti.
    (setf (sent-datagrams uwi)
	  (remove (id pkt) (sent-datagrams uwi) :key #'id))
    ;; Se non e' un ping va rispedito.
    (if (not (typep struct-dgram 'ulb-struct-ping))
      (setf (urgent-datagrams *ulb*)
	    (append (urgent-datagrams *ulb*) struct-dgram)))
    ;; Log dell'accaduto.
    (add uwi
	 (new first-hop-outcome :dgram-id (id struct-dgram)
	                        :timestamp *now*
				:value "nak"))))

(defmethod notify-ack ((uwi ulb-wifi-interface) (pkt udp-packet))
  (format *log* "~&notify-ack ~a ~a" uwi pkt)
  ;; Segnamo che interfaccia riceve ack
  (ack-firmware-detected uwi)
  ;; Recupera datagram.
  (let ((struct-dgram (find (id pkt) (sent-datagrams uwi) :key #'id)))
    ;; Rimuovilo dalla lista di quelli spediti
    (setf (sent-datagrams uwi)
	  (remove (id pkt) (sent-datagrams uwi) :key #'id))
    (add uwi
	 (new first-hop-outcome :dgram-id (id struct-dgram)
	                        :timestamp *now*
				:value "ack"))))


(defmethod add ((wi wifi-interface) (pkt udp-packet))
  (with-accessors ((buf socket-send-buffer)) wi
    (setf buf
	  (nconc buf (list pkt)))
    (when (= 1 (length buf))
        (flush wi))))


(defmethod add ((ulb udp-load-balancer) (pkt udp-packet))
  "Crea uno ulb-struct-datagram e lo aggiunge a outgoing-datagrams"
  (with-accessors ((outgoing outgoing-datagrams)
		   (urgent urgent-datagrams)) ulb
    (let ((struct (new ulb-struct-datagram :send-again-event nil :data pkt))
	  (queued (+ (length outgoing)
		     (length urgent))))
      (setf outgoing
	    (nconc outgoing (list struct)))
      (when (= 1 queued)
	(flush ulb)))))


(defmethod add ((uwi ulb-wifi-interface) (fpo full-path-outcome))
  "Aggiunge un full-path-outcome al full-path-log dell'ulb-wifi-interface."
  (with-accessors ((fp-log full-path-log)) uwi
    (push fpo fp-log)
    ;; Controlli di coerenza
    (assert (apply #'>= (mapcar #'sequence-number fp-log))
	    nil "full-path-log non e' ordinato per numero di sequenza dei ping!")
    (assert (apply #'>= (mapcar #'ping-sent-at fp-log))
	    nil "full-path-log non e' ordinato per istante di invio dei ping!")))


(defmethod add ((uwi ulb-wifi-interface) (fio first-hop-outcome))
  "Aggiunge un first-hop-outcome al first-hop-log dell'ulb-wifi-interface"
  (with-accessors ((fh-log first-hop-log)) uwi
    (setf fh-log
	  (nconc fh-log (list fio)))))


;;; Proxy server

(defclass proxy-source (identified)
  ((pings-received
     :initform ()
     :accessor pings-received
     :documentation "Lista dei ping ricevuti")

   (score
     :initform 0
     :accessor score
     :documentation "Valutazione.")

   (expire-event
     :reader expire-event
     :documentation "Riferimento all'evento che disattiva questa sorgente")

   (last-datagram-at
     :initform nil
     :accessor last-datagram-at
     :documentation "Istante in cui e' stato ricevuto l'ultimo dgram dati")))


(defmethod print-object ((ps proxy-source) (s stream))
  (format s "[proxy-source id:~a score:~a last-datagram-at:~a]"
	  (id ps) (score ps) (last-datagram-at ps)))


(defmethod initialize-instance :after ((ps proxy-source) &key)
  (format *log* "~&new proxy-source ~a" ps)
  (setf (slot-value ps 'expire-event)
	(new event :exec-at (+ *now* *proxy-source-expiring-time*)
	           :action (lambda ()
			     (remhash (id ps) (active-sources *proxy*))))))


(defmethod add ((src proxy-source) (ping ping-packet))
  "Aggiunge ping alla lista dei ping."
  (with-accessors ((pr pings-received)) src
    (push ping pr)
    (sort pr #'< :key #'sequence-number)))


(defclass proxy-server (identified)
  ((id
     :initform "proxy")

   (active-sources
     :initform (make-hash-table :test #'equal)
     :accessor active-sources)

   (outgoing-datagrams
     :initform ())

   (incoming-datagrams
     :initform ())))


(defmethod print-object ((ps proxy-server) (s stream))
  (format s "[proxy-server id:~a]" (id ps)))


(defmethod active-source ((px proxy-server) (id string))
  (multiple-value-bind (src src-present-p) (gethash id (active-sources px))
    (if src-present-p
      (progn
	(reschedule (expire-event src) (+ *now* *proxy-source-expiring-time*))
	src)
      (add (active-sources px) (new proxy-source :id id)))))


;;; Metodi deliver: ritornano il tempo impiegato ad INVIARE il pacchetto
;;; (istante invio primo bit - istante invio ultimo bit)

(defmethod deliver ((pkt udp-packet) ;; TODO controllare che sia un udp-packet
		    (wi wifi-interface) (link net-link) (ap access-point))
  "Da wifi-interface ad access-point"
  (let* ((send-delta-time (transmission-delta-time (size pkt) (bandwidth link)))
	 (arrival-time (+ *now* send-delta-time (delay link)))
	 (success-p (> (random 101) (error-rate link))))
    (when success-p
      (format *log* "~&deliver success ~a ~a ~a" pkt wi ap)
      ;; access-point riceve
      (add-events (new event :exec-at arrival-time
		             :action (lambda ()
				       (recv pkt ap wi))))
      (when (firmware-ack-p (firmware wi))
	;; Se il firmware notifica gli ack, l'ulb riceve una notifica
	;; sull'interfaccia quando l'ACK a livello MAC torna indietro dopo che
	;; l'access point ha ricevuto il pacchetto.
	(add-events
	  (new event :exec-at (+ (delay link) arrival-time)
	             :action (lambda ()
			       (notify-ack (gethash (id wi)
						    (active-wifi-interfaces *ulb*))
					   pkt))))))
    (when (not success-p)
      (format *log* "~&deliver fail ~a ~a ~a" pkt wi ap)
      (when (firmware-nak-p (firmware wi))
	(add-events
	  (new event :exec-at (+ (delay link) arrival-time)
	             :action (lambda ()
			       (notify-nak (gethash (id wi)
						    (active-wifi-interfaces *ulb*))
					   pkt))))))
    send-delta-time))


(defmethod deliver ((pkt packet)
		    (ap access-point) (link net-link) (wi wifi-interface))
  "Da access-point a wifi-interface"
  (let* ((send-delta-time (transmission-delta-time (size pkt) (bandwidth link)))
	 (arrival-time (+ *now* send-delta-time (delay link)))
	 (success-p (> (random 101) (error-rate link))))
    (if success-p
      (progn
	(format *log* "~&deliver success ~a ~a ~a" pkt ap wi)
	(add-events
	  (new event :exec-at arrival-time
	             :action (lambda ()
			       (recv pkt wi ap)))))
      (format *log* "~&deliver fail ~a ~a ~a" pkt ap wi))
    send-delta-time))


(defmethod deliver ((pkt packet)
		    (ap access-point) (link net-link) (px proxy-server))
  "Da access-point a proxy-server"
  (let* ((send-delta-time (transmission-delta-time (size pkt) (bandwidth link)))
	 (arrival-time (+ *now* send-delta-time (delay link)))
	 (success-p (> (random 101) (error-rate link))))
    (if success-p
      (progn
	(format *log* "~&deliver success ~a ~a ~a" pkt ap px)
	(add-events
	  (new event :exec-at arrival-time
	             :action (lambda ()
			       (recv pkt px ap)))))
      (format *log* "~&deliver fail ~a ~a ~a" pkt ap px))
    send-delta-time))


(defmethod deliver ((pkt packet)
		    (px proxy-server) (link net-link) (ap access-point))
  "Da proxy-server ad access-point"
  (let* ((send-delta-time (transmission-delta-time (size pkt) (bandwidth link)))
	 (arrival-time (+ *now* send-delta-time (delay link)))
	 (success-p (> (random 101) (error-rate link))))
    (if success-p
      (progn
	(format *log* "~&deliver success ~a ~a ~a" pkt px ap)
	(add-events
	  (new event :exec-at arrival-time
	             :action (lambda ()
			       (recv pkt ap px)))))
      (format *log* "~&deliver fail ~a ~a ~a" pkt px ap))
    send-delta-time))


(defmethod sendmsg-getid ((wi wifi-interface) (pkt udp-packet))
  (let ((sym (generate-sendmsg-id)))
    (setf (id pkt) sym)
    (setf (addr pkt) (id wi))
    (add wi pkt)
    sym))


(defmethod send ((uwi ulb-wifi-interface) (struct ulb-struct-datagram))
  (setf (id struct)
	(sendmsg-getid (wifi-interface uwi) (data struct)))
  (push struct (sent-datagrams uwi)))


(defmethod send ((uwi ulb-wifi-interface) (struct ulb-struct-ping))
  "ULB spedisce un ping"
  ;; sendmsg-getid
  (setf (id struct)
        (sendmsg-getid (wifi-interface uwi) (data struct)))
  ;; registra spedizione ping
  (add uwi (new full-path-outcome
		:sequence-number (sequence-number (data struct))))
  ;; aggiunge il ping tra i datagram spediti
  (push struct (sent-datagrams uwi))
  ;; impostazione prossimo ping
  (add-events
    (setf (send-ping-event uwi)
	  (new event :exec-at (if (< (1+ (current-ping-seqnum uwi))
				     *ping-burst-length*)
				*now*
				(+ *now* *ping-interval*))
	             :action #'(lambda ()
				 (send uwi (new ulb-struct-ping :wifi-interface uwi)))))))


(defmethod recv ((pkt udp-packet) (ap access-point) (wi wifi-interface))
  "Access point riceve un pacchetto da interfaccia wifi e lo spedisce al proxy."
  (format *log* "~&recv ~a ~a ~a" pkt ap wi)
  (deliver pkt ap (link-between ap *proxy*) *proxy*))


(defmethod recv ((pkt udp-packet) (ap access-point) (px proxy-server))
  "Access point riceve pacchetto da proxy e lo inoltra all'interfaccia."
  (format *log* "~&recv ~a ~a ~a" pkt ap px)
  (let ((dest-wi (wifi-interface-by (addr pkt))))
    (deliver pkt ap (link-between ap dest-wi) dest-wi)))


(defmethod recv ((pkt udp-packet) (px proxy-server) (ap access-point))
  "Proxy riceve un datagram"
  (format *log* "recv ~a ~a ~a" pkt px ap)
  (let ((src (active-source px (addr pkt))))
    (setf (last-datagram-at src) *now*)))
;; NB: da qui bisognerebbe spedire al softphone remoto, ma non è necessario ai
;; fini della simulazione.


(defmethod recv ((ping ping-packet) (px proxy-server) (ap access-point))
  "Proxy riceve un ping"
  (format *log* "recv ~a ~a ~a" ping px ap)
  (let ((src (active-source px (addr ping))))
    (add src ping)
    (deliver (new ping-packet :id (id ping)
                              :addr (addr ping)
		              :sequence-number (sequence-number ping)
		              :score nil)  ;; non necessario
	     px (link-between ap px) ap)))


(defmethod recv ((pkt udp-packet) (wi wifi-interface) (ap access-point))
  "Interfaccia wifi riceve un pacchetto udp"
  (multiple-value-bind (uwi uwi-present-p) (gethash (id wi) (active-wifi-interfaces *ulb*))
    (if uwi-present-p
      (recv pkt uwi ap)
      (format *log* "recv fail ~a ~a ~a: ulb interface not active"
	      pkt wi ap))))


(defmethod recv ((pkt udp-packet) (uwi ulb-wifi-interface) (ap access-point))
  "Interfaccia ulb riceve pacchetto udp"
  (error "TODO recv pkt uwi ap"))


(defmethod recv ((ping ping-packet) (uwi ulb-wifi-interface) (ap access-point))
  "Interfaccia ulb riceve ping di risposta."
  ;; Manutenzione full-path-log: imposta ping-recv-at in full-path-outcome
  (let ((outcome (find (sequence-number ping)
		       (full-path-log uwi)
		       :key #'sequence-number)))
    (assert (not (null outcome)) nil
	    "recv ping uwi ap: outcome non trovato!")
    (assert (not (slot-boundp outcome 'ping-recv-at)) nil
	    "recv ping-recv-at uwi ap: ping-recv-at bound!")
    (setf (ping-recv-at outcome) *now*))

  (format *log* "recv ~a ~a ~a" ping uwi ap)

  ;; controlla firmware detected
  (with-accessors ((fw firmware-detected)) uwi
    (labels ((ping-by-seqnum-p (struct)
                 (and (typep struct 'ulb-struct-ping)
		      (equal (sequence-number (data struct))
			     (sequence-number ping)))))
      (let ((ping-sent (find-if #'ping-by-seqnum-p (sent-datagrams uwi))))
	(setf (sent-datagrams uwi)
	      (remove-if #'ping-by-seqnum-p (sent-datagrams uwi)))
	(cond
	  ((firmware-ack-p fw)
	   (assert (null ping-sent) nil
		   "Ping ha ricevuto risposta, firmware rilevato come ACK ma
		   ping-sent ancora tra i sent-datagrams!"))

	  ((equal "nak" fw)
	   (assert (not (null ping-sent)) nil
		   "Ping ha ricevuto risposta, firmware rilevato come NAK ma
		   ping-sent non trovato tra i sent-datagrams!"))

	  (t (assert (null fw) nil "Valore firmware-detected incasinato!")
	     (when (not (null ping-sent))
	       (setf fw "nak"))))))))


(defmethod activate ((ulb udp-load-balancer) (wi wifi-interface))
  (let* ((id (id wi))
         (uwi (setf (gethash id (active-wifi-interfaces ulb))
                    (new ulb-wifi-interface :id id :wifi-interface wi))))
    (send uwi
          (new ulb-struct-ping :wifi-interface uwi))))


(defmethod deactivate ((ulb udp-load-balancer) (wi wifi-interface))
  (let ((id (id wi)))
    ;; TODO dirottare pacchetti
    ;; TODO cancellare eventi pendenti
    (remhash id (active-wifi-interfaces ulb))))


(defmethod link-between ((ap access-point) (wi wifi-interface))
  (gethash (id wi) (net-links ap)))


(defmethod link-between ((ap access-point) (px proxy-server))
  (gethash "proxy" (net-links ap)))


;;; Evento

(defclass event ()
  ((exec-at
     :initarg :exec-at
     :initform (error ":exec-at missing")
     :accessor exec-at
     :documentation "Istante in cui deve essere eseguito.")

   (action
     :initarg :action
     :initform (error ":action mancante")
     :reader action
     :documentation "Funzione da chiamare per eseguire l'evento.")))


(defmethod flush ((wi wifi-interface))
  (format *log* "~&flush ~a" wi)
  (with-accessors ((buf socket-send-buffer)) wi
    (when buf
      (let* ((pkt (first buf))
	     (ap (associated-ap wi))
	     (link (link-between ap wi))
	     ;; deliver del pacchetto
	     (delta-time (deliver pkt wi link ap)))
	(add-events (new event :exec-at (+ *now* delta-time)
			       :action (lambda ()
					 (pop buf)
					 (flush *ulb*)
					 (flush wi))))))))


(defmethod flush ((ulb udp-load-balancer))
  "Dalle code di datagram alla spedizione sulla best interface"
  (with-accessors ((urgent urgent-datagrams)
		   (outgoing outgoing-datagrams)) ulb
    (when (not (and (null urgent)
		    (null outgoing)))
      (let ((cur-pkg-size (size
			    (data
			      (first
				(if (not (null urgent))
					       urgent
					       outgoing)))))
	    (best (best-interface *ulb*)))
	(when (>= (available-send-buffer best)
		  cur-pkg-size)
	  (send best (if (not (null urgent))
		       (pop urgent)
		       (pop outgoing))))))))


(defmethod available-send-buffer ((uwi ulb-wifi-interface))
  "NB: puo' essere negativo, i ping vengono aggiunti anche se non ci sarebbe spazio."
  (let ((used (reduce #'+ (mapcar #'size (socket-send-buffer
					   (wifi-interface uwi))))))
    (- *wmem* used)))


(defmethod fire ((ev event))
    (funcall (action ev)))


(defmethod set-link ((ap access-point) dest
                     &key (error-rate nil error-rate-provided-p)
                          (delay nil delay-provided-p)
                          (bandwidth nil bandwidth-provided-p))
  "Simula un cambiamento di delay, error-rate o bandwidth nel link
   dall'access-point con l'essid specificato a to. Come conseguenza, una
   interfaccia wireless non attiva puo' essere attivata, una attiva puo'
   essere disattivata"
  (format *log* "~&set-link ~a ~a" ap dest)
  (let ((link (link-between ap dest)))

    ; Impostazione parametri specificati.
    (if error-rate-provided-p
      (setf (error-rate link) error-rate))
    (if delay-provided-p
      (setf (delay link) delay))
    (if bandwidth-provided-p
      (setf (bandwidth link) bandwidth))

    ; wpa-supplicant potrebbe attivare o disattivare l'interfaccia wireless.
    (when (typep dest 'wifi-interface)
      (if (and (associated-ap dest)
               (not (wpa-supplicant-would-activate link ap)))
        (iface-down dest))
      (if (and (not (associated-ap dest))
               (not (wifi-interface-associated-with ap))
               (wpa-supplicant-would-activate link ap))
        (iface-up dest ap)))))


(defun talk-local (&key duration)
  (dolist (pkt (make-udp-packet-list duration))
    (add *ulb* pkt)))


(defun talk-remote (&key duration)
  (format *log* "~&talk-remote, duration ~a" duration))


(defmethod iface-down ((wi wifi-interface))
  (error "TODO iface-down"))


(defmethod iface-up ((wi wifi-interface) (ap access-point))
  (format *log* "~&iface-up ~a ~a" wi ap)
  (setf (associated-ap wi) ap)
  (activate *ulb* wi))


(defun run ()
  "Esegue tutti gli eventi"
  (loop for current-event = (when *events* (pop *events*))
        while current-event
        do (assert (<= *now* (exec-at current-event)) nil "Eventi disordinati!")
	(setf *now* (exec-at current-event))
	;;(when (= *now* 9314) (break))
        (format *log* "~&~%~d " (exec-at current-event))
        (fire current-event)
;;	(break)
  ))


(defun do-logging (fn file-name)
  (with-open-file (*log* file-name :direction :output)
    (funcall fn)))


;;; Funzioni per lo script di configurazione

(defun add-access-points (&rest access-points)
  (dolist (ap access-points)
    (add *access-points* ap)))


(defun add-wifi-interfaces (&rest wifi-interfaces)
  (dolist (wi wifi-interfaces)
    (add *wifi-interfaces* wi)))


(defun generate-links ()
  "Genera i link tra ap e wi e tra ap e proxy"
  (loop for ap being the hash-values in *access-points*
        do (loop for wi
                 being the hash-values in *wifi-interfaces*
                 using (hash-key id)
                 do (setf (gethash id (net-links ap))
                          (new net-link :id (wifi-interface-by id))))
        (setf (gethash "proxy" (net-links ap))
              (new net-link :id *proxy*))))


;;; Instanziazione oggetti globali

(setf *proxy* (new proxy-server))
(setf *ulb* (new udp-load-balancer))
(setf *codec-bw* (kilobits-per-second 16))
