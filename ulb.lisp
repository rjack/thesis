;;; Variabili globali.

(defparameter *wifi-interfaces* (make-hash-table :test #'equal))
(defparameter *access-points* (make-hash-table :test #'equal))
(defparameter *events* ())
(defparameter *ulb* nil)
(defparameter *proxy* nil)
(defparameter *sendmsg-current-id* -1)
(defparameter *now* 0)

(defparameter *codec-kbs* 16)

(defparameter *ping-burst-length* 5)
(defparameter *ping-interval* 250) ;; millisecondi

;; TODO controllare
(defparameter *rtp-payload-min-size* 300)
(defparameter *rtp-payload-max-size* 700)

(defparameter *wpa-supplicant-error-rate-activation-threshold* 20
  "Se error-rate di un link wifi e' sotto questa soglia, l'interfaccia puo'
  venire attivata.")


;;; Accesso alle variabili globali

(defun add-events (&rest new-events)
  "Aggiunge i new-events agli *events* e riordina il tutto per istante di
  esecuzione."
  (setf *events* (nconc new-events *events*))
  (sort *events* #'< :key #'exec-at))


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

(defun percentp (n)
  (and (>= n 0) (<= n 100)))


(defun generate-sendmsg-id ()
  (format nil "sendmsg-id-~d" (incf *sendmsg-current-id*)))


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
     :initform 0
     :documentation "Dimensione del data-packet, in byte.")

   (overhead-size
     :initform 0)))


(defclass rtp-packet (packet)
  ((overhead-size
     :initform 12)))


(defclass udp-packet (packet)
  ((id
     :accessor id
     :documentation "Campo ID nell'header UDP che sarebbe usato per la
     frammentazione dei datagram ma che viene sfruttato d sendmsg-getid")

   (source
     :initarg :source
     :initform (error ":source mancante")
     :accessor source
     :documentation "Il mittente del pacchetto.")

   (overhead-size
     :initform 8)))


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


;;; Queste classi sono la rappresentazione software dei pacchetti da parte dei
;;; programmi.

(defclass ulb-struct-datagram (identified)
  ((id
     :initarg nil
     :accessor id
     :documentation "assegnato da sendmsg-getid")

   (end-of-life-event
     :initarg :end-of-life-event
     :initform (error ":end-of-life-event mancante")
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


(defmethod initialize-instance :after ((ping ulb-struct-ping) &key wifi-interface)
  (let ((seq (incf (current-ping-seqnum wifi-interface))))
    (setf (slot-value ping 'data)
          (new ping-packet :source (id wifi-interface)
	                   :score (score wifi-interface)
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
     :initform (make-hash-table :test #'equal) ;; di oggetti di tipo identified
     :accessor sent-datagrams
     :documentation "Gli ulb-struct-datagram spediti da un'interfaccia vengono
     accodati qui in attesa di un ACK o di un end-of-life-event che li scarti,
     oppure di un NAK o di un send-again-event che li ritrasmetta.")

   (send-ping-event
     :initform nil
     :accessor send-ping-event
     :documentation "Riferimento all'evento che spedira' il prossimo ping su
     questa interfaccia. L'evento deve venire procrastinato ogni volta che
     l'interfaccia spedisce un datagram dati.")

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


(defmethod nak-firmware-detected ((uwi ulb-wifi-interface))
  (with-accessors ((fw firmware-detected)) uwi
    (cond ((null fw) (setf fw "nak"))
	  ((equal "ack" fw) (setf fw "full"))
	  (t nil))))


(defmethod ack-firmware-detected ((uwi ulb-wifi-interface))
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
     :documentation "Coda di ulb-struct-datagram rtp (no ping!) da spedire
     sull'interfaccia con voto migliore.")))


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


(defmethod notify-nak ((uwi ulb-wifi-interface) (pkt udp-packet))
  (format t "notify-nak ~a ~a" (id uwi) (id pkt))
  ;; Ora sappiamo che interfaccia riceve nak: annotiamolo.
  (nak-firmware-detected uwi)
  ;; Recupera datagram.
  (let ((struct-dgram (gethash (id pkt) (sent-datagrams uwi))))
    ;; Rimuovilo dalla lista di quelli spediti.
    (remhash (id pkt) (sent-datagrams uwi))
    ;; Se non e' un ping va rispedito.
    (if (not (typep struct-dgram 'ulb-struct-ping))
      (add (outgoing-datagrams *ulb*) struct-dgram))
    ;; Log dell'accaduto.
    (add uwi
	 (new first-hop-outcome :dgram-id (id struct-dgram)
	                        :timestamp *now*
				:value "nak"))))

(defmethod notify-ack ((uwi ulb-wifi-interface) (pkt udp-packet))
  (format t "~%notify-ack ~a ~a" (id uwi) (id pkt))
  ;; Segnamo che interfaccia riceve ack
  (ack-firmware-detected uwi)
  ;; Recupera datagram.
  (let ((struct-dgram (gethash (id pkt) (sent-datagrams uwi))))
    ;; Rimuovilo dalla lista di quelli spediti
    (remhash (id pkt) (sent-datagrams uwi))
    (add uwi
	 (new first-hop-outcome :dgram-id (id struct-dgram)
	                        :timestamp *now*
				:value "ack"))))


(defmethod firmware-ack-p ((wi wifi-interface))
  (with-accessors ((fw firmware)) wi
    (or (equal fw "full")
	(equal fw "ack"))))


(defmethod firmware-nak-p ((wi wifi-interface))
  (with-accessors ((fw firmware)) wi
    (or (equal fw "full")
	(equal fw "nak"))))


(defmethod add ((wi wifi-interface) (pkt udp-packet))
  (with-accessors ((buf socket-send-buffer)) wi
    (if buf
      (nconc buf (list pkt))
      (progn
        (setf buf (list pkt))
        (flush wi)))))


(defmethod add ((uwi ulb-wifi-interface) (fpo full-path-outcome))
  "Aggiunge un full-path-outcome al full-path-log dell'ulb-wifi-interface."
  (with-accessors ((fp-log full-path-log)) uwi
    (setf fp-log (nconc fp-log (list fpo)))
    (assert (apply #'<= (mapcar #'sequence-number fp-log))
	    nil "full-path-log non e' ordinato per numero di sequenza dei ping!")
    (assert (apply #'<= (mapcar #'ping-sent-at fp-log))
	    nil "full-path-log non e' ordinato per istante di invio dei ping!")))


(defmethod add ((uwi ulb-wifi-interface) (fio first-hop-outcome))
  "Aggiunge un first-hop-outcome al first-hop-log dell'ulb-wifi-interface"
  (with-accessors ((fh-log first-hop-log)) uwi
    (setf fh-log (nconc fh-log (list fio)))))


;;; Proxy server

(defclass proxy-server (identified)
  ((id
     :initform :proxy)))


;; Metodi deliver: ritornano il tempo impiegato ad INVIARE il pacchetto
;; (istante invio primo bit - istante invio ultimo bit)

(defmethod deliver ((pkt udp-packet) ;; TODO controllare che sia un udp-packet
		    (wi wifi-interface) (link net-link) (ap access-point))
  "Da wifi-interface ad access-point"
  (let* ((send-delta-time (transmission-delta-time (size pkt) (bandwidth link)))
	 (arrival-time (+ *now* send-delta-time (delay link)))
	 (success-p (> (random 101) (error-rate link))))
    (when success-p
      (format t "~&deliver success ~a ~a ~a" (id pkt) (id wi) (id ap))
      ;; access-point riceve
      (add-events (new event :exec-at arrival-time
		             :action (lambda ()
				       (recv pkt ap wi))))
      (when (firmware-ack-p wi)
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
      (format t "~&deliver fail ~a ~a ~a" (id pkt) (id wi) (id ap))
      (when (firmware-nak-p wi)
	(add-events
	  (new event :exec-at (+ (delay link) arrival-time)
	             :action (lambda ()
			       (notify-nak (gethash (id wi)
						    (active-wifi-interfaces *ulb*))
					   pkt))))))))


(defmethod deliver ((pkt packet)
		    (ap access-point) (link net-link) (wi wifi-interface))
  "Da access-point a wifi-interface"
  (error "TODO deliver access-point wifi-interface"))


(defmethod deliver ((pkt packet)
		    (ap access-point) (link net-link) (px proxy-server))
  "Da access-point a proxy-server"
  (let* ((send-delta-time (transmission-delta-time (size pkt) (bandwidth link)))
	 (arrival-time (+ *now* send-delta-time (delay link)))
	 (success-p (> (random 101) (error-rate link))))
    (if success-p
      (progn
	(format t "~&deliver success ~a ~a ~a" (id pkt) (id ap) (id px))
	(add-events
	  (new event :exec-at arrival-time
	             :action (lambda ()
			       (recv pkt *proxy* ap)))))
      (format t "~&deliver fail ~a ~a ~a" (id pkt) (id ap) (id px)))))


(defmethod deliver ((pkt packet)
		    (px proxy-server) (link net-link) (ap access-point))
  "Da proxy-server ad access-point"
  (error "TODO deliver proxy-server access-point"))


(defmethod sendmsg-getid ((wi wifi-interface) (pkt udp-packet))
  (let ((sym (generate-sendmsg-id)))
    (setf (id pkt) sym)
    (add wi pkt)
    sym))


(defmethod send ((uwi ulb-wifi-interface) (struct ulb-struct-datagram))
  (error "TODO send ulb-wifi-interface ulb-struct-datagram"))


(defmethod send ((uwi ulb-wifi-interface) (struct ulb-struct-ping))
  "ULB spedisce un ping"
  ;; sendmsg-getid
  (setf (id struct)
        (sendmsg-getid (wifi-interface uwi) (data struct)))
  ;; registra spedizione ping
  (add uwi (new full-path-outcome
		:sequence-number (sequence-number (data struct))))
  ;; aggiunge il ping tra i datagram spediti
  (add (sent-datagrams uwi)
       struct)
  ;; impostazione prossimo ping
  (add-events
    (setf (send-ping-event uwi)
	  (new event :exec-at (if (< (current-ping-seqnum uwi)
				     *ping-burst-length*)
				*now*
				(+ *now* *ping-interval*))
	             :action #'(lambda ()
				 (send uwi (new ulb-struct-ping :wifi-interface uwi)))))))


(defmethod recv ((pkt udp-packet) (ap access-point) (wi wifi-interface))
  "Access point riceve un pacchetto da interfaccia wifi e lo spedisce al proxy."
  (format t "~&recv ~a ~a ~a" (id pkt) (id ap) (id wi))
  (deliver pkt ap (link-between ap *proxy*) *proxy*))


(defmethod recv ((pkt udp-packet) (px proxy-server) (ap access-point))
  "Proxy riceve un datagram"
  ;; TODO
  (format t "recv ~a ~a ~a" (id pkt) (id px) (id ap)))


(defmethod recv ((ping ping-packet) (px proxy-server) (ap access-point))
  "Proxy riceve un ping"
  ;; TODO
  (format t "recv ~a ~a ~a" (id pkt) (id px) (id ap)))


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
  (gethash :proxy (net-links ap)))


;;; Evento

(defclass event ()
  ((exec-at
     :initarg :exec-at
     :initform (error ":exec-at missing")
     :reader exec-at
     :documentation "Istante in cui deve essere eseguito.")

   (action
     :initarg :action
     :initform (error ":action mancante")
     :reader action
     :documentation "Funzione da chiamare per eseguire l'evento.")))


(defmethod flush ((wi wifi-interface))
  (let* ((pkt (pop (socket-send-buffer wi)))
         (ap (associated-ap wi))
         (link (link-between ap wi))
	 ;; consegna pkt
         (delta-time (deliver pkt wi link ap)))
    (when (socket-send-buffer wi)
      (add-events (new event :exec-at (+ *now* delta-time)
                             :action (lambda ()
                                       (flush wi)))))))


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
  (format t "~&set-link ~a ~a" (id ap) (id dest))
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
  (format t "~&talk-local, duration ~a" duration))


(defun talk-remote (&key duration)
  (format t "~&talk-remote, duration ~a" duration))


(defmethod iface-down ((wi wifi-interface))
  (error "TODO iface-down"))


(defmethod iface-up ((wi wifi-interface) (ap access-point))
  (format t "~&iface-up ~a ~a" (id wi) (id ap))
  (setf (associated-ap wi) ap)
  (activate *ulb* wi))


(defun run ()
  "Esegue tutti gli eventi"
  (loop for current-event = (when *events* (pop *events*))
        while current-event
        do (assert (<= *now* (exec-at current-event)) nil "Eventi disordinati!")
	(setf *now* (exec-at current-event))
        (format t "~&~%~d " (exec-at current-event))
        (fire current-event)))


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
        (setf (gethash :proxy (net-links ap))
              (new net-link :id *proxy*))))


;;; Instanziazione oggetti globali

(setf *proxy* (new proxy-server))
(setf *ulb* (new udp-load-balancer))
