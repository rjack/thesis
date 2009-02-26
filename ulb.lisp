;;; Variabili globali.

(defparameter *wifi-interfaces* (make-hash-table))
(defparameter *access-points* (make-hash-table))
(defparameter *events* ())
(defparameter *ulb* nil)
(defparameter *proxy* nil)

(defparameter *codec-kbs* 16)

(defparameter *ping-burst-length* 5)

;; TODO controllare
(defparameter *rtp-payload-min-size* 300)
(defparameter *rtp-payload-max-size* 700)

(defparameter *wpa-supplicant-error-rate-activation-threshold* 20
  "Se error-rate di un link wifi e' sotto questa soglia, l'interfaccia puo'
  venire attivata.")


;;; Funzioni per lo script di configurazione

(defun generate-access-points (&rest aps)
  (dolist (essid aps)
    (setf (gethash essid *access-points*)
	  (new access-point :essid essid))))


(defun generate-wifi-interfaces (&rest wis)
  (loop for id in wis by #'cddr
	and cap in (rest wis) by #'cddr
	do (setf (gethash id *wifi-interfaces*)
		 (new wifi-interface :firmware-capabilities cap))))


(defun generate-links ()
  "Genera i link tra ap e wi e tra ap e proxy"
  (loop for ap being the hash-values in *access-points*
	do (loop for wi
		 being the hash-values in *wifi-interfaces*
		 using (hash-key id)
		 do (setf (gethash id (net-links ap))
			  (new net-link)))
	(setf (gethash :proxy (net-links ap))
	      (new net-link))))


;;; Accesso alle variabili globali

(defun add-events (&rest new-events)
  "Aggiunge i new-events agli *events* e riordina il tutto per istante di
  esecuzione."
  (setf *events* (nconc new-events *events*))
  (sort	*events* #'< :key #'exec-at))


(defun access-point (essid)
  (gethash essid *access-points*))


(defun wifi-interface (id)
  (gethash id *wifi-interfaces*))


(defun net-link (essid id)
  (gethash id (net-links (access-point essid))))


;;; Utilita'

(defun percentp (n)
  (and (>= n 0) (<= n 100)))


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


(defun transmission-time (nbytes bandwidth)
  (/ nbytes bandwidth))


;;; Pacchetti

;;; Queste classi rappresentano pacchetti veri, ovvero pacchetti che
;;; transitano sulla rete.

(defclass packet ()
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
  ((overhead-size
     :initform 8)))


(defclass ipv4-packet (packet)
  ((overhead-size
     :initform 20)))


(defclass wifi-frame (packet)
  ((overhead-size
     :initform 34)))


(defclass ping-packet (udp-packet)
  ((score
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

(defclass ulb-struct-datagram ()
  ((id
     :documentation "Assegnato da sendmsg_getID.")

   (end-of-life-event
     :documentation "Riferimento all'evento che rimuove questo pacchetto
     dall'ULB")

   (send-again-event
     :documentation "Riferimento all'evento che pone questo pacchetto
     nuovamente nella coda di spedizione dall'ULB")

   (data
     :documentation "Riferimento al pacchetto vero e proprio: un udp-packet o
     un ping-packet.")))


;; Metodi sui pacchetti

(defmethod size ((pkt packet))
  (+ (overhead-size pkt)
     (size (payload pkt))))

(defmethod size ((data data-packet))
  (payload data))


;;; Classi per l'Udp Load Balancer

(defclass first-hop-outcome ()
  ((dgram-id
     :initarg :dgram-id
     :initform (error ":dgram-id mancante")
     :reader dgram-id
     :documentation "ID del datagram, sarebbe assegnato da sendmsg_getID()")

   (dgram-type
     :initarg :dgram-type
     :initform (error ":dgram-type mancante")
     :reader dgram-type
     :documentation "Tipo del datagram, :data oppure :ping")

   (timestamp
     :initarg :timestamp
     :initform (error ":timestamp mancante")
     :reader timestamp
     :documentation "L'istante di creazione di questo first-hop-outcome")

   (value
     :initarg :value
     :initform (error ":value mancante")
     :reader value
     :documentation "Valore dell'outcome: :ack oppure :nak")
   ;; TODO :ack se TED dice ack oppure se ulb sa che ifaccia dice solo NAK ed
   ;; e' scaduto un timeout.
   ;; TODO :nak se TED dice nak oppure se ulb sa che ifaccia dice solo ACK ed
   ;; e' scaduto un timeout.


(defclass full-path-outcome ()
  ((ping-seqnum
     :initarg :ping-seqnum
     :initform (error ":ping-seqnum mancante")
     :reader probe-seqnum
     :documentation "Numero di sequenza del ping di cui si riferisce questo
     full-path-outcome.")

   (ping-sent-at
     :initarg :ping-sent-at
     :initform (error ":ping-sent-at mancante")
     :reader ping-sent-at
     :documentation "Istante di spedizione del ping.")

   (ping-recv-at
     :initarg :ping-recv-at
     :initform (error ":ping-recv-at mancante")
     :reader ping-recv-at
     :documentation "Istante di ricezione del ping di risposta.")))


(defclass ulb-wifi-interface ()
  ((firmware-detected-capabilities
     :initarg :firmware-detected-capabilities
     :initform (error ":firmware-detected-capabilities mancante")
     :accessor firmware-detected-capabilities
     :documentation ":ack, :nak oppure :full. Indica cio' che ULB ha dedotto
     del firmware della scheda, osservando le notifiche e i ping ricevuti.")

   (sent-datagrams
     :documentation "Gli ulb-struct-datagram spediti da un'interfaccia vengono
     accodati qui in attesa di un ACK o di un end-of-life-event che li scarti,
     oppure di un NAK o di un send-again-event che li ritrasmetta.")

   (send-ping-event
     :documentation "Riferimento all'evento che spedira' il prossimo ping su
     questa interfaccia. L'evento deve venire procrastinato ogni volta che
     l'interfaccia spedisce un datagram dati.")

   (next-ping-seqnum
     :initform -1
     :accessor next-ping-seqnum
     :documentation "Numero di sequenza del prossimo ping da spedire su questa
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


(defclass udp-load-balancer ()
  ((active-wifi-interfaces
     :initform (make-hash-table)
     :accessor active-wifi-interfaces
     :documentation "Hash table di riferimenti a istanze di
     ulb-wifi-interface, con gli id delle interfacce come chiavi.")

   (outgoing-datagrams
     :documentation "Coda di ulb-struct-datagram rtp (no ping!) da spedire
     sull'interfaccia con voto migliore.")))


;;; Net link

(defclass net-link ()
  ((delay
     :accessor delay
     :documentation "Latenza di questo link, cioe' rtt / 2.")

   (bandwidth
     :accessor bandwidth
     :documentation "Banda di questo link.")

   (error-rate
     :accessor error-rate
     :documentation "Percentuale d'errore su questo link, da 0 a 100")))


(defmethod wpa-supplicant-would-activate ((nl net-link))
  (and (bandwidth nl)
       (< (error-rate nl)
	  *wpa-supplicant-error-rate-activation-threshold*)))

;;; Access point

(defclass access-point ()
   (net-links
     :initform (make-hash-table)
     :reader net-links
     :documentation "Hash table di net-link, con id della destinazione come
     chiave."))


;;; Interfaccia wireless

(defclass wifi-interface ()
  ((firmware-capabilities
     :initarg :firmware-capabilities
     :initform (error ":firmware-capabilities mancante")
     :reader firmware-capabilities
     :documentation ":ack, :nak oppure :full. Rappresenta le effettive
     capacita' del firmware della sheda wireless.")

   (socket-send-buffer
     :initform nil
     :accessor socket-send-buffer
     :documentation "Lista di wifi-frame spediti dall'ulb su questa
     interfaccia.")

   (associated
     :initform nil
     :accessor associated
     :documentation "Riferimento all'access-point associato, nil se
     l'interfaccia e' inattiva.")))


;;; Proxy server

(defclass proxy-server ()
  ())


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


(defmethod fire ((ev event))
    (funcall (action ev)))


(defun set-link (essid to &key
		 (error-rate nil error-rate-provided-p)
		 (delay nil delay-provided-p)
		 (bandwidth nil bandwidth-provided-p))
  "Simula un cambiamento di delay, error-rate o bandwidth nel link
   dall'access-point con l'essid specificato a to. Come conseguenza, una
   interfaccia wireless non attiva puo' essere attivata, una attiva puo'
   essere disattivata"
  (let ((link (net-link essid to)))

    ; Impostazione parametri specificati.
    (if error-rate-provided-p
      (setf (error-rate link) error-rate))
    (if delay-provided-p
      (setf (delay link) delay))
    (if bandwidth-provided-p
      (setf (bandwidth link) bandwidth))

    ; Se link wifi, wpa-supplicant potrebbe attivare o disattivare l'interfaccia.
    ; Altrimenti altrimenti e' link wired con proxy, e non c'e' altro da fare.
    ; TODO DA QUI
    (when (wifi-interface to)
      (if (and (associated kwi)
	       (not (wpa-supplicant-would-activate link)))
	(iface-down sim kwi))
      (if (and (not (associated kwi))
	       (wpa-supplicant-would-activate link))
	(iface-up sim kwi essid)))))


(defmethod flush-socket-send-buffer ((sim simulator) (kwi kernel-wifi-interface))
  (let* ((ap (gethash (associated kwi) (access-points sim)))
	 (link (gethash (id kwi) (net-links ap))))
    (if (deliver-success link)
      (error "TODO calcola quando arriva all'access point e aggiungi evento"))))


(defmethod send ((sim simulator) (kwi kernel-wifi-interface) (pkt packet))
  "Accoda pkt nel socket-send-buffer di kwi.
  Se il buffer era vuoto, scatena l'evento flush."
  (with-accessors ((buf socket-send-buffer)) kwi
    (if buf
      (nconc buf pkt)
      (progn
	(setf buf (list pkt))
	(flush-socket-send-buffer sim kwi)))))



(defmethod talk-local ((sim simulator) (ev event) &key duration)
  (format t "~&talk-local, at ~a, duration ~a~%" (exec-at ev) duration))


(defmethod talk-remote ((sim simulator) (ev event) &key duration)
  (format t "~&talk-remote, at ~a, duration ~a~%" (exec-at ev) duration))


(defmethod iface-down ((sim simulator) (uwi ulb-wifi-interface))
  (error "TODO iface-down"))


;(defmethod iface-up ((sim simulator) (kwi kernel-wifi-interface) (essid string))
;  (setf (associated kwi) essid)


(defun run ()
  "Esegue tutti gli eventi"
  (loop for current-event = (when *events* (pop *events*))
	while current-event
	do (fire current-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instanziazione classi globali
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *proxy* (new proxy))
(setf *ulb* (new udp-load-balancer))
