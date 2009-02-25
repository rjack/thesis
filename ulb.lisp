;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARAMETERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *codec-kbs* 16)

(defparameter *ping-burst-length* 5)

;; TODO controllare
(defparameter *rtp-payload-min-size* 300)
(defparameter *rtp-payload-max-size* 700)

(defparameter *wpa-supplicant-error-rate-activation-threshold* 20
  "Se error-rate di un link wifi e' sotto questa soglia, l'interfaccia puo'
  venire attivata.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun percentp (n)
  (and (>= n 0) (<= n 100)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro new (name &rest body)
  `(make-instance ',name ,@body))


(defmacro set-link-status-event (&rest body)
  `(new event :action #'set-link-status :needs-sim-ref t ,@body))


(defmacro talk-local-event (&rest body)
  `(new event :action #'talk-local
	:needs-sim-ref t :needs-itself-ref t ,@body))


(defmacro talk-remote-event (&rest body)
  `(new event :action #'talk-remote
	:needs-sim-ref t :needs-itself-ref t ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNITS OF MEASURE
;;; Time: everything is in milliseconds.
;;; Bandwidth: everything is in bytes per millisecond.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PACKETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod size ((pkt packet))
  (+ (overhead-size pkt)
     (size (payload pkt))))

(defmethod size ((data data-packet))
  (payload data))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WIFI-INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass first-hop-outcome ()
  ((dgram-id
     :initarg :dgram-id
     :initform (error ":dgram-id missing")
     :reader dgram-id
     :documentation "Datagram ID, as returned by sendmsg_getID()")

   (dgram-type
     :initarg :dgram-type
     :initform (error ":dgram-type missing")
     :reader dgram-type
     :documentation "Datagram type, 'DATA' or 'PROBE'")

   (timestamp
     :initarg :timestamp
     :initform (error ":timestamp missing")
     :reader timestamp
     :documentation "Outcome timestamp, i.e. when this outcome was logged")

   (value
     :initarg :value
     :initform (error ":value missing")
     :reader value
     :documentation "'ACK' if the datagram was acked, 'NAK' otherwise")))


(defclass full-path-outcome ()
  ((probe-seqnum
     :initarg :probe-seqnum
     :initform (error ":probe-seqnum missing")
     :reader probe-seqnum
     :documentation "Probe incremental seqnum, *NOT* the sendmsg_getID id")

   (probe-sent-at
     :initarg :probe-sent-at
     :initform (error ":probe-sent-at missing")
     :reader probe-sent-at
     :documentation "When the probe was sent")

   (probe-recv-at
     :initarg :probe-recv-at
     :initform (error ":probe-recv-at missing")
     :reader probe-recv-at
     :documentation "When the response was received")))


(defclass wifi-interface ()
  ((id
     :initarg :id
     :initform (error ":id missing")
     :reader id
     :documentation "Unique id (e.g. the iface name: eth0, wlan1, etc.)")

   (firmware-capabilities
     :initarg :firmware-capabilities
     :initform (error ":firmware-capabilities missing")
     :reader firmware-capabilities
     :documentation "String: ACK, NAK or FULL")

   (socket-send-buffer
     :initform nil
     :accessor socket-send-buffer
     :documentation "A list of packets, representing the socket send buffer.")

   (associated
     :initform nil
     :accessor associated
     :documentation "ESSID of the associated access point, nil if down")

   (ping-seqnum
     :initform 0
     :accessor ping-seqnum
     :documentation "Numero di sequenza del prossimo ping da spedire su questa
                     interfaccia.")

   (score
     :initform 0
     :accessor score
     :documentation "Voto dell'interfaccia")

   (first-hop-log
     :initform nil
     :accessor first-hop-log
     :documentation "List of first-hop-outcome instances")

   (full-path-log
     :initform nil
     :accessor full-path-log
     :documentation "List of full-path-outcome instances")))


(defmethod initialize-instance :after ((wi wifi-interface) &key)
  (with-accessors ((fwcap firmware-capabilities)) wi
    (assert (or (string= fwcap "ACK")
		(string= fwcap "NAK")
		(string= fwcap "FULL"))
	    nil "bad firmware-capabilities specified: ~a" fwcap)))


(defmethod make-ping ((wi wifi-interface))
  (new ping-packet
       :payload (new data-packet :payload 8)     ;; voto e seqnum: due int.
       :score (score wi)
       :sequence-number (incf (ping-seqnum wi))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NET-LINK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass net-link ()
  ((to
     :initarg :to
     :initform (error ":to missing")
     :reader to
     :documentation "Id of the endpoint of this net-link (e.g. wifi-interface
                     id or access-point essid")

   (delay
     :accessor delay
     :documentation "Delay of this link, e.g. rtt / 2")

   (bandwidth
     :accessor bandwidth
     :documentation "Bandwidth of this link, in bytes per second")

   (error-rate
     :accessor error-rate
     :documentation "Integer between 0 and 100")))


(defmethod wpa-supplicant-would-activate ((nl net-link))
  (and (bandwidth nl)
       (< (error-rate nl)
	  *wpa-supplicant-error-rate-activation-threshold*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOFTPHONE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROXY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIMULATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass access-point ()
  ((essid
     :initarg :essid
     :initform (error ":id missing")
     :reader essid
     :documentation "Access point unique essid")

   (net-links
     :initform (make-hash-table :test #'equal)
     :reader net-links
     :documentation "Hash table of net-link instances, with to as key, one for
                     wifi-interface and only one for proxy")))


(defclass event ()
  ((exec-at
     :initarg :exec-at
     :initform (error ":exec-at missing")
     :reader exec-at
     :documentation "Time at which this event must be executed (absolute)")

   (needs-sim-ref
     :initarg :needs-sim-ref
     :initform nil
     :reader needs-sim-ref
     :documentation "True if action needs the reference to the simulator instance")

   (needs-itself-ref
     :initarg :needs-itself-ref
     :initform nil
     :reader needs-itself-ref
     :documentation "True if action needs the reference to this event")

   (action
     :initarg :action
     :initform (error ":action missing")
     :reader action
     :documentation "Function that must be called to execute this event: must
		     return nil or a list of events generated by this event.
		     When called, will receive this event as the first
		     argument and then action-arguments.")

   (action-arguments
     :initarg :action-arguments
     :initform (error ":action-arguments missing")
     :reader action-arguments
     :documentation "List of arguments to be passed to action")))


(defclass simulator ()
  ((access-points
     :initform (make-hash-table :test #'equal)
     :accessor access-points
     :documentation "Hash table of access-point instances, with essids as keys")

   (wifi-interfaces
     :initform (make-hash-table :test #'equal)
     :accessor wifi-interfaces
     :documentation "Hash table of wifi-interface instances, with ids as keys")

   (events
     :initform nil
     :accessor events
     :documentation "List of events, ordered by execution time")))


(defmethod add ((sim simulator) (evs list))
  "Add the evs event list to the events of sim, in order of execution time"
  (assert (not (null evs)) nil
	  "evs must not be nil")
  (setf (events sim) (nconc evs (events sim)))
  (sort	(events sim) #'< :key #'exec-at))


(defmethod add ((sim simulator) (ap access-point))
  "Add ap to the access points of the simulator"
  (assert (not (null ap)) nil "ap must not be nil")
  (setf (gethash (essid ap) (access-points sim)) ap))


(defmethod add ((sim simulator) (wi wifi-interface))
  "Add wi to the wifi-interfaces of the simulator"
  (assert (not (null wi)) nil "wi must not be nil")
  (setf (gethash (id wi) (wifi-interfaces sim)) wi))


(defmethod generate-net-links ((sim simulator))
  (loop for ap being the hash-values in (access-points sim)
	do (loop for wi being the hash-values in (wifi-interfaces sim)
		 do (setf (gethash (id wi) (net-links ap))
			  (new net-link :to (id wi))))
	(setf (gethash "proxy" (net-links ap))
	      (new net-link :to "proxy"))))


(defmethod fire ((sim simulator) (ev event))
  ;; FIXME dio che schifo
  (cond
    ((and (needs-sim-ref ev)
	  (needs-itself-ref ev))
     (apply (action ev) sim ev (action-arguments ev)))

    ((and (needs-sim-ref ev)
	  (not (needs-itself-ref ev)))
     (apply (action ev) sim (action-arguments ev)))

    ((and (not (needs-sim-ref ev))
	  (needs-itself-ref ev))
     (apply (action ev) ev (action-arguments ev)))

    (t (apply (action ev) (action-arguments ev)))))


(defmethod set-link-status ((sim simulator) &key essid to
			    (error-rate nil error-rate-provided-p)
			    (delay nil delay-provided-p)
			    (bandwidth nil bandwidth-provided-p))
  "Simula un cambiamento di delay, error-rate o bandwidth nel link da essid a
   to. Come conseguenza, una interfaccia wireless non attiva puo' essere
   attivata, una attiva puo' essere disattivata"
  (let* ((ap (gethash essid (access-points sim)))
	 (wi (gethash to (wifi-interfaces sim)))
	 (link (gethash to (net-links ap))))

    ; Impostazione parametri specificati.
    (if error-rate-provided-p
      (setf (error-rate link) error-rate))
    (if delay-provided-p
      (setf (delay link) delay))
    (if bandwidth-provided-p
      (setf (bandwidth link) bandwidth))

    ; Se link wifi attiva o disattiva (altrimenti e' link wired con proxy,
    ; quindi nulla).
    (when wi
      (if (and (associated wi)
	       (not (wpa-supplicant-would-activate link)))
	(iface-down sim wi))
      (if (and (not (associated wi))
	       (wpa-supplicant-would-activate link))
	(iface-up sim wi essid)))))


(defmethod send ((sim simulator) (wi wifi-interface) (pkt packet))
  (error "TODO send"))


(defmethod talk-local ((sim simulator) (ev event) &key duration)
  (format t "~&talk-local, at ~a, duration ~a~%" (exec-at ev) duration))


(defmethod talk-remote ((sim simulator) (ev event) &key duration)
  (format t "~&talk-remote, at ~a, duration ~a~%" (exec-at ev) duration))


(defmethod iface-down ((sim simulator) (wi wifi-interface))
  (error "TODO iface-down"))


(defmethod iface-up ((sim simulator) (wi wifi-interface) (essid string))
  (setf (associated wi) essid)
  (loop repeat *ping-burst-length*
	do (send sim wi (make-ping wi))))


(defmethod run ((sim simulator))
  "Execute all of the events in the simulator."
  (loop for current-event = (when (events sim) (pop (events sim)))
	while current-event
	do (fire sim current-event)))
