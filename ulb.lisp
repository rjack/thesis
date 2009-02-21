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
;;; TIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun msecs (ms)
  ms)


(defun secs (s)
  (* s (msecs 1000)))


(defun mins (m)
  (* m (secs 60)))


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

   (essid
     :initform nil
     :accessor essid
     :documentation "ESSID of the associated access point, nil if down")

   (first-hop-log
     :initform nil
     :accessor first-hop-log
     :documentation "List of first-hop-outcome instances")

   (full-path-log
     :initform nil
     :accessor full-path-log
     :documentation "List of full-path-outcome instances")))


(defmethod initialize-instance :after ((wlan wifi-interface) &key)
  (with-accessors ((fwcap firmware-capabilities)) wlan
    (assert (or (string= fwcap "ACK")
		(string= fwcap "NAK")
		(string= fwcap "FULL"))
	    nil "bad firmware-capabilities specified: ~a" fwcap)))


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

   (error-rate
     :accessor error-rate
     :documentation "Integer between 0 and 100")))


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


(defmethod set-link-status ((sim simulator) &key essid to error-rate delay)
  (let* ((ap (gethash essid (access-points sim)))
	 (link (gethash to (net-links ap))))
    (setf (error-rate link) error-rate)
    (setf (delay link) delay)))


(defmethod talk-local ((sim simulator) (ev event) &key duration)
  (format *query-io*
	  "~&talk-local, at ~a, duration ~a~%"
	  (exec-at ev) duration))


(defmethod talk-remote ((sim simulator) (ev event) &key duration)
  (format *query-io*
	  "~&talk-remote, at ~a, duration ~a~%"
	  (exec-at ev) duration))

(defmethod run ((sim simulator))
  "Execute all of the events in the simulator."
  (loop for current-event = (when (events sim) (pop (events sim)))
	while current-event
	do (fire sim current-event)))
