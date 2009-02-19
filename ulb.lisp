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
     :documentation "Unique id (e.g. the iface name: eth0, wlan1, etc.)")

   (firmware-capabilities
     :initarg :firmware-capabilities
     :initform (error ":firmware-capabilities missing")
     :reader firmware-capabilities
     :documentation "List containing the string ACK, NAK or both")

   (associated-access-point
     :initform nil
     :accessor associated-access-point
     :documentation "Reference to the associated ap, NIL if interface is
                     down")

   (first-hop-log
     :initform nil
     :accessor first-hop-log
     :documentation "List of first-hop-outcome instances")

   (full-path-log
     :initform nil
     :accessor full-path-log
     :documentation "List of full-path-outcome instances")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass arc ()
  ((delay
     :initarg :delay
     :initform (error ":delay missing")
     :accessor delay)

   (error-rate
     :initarg :error-rate
     :initform (error ":error-rate missing")
     :accessor error-rate)

   (vertexes
     :initarg :vertexes
     :initform (error ":vertexes missing")
     :accessor vertexes)))


(defmethod initialize-instance :after ((arc arc) &key)
  (let ((vertnum (length (vertexes arc)))
	(error-rate (error-rate arc)))
    (assert (= 2 vertnum) nil
	    "arcs must have 2 vertexes, not ~D" vertnum)
    (assert (percentp error-rate) nil
	    "error-rate must be between 0 and 100, ~D given" error-rate)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ULB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ulb ()
  ((wifi-interfaces
     :initarg :wifi-interfaces
     :initform (error ":wifi-interfaces missing")
     :accessor wifi-interfaces
     :documentation "list of wifi-interface instances")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOFTPHONE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROXY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIMULATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass event ()
  ((exec-at
     :initarg :exec-at
     :initform (error ":exec-at missing")
     :reader exec-at
     :documentation "Time at which this event must be executed (absolute)")

   (action
     :initarg :action
     :initform (error ":action missing")
     :reader action
     :documentation "Function that must be called to execute this event: must
		     return nil or a list of events generated by this event")

   (action-arguments
     :initarg :action-arguments
     :initform (error ":action-arguments missing")
     :reader action-arguments
     :documentation "List of arguments to be passed to action")))


(defmethod fire ((ev event))
  (apply (action ev) (action-arguments ev)))


(defclass simulator ()
  ((paths
     :accessor paths)
   (events
     :accessor events)))


;; TODO TEST ME!
(defmethod run ((sim simulator))
  (loop for current-event = (when (events sim) (pop (events sim)))
	while (events sim)
	do (let ((new-events (fire current-event)))
	     (when new-events
	       (add sim new-events)))))


(defmethod add ((sim simulator) (evs list))
  "Add the evs event list to the events of sim, in order of execution time")
