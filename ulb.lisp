;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun percentp (n)
  (and (>= n 0) (<= n 100)))


#|
(defun parse-event (form)
  "Parse form and return an event instance"
  (assert (eql (first form) 'event) nil
	  "Not an event form: ~a" (first form))
  (let ((subform (rest form)))
    (new event :exec-at (getf form :after) :action :action-arguments)
    |#


(defun parse-events (form)
  "Parse form and return a list of event instances"
  (assert (eql (first form) 'events) nil
	  "Not an events form: ~a" (first form))
  (map 'list (rest form) #'parse-event))


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

   (wifi-links
     :reader wifi-links
     :documentation "List of net-link instances, one for wifi-interface")

   (wired-link
     :reader wired-link
     :documentation "net-link for the wired link to proxy server.")))


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
		     return nil or a list of events generated by this event.
		     When called, will receive this event as the first
		     argument and then action-arguments.")

   (action-arguments
     :initarg :action-arguments
     :initform (error ":action-arguments missing")
     :reader action-arguments
     :documentation "List of arguments to be passed to action")))


(defmethod fire ((ev event))
  (apply (action ev) ev (action-arguments ev)))


(defclass simulator ()
  ((access-points
     :initform nil
     :accessor access-points
     :documentation "List of access-point instances")

   (wifi-interfaces
     :initform nil
     :accessor wifi-interfaces
     :documentation "List of wifi-interface instances")

   (events
     :initform nil
     :accessor events)))


(defmethod initialize-instance :after ((sim simulator) &key config-file-path)
  (with-open-file (in config-file-path)
    (loop for form = (read in nil)
	  while form
	  do (parse sim form))))


(defmethod parse ((sim simulator) (form list))
  "Recursively parse form and initialize sim"
  (let ((name (first form)))
    (cond
      ((eql name 'scenario) (dolist (subform (rest form))
			      (parse sim subform)))

      ((eql name 'events) (add sim (parse-events form)))

      ((eql name 'access-point)
       (push (eval (cons 'new form))
	     (access-points sim)))

      ((eql name 'wifi-interface)
       (push (eval (cons 'new form))
	     (wifi-interfaces sim)))

      ;; TODO DA FINIRE
      (t (error "~a not recognized" name)))))


(defmethod add ((sim simulator) (evs list))
  "Add the evs event list to the events of sim, in order of execution time"
  (assert (not (null evs)) nil
	  "evs must not be nill")
  (setf (events sim) (nconc evs (events sim)))
  (sort	(events sim) #'< :key #'exec-at))


(defmethod run ((sim simulator))
  "Execute all of the events in the simulator."
  (loop for current-event = (when (events sim) (pop (events sim)))
	while current-event
	do (let ((new-events (fire current-event)))
	     (when new-events
	       (add sim new-events)))))
