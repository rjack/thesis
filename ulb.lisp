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

(defclass simulator ()
  ((paths
     :accessor paths)
   (events
     :accessor paths)))


;(defmethod initialize-instance :after ((sim simulator) &key script)
