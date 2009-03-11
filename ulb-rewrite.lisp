(declaim (optimize (safety 3)))

(defclass identifiable ()
  ((id
    :initarg :id
;    :initform (error ":id missing")
    :reader id-of
    :type fixnum
    :documentation "Identifier. Can be unique or not, subclasses decide.")))


(defclass event (identifiable)
  ((time
    :initarg :time
    :initform (error "missing :time")
    :reader time-of
    :type fixnum
    :documentation "When this event must be executed.")

   (action
    :initarg :action
    :initform (error "missing :action")
    :reader action-of
    :type function   ; FIXME: must fully specify signature.
    :documentation "Implements the behaviour of this event.")))



(defclass world ()
  ((events
    :initarg events
    :initform ()
    :accessor events-of
    :type list
    :documentation "World's events.")))


(defgeneric run (context obj)
  (:documentation "Execute the action associated with the object in
  the given context."))


(defmethod run ((w world) (ev event))
  (funcall (action-of ev)))