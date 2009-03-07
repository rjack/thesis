
(defmacro set-link-event (exec-at ap dest &rest args)
  `(new event :exec-at ,exec-at
	:action (lambda ()
		  (set-link ,ap ,dest ,@args))))

(defmacro quit-event (exec-at)
  `(new event :exec-at ,exec-at
	:action (lambda ()
		  (format t "~&quit")
		  (quit))))


;;; Scenario

(defparameter *eth0* (new wifi-interface :id "eth0" :firmware "nak"))

(defparameter *csnet* (new access-point :id "csnet"))

(add-wifi-interfaces *eth0*)
(add-access-points *csnet*)

(generate-links)


;;; Eventi

(add-events
  (set-link-event (msecs 0) *csnet* *proxy*
		  :error-rate 20 :delay (msecs 30)
		  :bandwidth (megabits-per-second 10))

  (set-link-event (msecs 0) *csnet* *eth0*
		  :error-rate 10 :delay (msecs 2)
		  :bandwidth (megabits-per-second 1))

  (new event :exec-at (secs 2)
       :action (lambda ()
		 (talk-local :duration (secs 2))))

  (new event :exec-at (secs 3)
       :action (lambda ()
		 (talk-remote :duration (secs 1))))

  (quit-event (secs 10)))
