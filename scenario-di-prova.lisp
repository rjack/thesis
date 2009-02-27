
(defmacro set-link-event (exec-at ap dest &rest args)
  `(new event :exec-at ,exec-at
	:action (lambda ()
		  (set-link ,ap ,dest ,@args))))


;;; Scenario

(defparameter *eth0* (new wifi-interface :id :eth0 :firmware :ack))
(defparameter *eth1* (new wifi-interface :id :eth1 :firmware :nak))

(defparameter *csnet* (new access-point :id :csnet))
(defparameter *almawifi* (new access-point :id :almawifi))

(add-wifi-interfaces *eth0* *eth1*)
(add-access-points *csnet* *almawifi*)

(generate-links)


;;; Eventi

(add-events
  (set-link-event (msecs 0) *almawifi* *proxy*
		  :error-rate 10 :delay (msecs 23)
		  :bandwidth (megabits-per-second 27))

  (set-link-event (msecs 0) *almawifi* *eth0*
		  :error-rate 10 :delay (msecs 23)
		  :bandwidth (megabits-per-second 27))

  (set-link-event (msecs 0) *almawifi* *eth1*
		  :error-rate 10 :delay (msecs 23)
		  :bandwidth (megabits-per-second 10))

  (set-link-event (msecs 0) *csnet* *proxy*
		  :error-rate 10 :delay (msecs 23)
		  :bandwidth (megabits-per-second 27))

  (set-link-event (msecs 0) *csnet* *eth0*
		  :error-rate 10 :delay (msecs 23)
		  :bandwidth (megabits-per-second 27))

  (set-link-event (msecs 0) *csnet* *eth1*
		  :error-rate 10 :delay (msecs 23)
		  :bandwidth (megabits-per-second 10))

  (new event :exec-at (secs 2)
       :action (lambda ()
		 (talk-local :duration (secs 2))))

  (new event :exec-at (secs 3)
       :action (lambda ()
		 (talk-remote :duration (secs 1)))))


;;; Via!

(run)
