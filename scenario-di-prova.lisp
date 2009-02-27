;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCENARIO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *eth0* (new wifi-interface :id :eth0 :firmware :ack))
(defparameter *eth1* (new wifi-interface :id :eth1 :firmware :nak))

(defparameter *csnet* (new access-point :essid :csnet))
(defparameter *almawifi* (new access-point :essid :almawifi))

(add-wifi-interfaces *eth0* *eth1*)
(add-access-points *csnet* *almawifi*)

(generate-links)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-events

  (new event :exec-at (msecs 0)
       :action (lambda ()
		 (set-link *almawifi* *eth0*
			   :error-rate 10 :delay (msecs 23)
			   :bandwidth (megabits-per-second 27))))

  (new event :exec-at (msecs 0)
       :action (lambda ()
		 (set-link *almawifi* *proxy*
			   :error-rate 10 :delay (msecs 23)
			   :bandwidth (megabits-per-second 27))))

  (new event :exec-at (secs 2)
       :action (lambda ()
		 (talk-local :duration (secs 2))))

  (new event :exec-at (secs 3)
       :action (lambda ()
		 (talk-remote :duration (secs 1)))))

(run)
