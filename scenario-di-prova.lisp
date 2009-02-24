(defparameter *sim* (new simulator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCENARIO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add *sim* (new access-point :essid "almawifi"))
(add *sim* (new access-point :essid "csnet"))

(add *sim* (new wifi-interface :id "eth0" :firmware-capabilities "ACK"))
(add *sim* (new wifi-interface :id "eth1" :firmware-capabilities "NAK"))

(generate-net-links *sim*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add *sim*
     (list

       (set-link-status-event
	 :exec-at (msecs 0)
	 :action-arguments (list :essid "almawifi" :to "proxy"
				 :error-rate 10 :delay (msecs 23)
				 :bandwidth (megabits-per-second 27)))
       (set-link-status-event
	 :exec-at (msecs 0)
	 :action-arguments (list :essid "almawifi" :to "eth0"
				 :error-rate 50 :delay (msecs 70)
				 :bandwidth (megabits-per-second 27)))

       (set-link-status-event
	 :exec-at (msecs 0)
	 :action-arguments (list :essid "almawifi" :to "eth1"
				 :error-rate 10 :delay (msecs 50)
				 :bandwidth (megabits-per-second 27)))


       (set-link-status-event
	 :exec-at (msecs 0)
	 :action-arguments (list :essid "csnet" :to "proxy"
				 :error-rate 0 :delay (msecs 12)
				 :bandwidth (megabits-per-second 27)))

       (set-link-status-event
	 :exec-at (msecs 0)
	 :action-arguments (list :essid "csnet" :to "eth0"
				 :error-rate 30 :delay (msecs 30)
				 :bandwidth (megabits-per-second 27)))

       (set-link-status-event
	 :exec-at (msecs 0)
	 :action-arguments (list :essid "csnet" :to "eth1"
				 :error-rate 70 :delay (msecs 100)
				 :bandwidth (megabits-per-second 27)))

       (talk-local-event
	 :exec-at (secs 2)
	 :action-arguments (list :duration (secs 2)))

       (talk-remote-event
	 :exec-at (secs 3)
	 :action-arguments (list :duration (secs 5)))))
