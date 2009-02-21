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
       (new event
	    :exec-at (msecs 0)
	    :action #'set-link-status
	    :action-arguments (list :essid "almawifi" :to "eth0"
				    :error-rate 50 :delay (msecs 70)))

       (new event
	    :exec-at (msecs 0)
	    :action #'set-link-status
	    :action-arguments (list :essid "almawifi" :to "eth1"
				    :error-rate 10 :delay (msecs 50)))

       (new event :exec-at (msecs 0)
	    :action #'set-link-status
	    :action-arguments (list :essid "csnet" :to "eth0"
				    :error-rate 30 :delay (msecs 30)))

       (new event
	    :exec-at (msecs 0)
	    :action #'set-link-status
	    :action-arguments (list :essid "csnet" :to "eth1"
				    :error-rate 70 :delay (msecs 100)))

       (new event
	    :exec-at (secs 2)
	    :action #'talk-local
	    :action-arguments (list :duration (secs 2)))

       (new event
	    :exec-at (secs 3)
	    :action #'talk-local
	    :action-arguments (list :duration (secs 5)))))
