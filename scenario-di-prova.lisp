;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ELEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(scenario

  (access-point :essid "almawifi")
  (access-point :essid "csnet")

  (wifi-interface :id "eth0" :firmware-capabilities "ACK")
  (wifi-interface :id "eth1" :firmware-capabilities "NAK"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(events

  ;; path events

  (event :after (msecs 0)
	 (wifi-link :access-point "almawifi" :wifi-interface "eth0"
		    :error-rate 50 :delay (msecs 3))
	 (wifi-link :access-point "almawifi" :wifi-interface "eth1"
		    :error-rate 30 :delay (msecs 5))
	 (wifi-link :access-point "csnet" :wifi-interface "eth0"
		    :error-rate 20 :delay (msecs 2))
	 (wifi-link :access-point "csnet" :wifi-interface "eth1"
		    :error-rate 10 :delay (msecs 2)))

  ;; wire events

  (event :after (msecs 0)
	 (wired-link :access-point "almawifi"
		     :error-rate 20 :delay (msecs 120))
	 (wired-link :access-point "csnet"
		     :error-rate 30 :delay (msecs 20)))

  ;; talk events

  (event :after (msecs 0)
	 (talk-local (secs 3)))

  (event :after (+ (secs 3) (msecs 120))
	 (talk-remote (secs 5))))
