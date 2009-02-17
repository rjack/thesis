;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(events

  ;; path events

  (event :after (msecs 0)
	 (link :access-point 0 :iface 0 :error-rate 50 :delay (msecs 3))
	 (link :access-point 0 :iface 1 :error-rate 30 :delay (msecs 5))
	 (link :access-point 1 :iface 0 :error-rate 20 :delay (msecs 2))
	 (link :access-point 1 :iface 1 :error-rate 10 :delay (msecs 2)))
 

  ;; wire events

  (event :after (msecs 0)
	 ((internet-path 0) :error-rate 40 :delay (msecs 120))
	 ((internet-path 1) :error-rate 10 :delay (msecs 70)))
 

  ;; talk events

  (event :after (msecs 0)
	 (talk-local (secs 3)))

  (event :after (+ (secs 3) (msecs 120))
	 (talk-remote (secs 5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(paths

  (access-point
