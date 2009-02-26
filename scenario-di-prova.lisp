;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCENARIO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(generate-access-points :csnet :almawifi)

(generate-wifi-interfaces :eth0 :nak
			  :eth1 :ack)

(generate-links)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(predefined-events

  (new event :exec-at (msecs 0)
       :action (lambda ()
		 (set-link :almawifi :eth0
			   :error-rate 10 :delay (msecs 23)
			   :bandwidth (megabits-per-second 27))))

  (new event :exec-at (msecs 0)
       :action (lambda ()
		 (set-link :almawifi :proxy
			   :error-rate 10 :delay (msecs 23)
			   :bandwidth (megabits-per-second 27))))
  (new event :exec-at (secs 2)
       :action (lambda ()
		 (talk-local :duration (secs 2))))

  (new event :exec-at (secs 3)
       :action (lambda ()
		 (talk-remote :duration (secs 1))))
