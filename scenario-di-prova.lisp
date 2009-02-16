(wifi-events

  (event :after (msec 0)
    (link :access-point 0 :iface 0 :error-rate 50 :delay (msec 3))
    (link :access-point 0 :iface 1 :error-rate 30 :delay (msec 5))
    (link :access-point 1 :iface 0 :error-rate 20 :delay (msec 2))
    (link :access-point 1 :iface 1 :error-rate 10 :delay (msec 2))))


(wire-events

  (event :after (msec 0)
    ((internet-path 0) :error-rate 40 :delay (msec 120))
    ((internet-path 1) :error-rate 10 :delay (msec 70))))


(talk-events

  (event :after (msec 0)
    (talk-local (sec 3)))
  
  (event :after (+ (sec 3) (msec 120))
    (talk-remote (sec 5))))
