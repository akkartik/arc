(= new-thread $.thread)
(= kill-thread $.kill-thread)
(= break-thread $.break-thread)
(= current-thread $.current-thread)
(def dead(th) ($:tnil (thread-dead? th)))

(mac thread body
  `(new-thread (fn () ,@body)))

(= sin $.sin)
(= cos $.cos)
(= tan $.tan)
(= asin $.asin)
(= acos $.acos)
(= atan $.atan)
(= log $.log)
(= remainder $.remainder)
(= ceiling $.ceiling)
(= floor $.floor)
