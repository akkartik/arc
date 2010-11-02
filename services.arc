(= new-thread $.thread)
(= kill-thread $.kill-thread)
(= break-thread $.break-thread)
(= current-thread $.current-thread)
(def dead(th) ($:tnil (thread-dead? th)))

(mac thread body
  `(new-thread (fn () ,@body)))

(= ccc $.call-with-current-continuation)

(= quit $.exit)

; need to use a better seed
(= rand $.random)
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
(= trunc [$:inexact->exact (truncate _)])
(= exact [$:tnil (exint? _)])

(= newstring $.make-string)

(= memory $.current-memory-use)
(= seconds $.current-seconds)
(= msec $.current-milliseconds)
(= current-process-milliseconds $.current-process-milliseconds)
(= current-gc-milliseconds $.current-gc-milliseconds)

(= sleep ($:wrapnil sleep))