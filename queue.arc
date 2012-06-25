(def queue ()
  (annotate 'queue (list nil nil 0)))

(def enq (obj qq)
  (let q rep.qq
    (atomic
      (++ q.2)
      (if (no q.0)
        (= q.1 (= q.0 list.obj))
        (= (cdr q.1)  list.obj
           q.1        (cdr q.1)))
      q.0)))

(def deq (qq)
  (let q rep.qq
    (atomic (unless (is 0 q.2) (-- q.2))
            (pop q.0))))

(def qlist (qq)
  rep.qq.0)

(defmethod len (qq) queue
  rep.qq.2)

(def enq-limit (val qq ? limit 1000)
  (atomic
     (unless (< len.qq limit)
       (deq qq))
     (enq val qq)))
