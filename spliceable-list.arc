;; append elems to it, check n-elem suffix for a match, then splice the suffix out

(def spliceable-list (n ? init nil)
  ++.n
  (annotate 'spliceable-list (obj contents init
                                  last lastcdr.init
                                  suffix-len n
                                  suffix (suffix n init))))

(defextend len (l) (isa l 'spliceable-list)
  (len rep.l!contents))

(defextend empty (l) (isa l 'spliceable-list)
  (empty rep.l!contents))

(def append (b a)
  (= (cdr lastcdr.a) b))

(defextend append (tail l) (isa l 'spliceable-list)
  (if empty.l
    (= rep.l!contents tail
       rep.l!last rep.tail)
    (= (cdr rep.l!last) tail))
  (zap lastcdr rep.l!last)
  (if rep.l!suffix
    (zap [nthcdr len.tail _] rep.l!suffix)
    ; no suffix yet; do we have enough elems to start?
    (if (is rep.l!suffix-len (len rep.l!contents))
      (= rep.l!suffix rep.l!contents))))

(defcoerce cons spliceable-list (l)
  rep.l!contents)

(defcoerce spliceable-list cons (l)
  (spliceable-list l))

(defcall spliceable-list (l i)
  rep.l!contents.i)

(defextend sref (l v i) (isa l 'spliceable-list)
  (sref rep.l!contents v i))

; returns all but the suffix; corrupts the suffix list
(def splice (l)
  (when rep.l!suffix
    (wipe (cdr rep.l!suffix))
    rep.l!contents))

; return last n elems of l -- as long as there are at least that many
(def suffix (n l)
  (let max len.l
    (if (>= max n)
      (nthcdr (- max n) l))))

(defextend suffix (l) (isa l 'spliceable-list)
  (aif
    rep.l!suffix
      cdr.it
    (iso (len rep.l!contents) (- rep.l!suffix-len 1))
      rep.l!contents))
