; Main Arc lib.  Ported to Scheme version Jul 06.

; need better way of generating strings; too many calls to string
;  maybe strings with escape char for evaluation
; make foo~bar equiv of foo:~bar (in expand-ssyntax)
; add sigs of ops defined in ac.scm
; get hold of error types within arc
; does macex have to be defined in scheme instead of using def below?
; write disp, read, write in arc
; could I get all of macros up into arc.arc?
; warn when shadow a global name
; some simple regexp/parsing plan

; experimental: no testify in keep and some
; separate string type
;  (= (cdr (cdr str)) "foo") couldn't work because no way to get str tail
;  not sure this is a mistake; strings may be subtly different from
;  lists of chars


(assign do (annotate 'mac
             (fn args `((fn () ,@args)))))

(assign safeset (annotate 'mac
                  (fn (var val)
                    `(do (if (bound ',var)
                             (do (disp "*** redefining " (stderr))
                                 (disp ',var (stderr))
                                 (disp #\newline (stderr))))
                         (assign ,var ,val)))))

(assign def (annotate 'mac
               (fn (name parms . body)
                 `(do (sref sig ',parms ',name)
                      (safeset ,name (fn ,parms ,@body))))))

(def caar (xs) (car (car xs)))
(def cadr (xs) (car (cdr xs)))
(def cddr (xs) (cdr (cdr xs)))

(def no (x) (is x nil))

(def acons (x) (is (type x) 'cons))

(def atom (x) (no (acons x)))

(def isa (x y) (is (type x) y))

(def list args args)

(def idfn (x) x)

(def isnt (x y) (no (is x y)))

; Maybe later make this internal.  Useful to let xs be a fn?

(def map1 (f xs)
  (if (no xs)
      nil
      (cons (f (car xs)) (map1 f (cdr xs)))))

(def pair (xs ? f list)
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))

(assign mac (annotate 'mac
              (fn (name parms . body)
                `(do (sref sig ',parms ',name)
                     (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))

(mac thunk body
  `(fn() ,@body))

(mac ifcall(var)
  `(when (bound ',var)
     (,var)))

(mac pushif(elem ls)
  `(aif ,elem
     (push it ,ls)))

(mac firsttime(place . body)
  `(unless ,place
     ,@body
     (set ,place)))

(mac proc(name args . body)
  `(def ,name ,args ,@body nil))

(mac ret(var val . body)
 `(let ,var ,val ,@body ,var))

(mac awhile(expr . body)
  `(whilet it ,expr
    ,@body))

(mac forever body
  `(while t ,@body))

(mac disabled body
  `(when nil ,@body))

(mac enabled body
  `(when t ,@body))



(mac with (parms . body)
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
  (if (no parms)
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms)
         (withs ,(cddr parms) ,@body))))

(mac w/uniq (names . body)
  (if (acons names)
      `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                             names))
         ,@body)
      `(let ,names (uniq) ,@body)))

(mac or args
  (if args
    (w/uniq g
      `(let ,g ,(car args)
         (if ,g ,g (or ,@(cdr args)))))))

(mac and args
  (if args
      (if (cdr args)
          `(if ,(car args) (and ,@(cdr args)))
          (car args))
      't))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr . body)
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))

(mac awhen (expr . body)
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  (if (no args)
      't
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

; Need rfn for use in macro expansions.

(mac rfn (name parms . body)
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  `(let self nil
     (assign self (fn ,parms ,@body))))

(def alist (x) (or (no x) (is (type x) 'cons)))



; Ac expands x:y:z into (compose x y z), ~x into (complement x)

; Only used when the call to compose doesn't occur in functional position.
; Composes in functional position are transformed away by ac.

(mac compose args
  (let g (uniq)
    `(fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
         args))))

; Ditto: complement in functional position optimized by ac.

(mac complement (f)
  (let g (uniq)
    `(fn ,g (no (apply ,f ,g)))))



; Rtm prefers to overload + to do this

(def join args
  (if (no args)
      nil
      (let a (car args)
        (if (no a)
            (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))

; bootstrapping version; overloaded later as a generic function
(def iso (x y)
  (or (is x y)
      (and (acons x)
           (acons y)
           (iso (car x) (car y))
           (iso (cdr x) (cdr y)))))

; bootstrapping version; overloaded later as a generic function
(def reclist (f xs)
  (and xs
       (or (f xs)
           (reclist f cdr.xs))))

(mac in (x . choices)
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))

(mac when (test . body)
  `(if ,test (do ,@body)))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.

(mac caselet (var expr . args)
  (let ex (afn (args)
            (if (no (cdr args))
                (car args)
                `(if (iso ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  `(caselet ,(uniq) ,expr ,@args))

(def some (f seq)
  (reclist f:car seq))



; Possible to write map without map1, but makes News 3x slower.

;(def map (f . seqs)
;  (if (some1 no seqs)
;       nil
;      (no (cdr seqs))
;       (let s1 (car seqs)
;         (cons (f (car s1))
;               (map f (cdr s1))))
;      (cons (apply f (map car seqs))
;            (apply map f (map cdr seqs)))))


(def map (f . seqs)
  (if (some [isa _ 'string] seqs)
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (+ i 1)))))
          0))
      (no (cdr seqs))
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some no seqs)
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def mappend (f . args)
  (apply + nil (apply map f args)))

(def firstn (n xs)
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

(def caris (x val)
  (and (acons x) (is (car x) val)))

(def warn (msg . args)
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  `(atomic (let ,@args)))

(mac atwith args
  `(atomic (with ,@args)))

(mac atwiths args
  `(atomic (withs ,@args)))


; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to withs
;  get is an expression returning the current value in the place
;  set is an expression representing a function of one argument
;   that stores a new value in the place

; A bit gross that it works based on the *name* in the car, but maybe
; wrong to worry.  Macros live in expression land.

; seems meaningful to e.g. (push 1 (pop x)) if (car x) is a cons.
; can't in cl though.  could I define a setter for push or pop?

(assign setter (table))

(mac defset (name parms . body)
  (w/uniq gexpr
    `(sref setter
           (fn (,gexpr)
             (let ,parms (cdr ,gexpr)
               ,@body))
           ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional
; position.  Such bugs will be seen only when the code is executed, when
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa expr 'sym)
         (if (ssyntax expr)
             (setforms (ssexpand expr))
             (w/uniq (g h)
               (list (list g expr)
                     g
                     `(fn (,h) (assign ,expr ,h)))))
        ; make it also work for uncompressed calls to compose
        (and (acons expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
        (and (acons expr) (acons (car expr)) (is (caar expr) 'get))
         (setforms (list (cadr expr) (cadr (car expr))))
         (let f (setter (car expr))
           (if f
               (f expr)
               ; assumed to be data structure in fn position
               (do (when (caris (car expr) 'fn)
                     (warn "Inverting what looks like a function call"
                           expr0 expr))
                   (w/uniq (g h)
                     (let argsyms (map [uniq] (cdr expr))
                        (list (+ (list g (car expr))
                                 (mappend list argsyms (cdr expr)))
                              `(,g ,@argsyms)
                              `(fn (,h) (sref ,g ,h ,(car argsyms))))))))))))

(def metafn (x)
  (or (ssyntax x)
      (and (acons x) (in (car x) 'compose 'complement))))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
       ((afn (fs)
          (if (caris (car fs) 'compose)            ; nested compose
               (self (join (cdr (car fs)) (cdr fs)))
              (cdr fs)
               (list (car fs) (self (cdr fs)))
              (cons (car fs) args)))
        (cdr f))
      (is (car f) 'no)
       (err "Can't invert " (cons f args))
       (cons f args)))

(def expand= (place val)
  (if (and (isa place 'sym) (~ssyntax place))
      `(assign ,place ,val)
      (let (vars prev setter) (setforms place)
        (w/uniq g
          `(atwith ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
  (expand=list args))
(= const =)
(= redef =)

(mac init args
  `(unless (bound ',(car args))
     (= ,@args)))

(mac letloop(var init term inc . body)
  `(let ,var nil
     (loop (= ,var ,init) ,term ,inc
        ,@body)))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs ? n 2)
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

; If ok to do with =, why not with def?  But see if use it.

(mac defs args
  `(do ,@(map [cons 'def _] (tuples args 3))))



(def last (xs)
  (if (cdr xs)
    (last cdr.xs)
    (car xs)))

(def transform-last (f xs)
  (if (cdr xs)
    (cons car.xs (transform-last f cdr.xs))
    (list:f car.xs)))

(= vtables* (table))
(mac genericexpander(coerce-all coerce-back
                     name args . body)
  (w/uniq (allargs basefn)
    `(do
      (= (vtables* ',name)
        (let ,basefn (fn ,args ,@body)
           ; Assume body handles primitives by default. Can be overridden.
           (obj () ,basefn
                sym ,basefn
                char ,basefn
                int ,basefn
                num ,basefn
                cons ,basefn)))
      (def ,name ,allargs
        (aif (aand (vtables* ',name) (it (type:last ,allargs)))
          (apply it ,allargs)
          ,(with (last-coercer `(apply ,name (transform-last [coerce _ 'cons] ,allargs))
                  all-coercer `(apply ,name (map [coerce _ 'cons] ,allargs)))
             (case (list coerce-all coerce-back)
               (nil nil)  last-coercer
               (t nil)  all-coercer
               (nil t)    `(coerce ,last-coercer (type (last ,allargs)))
               (t t)  `(coerce ,all-coercer (type (last ,allargs))))))))))

(mac defgeneric (name args . body)
  `(genericexpander nil nil ,name ,args ,@body))
(mac defgeneric* (name args . body)
  `(genericexpander t nil ,name ,args ,@body))

(mac deftransform (name args . body)
  `(genericexpander nil t ,name ,args ,@body))
(mac deftransform* (name args . body)
  `(genericexpander t t ,name ,args ,@body))

(mac defmethod (name args type . body)
  `(= ((vtables* ',name) ',type)
      (fn ,args
        ,@body)))



(def assoc (key al)
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key) (cadr (assoc key al)))

(def rev (xs)
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(mac unless (test . body)
  `(if (no ,test) (do ,@body)))

(mac while (test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))



(mac loop (start test update . body)
  (w/uniq (gfn gparm)
    `(do ,start
         ((rfn ,gfn (,gparm)
            (if ,gparm
                (do ,@body ,update (,gfn ,test))))
          ,test))))

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (assign ,v ,gi) (< ,v ,gm) (assign ,v (+ ,v 1))
         ,@body))))

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (- ,min 1))
       (loop (assign ,v ,gi) (> ,v ,gm) (assign ,v (- ,v 1))
         ,@body))))

(mac repeat (n . body)
  `(for ,(uniq) 1 ,n ,@body))

; could bind index instead of gensym

(mac each (var expr . body)
  (w/uniq (ga gv)
    `(iflet ,ga ,expr
      ((afn (,gv)
        (when (acons ,gv)
          (let ,var (car ,gv) ,@body)
          (self (cdr ,gv))))
        (coerce ,ga 'cons)))))

; (nthcdr x y) = (cut y x).

(def cut (seq start ? end nil)
  (let end (if (no end)   (len seq)
               (< end 0)  (+ (len seq) end)
                          end)
    (if (isa seq 'string)
        (let s2 (newstring (- end start))
          (for i 0 (- end start 1)
            (= (s2 i) (seq (+ start i))))
          s2)
        (firstn (- end start) (nthcdr start seq)))))

(mac whilet (var test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))



(mac do1 args
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

(mac push (x place)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  (with (vars (map [uniq] places)
         forms (map setforms places))
    `(atwiths ,(mappend (fn (g (binds val setter))
                          (+ binds (list g val)))
                        vars
                        forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g)
              (,setter (cdr ,g)))))))

(def adjoin (x xs ? test iso)
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac togglemem (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (if (mem ,gx ,val)
                      (rem ,gx ,val)
                      (adjoin ,gx ,val ,@args)))))))

(mac ++ (place ? i 1)
  (if (isa place 'sym)
      `(= ,place (+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (+ ,val ,gi)))))))

(mac -- (place ? i 1)
  (if (isa place 'sym)
      `(= ,place (- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (- ,val ,gi)))))))

; E.g. (++ x) equiv to (zap + x 1)

(mac zap (op place . args)
  (with (gop    (uniq)
         gargs  (map [uniq] args)
         mix    (afn seqs
                  (if (some no seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

; Can't simply mod pr to print strings represented as lists of chars,
; because empty string will get printed as nil.  Would need to rep strings
; as lists of chars annotated with 'string, and modify car and cdr to get
; the rep of these.  That would also require hacking the reader.

(def pr args
  (map1 disp args)
  (car args))

(def prt args
  (map1 [if _ (disp _)] args)
  (car args))

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))

(mac wipe args
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac set args
  `(do ,@(map (fn (a) `(= ,a t)) args)))

(mac accum (accfn . body)
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       (rev ,gacc))))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr ? eof nil)
  (w/uniq (gacc gdone gres)
    `(with (,gacc nil ,gdone nil)
       (while (no ,gdone)
         (let ,gres ,expr
           (if (is ,gres ,eof)
               (= ,gdone t)
               (push ,gres ,gacc))))
       (rev ,gacc))))

; For the common C idiom while x = snarfdata != stopval.
; Rename this if use it often.

(mac whiler (var expr endval . body)
  (w/uniq gf
    `(withs (,var nil ,gf (testify ,endval))
       (while (no (,gf (= ,var ,expr)))
         ,@body))))



(def fill-table (table data)
  (each (k v) (pair data) (= (table k) v))
  table)

(def keys (h)
  (accum a (each (k v) h (a k))))

(def vals (h)
  (accum a (each (k v) h (a v))))

(def tablist (h)
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(mac obj args
  `(listtab (list ,@(map (fn ((k v))
                           `(list ',k ,v))
                         (pair args)))))



(def empty (seq)
  (is len.seq 0))

(defgeneric reclist (f xs)
  (and xs
       (or (f xs)
           (reclist f cdr.xs))))

(def testify (x)
  (if (isa x 'fn) x [is _ x]))

(def all (test seq)
  (~some ~testify.test seq))

(def mem (test seq)
  (let f (testify test)
    (reclist [if (f:car _) _] seq)))

(def find (test seq)
  (some [if (testify.test _) _] seq))

(def recstring (test s ? start 0)
  ((afn (i)
     (and (< i (len s))
          (or (test i)
              (self (+ i 1)))))
   start))

(deftransform keep (f seq)
  (if (no seq)      nil
      (f car.seq)   (cons car.seq (keep f cdr.seq))
                    (keep f cdr.seq)))

(def rem (test seq)
  (keep ~testify.test seq))

;(def trues (f seq)
;  (rem nil (map f seq)))

(def trues (f xs)
  (and xs
      (let fx (f (car xs))
        (if fx
            (cons fx (trues f (cdr xs)))
            (trues f (cdr xs))))))



;(def macex (e)
;  (if (atom e)
;      e
;      (let op (and (atom (car e)) (eval (car e)))
;        (if (isa op 'mac)
;            (apply (rep op) (cdr e))
;            e))))

(def consif (x y) (if x (cons x y) y))

(def string args
  (apply + "" (map [coerce _ 'string] args)))

(mac as (type x)
  `(coerce ,x ',type))

(def flat x
  ((afn (x acc)
     (if (no x)   acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test ? alt nil)
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq ? start 0)
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)
                nil
               (f (car seq))
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq)
         start)
        (recstring [if (f (seq _)) _] seq start))))

(def even (n) (is (mod n 2) 0))

(def odd (n) (no (even n)))

(mac after (x . ys)
  `(protect (fn () ,x) (fn () ,@ys)))

(let expander
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name . body)
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    (expander 'open-socket var port body))
  )

(mac w/outstring (var . body)
  `(let ,var (outstring) ,@body))

; what happens to a file opened for append if arc is killed in
; the middle of a write?

(mac w/appendfile (var name . body)
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

; rename this simply "to"?  - prob not; rarely use

(mac w/stdout (str . body)
  `(call-w/stdout ,str (fn () ,@body)))

(mac w/stdin (str . body)
  `(call-w/stdin ,str (fn () ,@body)))

(mac tostring body
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(mac fromstring (str . body)
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

; http://arclanguage.org/item?id=12877
(mac fromfile (f . body)
  (w/uniq gf
    `(w/infile ,gf ,f
       (w/stdin ,gf
         ,@body))))

(mac tofile (f . body)
  (w/uniq gf
    `(w/outfile ,gf ,f
       (w/stdout ,gf
         ,@body))))

(mac ontofile (f . body)
  (w/uniq gf
    `(w/appendfile ,gf ,f
       (w/stdout ,gf
         ,@body))))

(def readstring1 (s ? eof nil)
  (w/instring i s (read i eof)))

(def read (? x (stdin) eof nil)
  (if (isa x 'string) (readstring1 x eof) (sread x eof)))

; inconsistency between names of readfile[1] and writefile

(def readfile (name) (w/infile s name (drain (read s))))

(def readfile1 (name) (w/infile s name (read s)))

(def readall (src ? eof nil)
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def allchars (str)
  (tostring (whiler c (readc str nil) no
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

(def writefile (val file)
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val)

(def sym (x) (coerce x 'sym))

(def int (x ? b 10) (coerce x 'int b))

(mac rand-choice exprs
  `(case (rand ,(len exprs))
     ,@(let key -1
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  (w/uniq ga
    `(let ,ga nil
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

; rejects bytes >= 248 lest digits be overrepresented

(def rand-string (n)
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (with (nc 62 s (newstring n) i 0)
      (w/infile str "/dev/urandom"
        (while (< i n)
          (let x (readb str)
             (unless (> x 247)
               (= (s i) (c (mod x nc)))
               (++ i)))))
      s)))

(mac forlen (var s . body)
  `(for ,var 0 (- (len ,s) 1) ,@body))

(mac on (var s . body)
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      (w/uniq gs
        `(let ,gs ,s
           (forlen index ,gs
             (let ,var (,gs index)
               ,@body))))))

(def best (f seq)
  (if (no seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt)))
        wins)))

(def max args (best > args))
(def min args (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     `(with (,a ,x ,b ,y) (if (> ,a ,b) ,a ,b))))

(def most (f seq)
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a
; macroexpansion.

(def insert-sorted (test elt seq)
  (if (no seq)
       (list elt)
      (test elt (car seq))
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  (if (no seq)
       (list elt)
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq))
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

; Could make this look at the sig of f and return a fn that took the
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
  (with (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (no (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (set (nilcache args))
                        nil)))))))


(mac defmemo (name parms . body)
  `(safeset ,name (memo (fn ,parms ,@body))))

(def <= args
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c) (no (whitec c)))

(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

(def digit (c) (<= #\0 c #\9))

(def alphadig (c) (or (letter c) (digit c)))

(def punc (c)
  (in c #\. #\, #\; #\: #\! #\?))

(mac xloop (withses . body)
  (let w (pair withses)
    `((rfn next ,(map car w) ,@body) ,@(map cadr w))))

(def readline (? s (stdin))
  (aif (readc s)
    (string
     (accum a
       (xloop (c it)
         (if (is c #\return)
              (if (is (peekc s) #\newline)
                   (readc s))
             (is c #\newline)
              nil
              (do (a c)
                  (aif (readc s)
                        (next it)))))))))

; Don't currently use this but suspect some code could.

(mac summing (sumfn . body)
  (w/uniq (gc gt)
    `(let ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body)
       ,gc)))

(def sum (f xs)
  (let n 0
    (each x xs (++ n (f x)))
    n))

(def treewise (f base tree)
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree))
         (treewise f base (cdr tree)))))

(def carif (x) (if (atom x) x (car x)))

; Could prob be generalized beyond printing.

(def prall (elts ? init "" sep ", ")
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))

(def prs args
  (prall args "" #\space))

(def prsn args (apply prs args) (prn))

(def pad (x wanted-len ? side 'left pad-char #\space)
  (zap string x)
  (let padding (newstring (- wanted-len len.x) pad-char)
    (case side
      left (string padding x)
      right (string x padding))))

(def grid (xses (o strict nil)) ;dense lines of code for brevity
  (let lens (map [best > (map (fn (xs) (len:string:car:nthcdr _ xs)) xses)]
                 (range 0 (dec:best > (map len xses))))
    (when strict (let u (best > lens) (= lens (n-of len.lens u))))
    (each xs xses (apply prsn (map pad xs lens)))))

(def tree-subst (old new tree)
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def dotted (x)
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

(def load-table (file ? eof nil)
  (w/infile i file (read-table i eof)))

(def read-table (? i (stdin) eof nil)
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
  (w/infile i file
    (w/uniq eof
      (drain (read-table i eof) eof))))

(def save-table (h file)
  (writefile (tablist h) file))

(def write-table (h ? o (stdout))
  (write (tablist h) o))

(deftransform copy (xs)
  (if (atom xs)
      xs
      (cons (car xs) (copy (cdr xs)))))

(def abs (n)
  (if (< n 0) (- n) n))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

(def round (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2)
        ((if (> n 0) + -) base 1)
        base)))

(def nearest (n quantum)
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) (/ (apply + ns) (len ns)))

(def med (ns ? test >)
  ((sort test ns) (round (/ (len ns) 2))))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1)

(def sort (test seq)
  (if (alist seq)
      (mergesort test (copy seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))

; Destructive stable merge-sort, adapted from slib and improved
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

; Also by Eli.

(def merge (less? x y)
  (if (no x) y
      (no y) x
      (let lup nil
        (assign lup
                (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
                  (if (less? (car y) (car x))
                    (do (if r-x? (scdr r y))
                        (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                    ; (car x) <= (car y)
                    (do (if (no r-x?) (scdr r x))
                        (if (cdr x) (lup x (cdr x) y t) (scdr x y))))))
        (if (less? (car y) (car x))
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def bestn (n f seq)
  (firstn n (sort f seq)))

(def split (seq pos)
  (list (cut seq 0 pos) (cut seq pos)))

(mac time (expr)
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (prn "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  `(time (repeat 10 ,expr)))

(def union (f xs ys)
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

(= templates* (table))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name)
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name)
      (union (fn (x y) (is (car x) (car y)))
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (pair fields)))
             (templates* ',name))))

(def inst (tem . args)
  (let x (table)
    (each (k v) (if (acons tem) tem (templates* tem))
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem ? str (stdin))
  (templatize tem (read str)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  (with (x (inst tem) fields (if (acons tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))
       (w/infile in file (readall in))))


(def number (n) (in (type n) 'int 'num))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))
(def hours-since (t1)   (/ (since t1) 3600))
(def days-since (t1)    (/ (since t1) 86400))

; could use a version for fns of 1 arg at least

(def cache (timef valf)
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac defcache (name lasts . body)
  `(safeset ,name (cache (fn () ,lasts)
                         (fn () ,@body))))

(mac errsafe (expr)
  `(on-err (fn (c) nil)
           (fn () ,expr)))

(def saferead (arg) (errsafe:read arg))

(def safe-load-table (filename)
  (or (errsafe:load-table filename)
      (table)))

(def ensure-dir (path)
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

(def date (? s (seconds))
  (rev (nthcdr 3 (timedate s))))

(def datestring (? s (seconds))
  (let (y m d) (date s)
    (string y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d)))

(def count (test x)
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str ? limit 80)
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def rand-elt (seq)
  (seq (rand (len seq))))

(mac until (test . body)
  `(while (no ,test) ,@body))

(def before (x y seq ? i 0)
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def orf fns
  (fn args
    ((afn (fs)
       (and fs (or (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def andf fns
  (fn args
    ((afn (fs)
       (if (no fs)       t
           (no (cdr fs)) (apply (car fs) args)
                         (and (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def atend (i s)
  (> i (- (len s) 2)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor args `(no (or ,@args)))
(= neither nor)

; Consider making the default sort fn take compare's two args (when do
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

; (def only (f g . args) (aif (apply g args) (f it)))

(def only (f)
  (fn args (if (car args) (apply f args))))

; Could combine with firstn if put f arg last, default to (fn (x) t).

(def retrieve (n f xs)
  (if (no n)                 (keep f xs)
      (or (<= n 0) (no xs))  nil
      (f (car xs))           (cons (car xs) (retrieve (- n 1) f (cdr xs)))
                             (retrieve n f (cdr xs))))

(def dedup (xs)
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (set (h x))))
    (rev acc)))

(def single (x) (and (acons x) (no (cdr x))))

(def intersperse (x ys)
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(defgeneric freq (seq)
  (ret ans (table)
    (each elem seq
      (++ (ans elem 0)))))

(= count-up sortable:freq)

(def commonest (seq)
  (with (winner nil n 0)
    (each (k v) (freq seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(def reduce (f xs)
  (if (cddr xs)
      (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

(def rreduce (f xs)
  (if (cddr xs)
      (f (car xs) (rreduce f (cdr xs)))
      (apply f xs)))

(let argsym (uniq)

  (def parse-format (str)
    (accum a
      (with (chars nil  i -1)
        (w/instring s str
          (whilet c (readc s)
            (case c
              #\# (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (a (read s)))
              #\~ (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (readc s)
                      (a (list argsym (++ i))))
                  (push c chars))))
         (when chars
           (a (coerce (rev chars) 'string))))))

  (mac prf (str . args)
    `(let ,argsym (list ,@args)
       (pr ,@(parse-format str))))
)

(def load (file)
  (w/infile f file
    (w/uniq eof
      (whiler e (read f eof) eof
        (eval e)))))

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
  `(let ,var (table) ,@body ,var))

(def ero args
  (w/stdout (stderr)
    (each a args
      (write a)
      (writec #\space))
    (writec #\newline))
  (car args))

(def queue () (annotate 'queue (list nil nil 0)))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

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

(def qlen (q) (rep.q 2))
(def qlist (q) (car rep.q))

(def enq-limit (val q ? limit 1000)
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  ((sort > ns) (trunc (/ (len ns) 2))))

(mac noisy-each (n var val . body)
  (w/uniq (gn gc)
    `(with (,gn ,n ,gc 0)
       (each ,var ,val
         (when (multiple (++ ,gc) ,gn)
           (pr ".")
           (flushout)
           )
         ,@body)
       (prn)
       (flushout))))

(mac point (name . body)
  (w/uniq (g p)
    `(ccc (fn (,g)
            (let ,name (fn (? ,p nil) (,g ,p))
              ,@body)))))

(mac catch body
  `(point throw ,@body))

(def downcase (x)
  (let downc (fn (c)
               (let n (coerce c 'int)
                 (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                     (coerce (+ n 32) 'char)
                     c)))
    (case (type x)
      string (map downc x)
      char   (downc x)
      sym    (sym (map downc (coerce x 'string)))
             (err "Can't downcase" x))))

(def upcase (x)
  (let upc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                   (coerce (- n 32) 'char)
                   c)))
    (case (type x)
      string (map upc x)
      char   (upc x)
      sym    (sym (map upc (coerce x 'string)))
             (err "Can't upcase" x))))

(def inc (x ? n 1)
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
  (if (> start end)
      nil
      (cons start (range (inc start) end))))

(def mismatch (s1 s2)
  (catch
    (on c s1
      (when (isnt c (s2 index))
        (throw index)))))

(def memtable (ks)
  (let h (table)
    (each k ks (set (h k)))
    h))

(= bar* " | ")

(mac w/bars body
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (set ,needbars)
                                 (pr ,out))))))
                  body)))))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(mac trav (x . fs)
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(mac or= (place expr)
  (let (binds val setter) (setforms place)
    `(atwiths ,binds
       (or ,val (,setter ,expr)))))

; Could take n args, but have never once needed that.
(defgeneric* iso (x y)
  (or (is x y)
      (and (acons x)
           (acons y)
           (iso car.x car.y)
           (iso cdr.x cdr.y))))
(= be iso)

(defmethod iso (x y) table
  (and (isa x 'table)
       (isa y 'table)
       (is (len keys.x) (len keys.y))
       (all
         (fn((k v))
           (iso y.k v))
         tablist.x)))

(defgeneric len (x)
  ($.length x))

(defmethod len (x) string
  ($.string-length x))

(defmethod len (x) table
  ($.hash-table-count x))

(defmethod len (x) queue
  (qlen x))



(def set-coercer (to from fun)
  (let cnv (or coerce*.to (= coerce*.to (table)))
    (= cnv.from fun)))

(mac defcoerce (to from parms . body)
  `(set-coercer ',to ',from (fn ,parms ,@body)))

(mac defcall (type-name parms . body)
  (w/uniq (fnobj args)
    `(defcoerce fn ,type-name (,fnobj)
       (fn ,args (apply (fn ,parms ,@body) ,fnobj ,args)))))

(defcoerce cons table (h)
  (tablist h))

(defcoerce table cons (al)
  (listtab al))

(defcoerce cons queue (q)
  (qlist q))



(defgeneric serialize(x)
  x)

(defmethod serialize(x) string
  x)

(defmethod serialize(x) cons
  (map serialize x))

(defmethod serialize(x) table
  (list 'table
    (accum a
      (maptable (fn (k v)
                  (a (list k serialize.v)))
                x))))

; can't use defgeneric; everything is likely a list when serialized
(or= vtables*!unserialize (table))
(def unserialize(x)
  (aif (vtables*!unserialize type*.x)   (it x)
    (acons x)   (map unserialize x)
                x))

(def type*(x)
  (if (and (pair? x)
           (isa car.x 'sym))
    car.x
    type.x))

(def pair?(l)
  (and (acons l)
       (acons:cdr l)
       (~acons:cddr l)))

(defmethod unserialize(x) table
  (w/table h
    (map (fn ((k v)) (= h.k unserialize.v))
         cadr.x)))



(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  `(= (hooks* ',name) (fn ,@rest)))

(mac out (expr) `(pr ,(tostring (eval expr))))

; if renamed this would be more natural for (map [_ user] pagefns*)

(def get (index) [_ index])

(= savers* (table))

(mac fromdisk (var file init load save)
  (w/uniq (gf gv)
    `(unless (bound ',var)
       (do1 (= ,var (iflet ,gf (file-exists ,file)
                               (,load ,gf)
                               ,init))
            (= (savers* ',var) (fn (,gv) (,save ,gv ,file)))))))

(mac diskvar (var file)
  `(fromdisk ,var ,file nil readfile1 writefile))

(mac disktable (var file)
  `(fromdisk ,var ,file (table) load-table save-table))

(mac todisk (var ? expr var)
  `((savers* ',var)
    ,(if (is var expr) var `(= ,var ,expr))))


(mac evtil (expr test)
  (w/uniq gv
    `(let ,gv ,expr
       (while (no (,test ,gv))
         (= ,gv ,expr))
       ,gv)))

(def rand-key (h)
  (if (empty h)
      nil
      (let n (rand (len h))
        (catch
          (each (k v) h
            (when (is (-- n) -1)
              (throw k)))))))

(def ratio (test xs)
  (if (empty xs)
      0
      (/ (count test xs) (len xs))))



(= scheme-f (read "#f"))
(= scheme-t (read "#t"))

(= ac-global-name $.ac-global-name)

(mac ac-set-global (name val)
  (w/uniq (gname v)
    `(with (,gname (ac-global-name ,name)
            ,v ,val)
       ($:namespace-set-variable-value! ,gname ,v)
       nil)))

(= defined-variables* (table))
(def ac-defined-var?(name)
  (if defined-variables*.name scheme-t scheme-f))

(mac defvar (name impl)
  `(do (ac-set-global ',name ,impl)
       (set (defined-variables* ',name))
       nil))

(mac defvar-impl (name)
  (let gname (ac-global-name name)
    `($ ,gname)))

(mac undefvar (name)
  `(do (wipe (defined-variables* ',name))
       (ac-set-global ',name nil)))

(mac implicit (name ? val nil)
  `(do (defvar ,name ($.make-parameter ,val))
       (mac ,(sym (string "w/" name)) (v . body)
         (w/uniq (param gv gf)
           `(with (,param (defvar-impl ,',name)
                   ,gv ,v
                   ,gf (fn () ,@body))
              ($ (parameterize ((,param ,gv)) (,gf))))))))



; any logical reason I can't say (push x (if foo y z)) ?
;   eval would have to always ret 2 things, the val and where it came from
; idea: implicit tables of tables; setf empty field, becomes table
;   or should setf on a table just take n args?

; idea: use constants in functional position for currying?
;       (1 foo) would mean (fn args (apply foo 1 args))
; another solution would be to declare certain symbols curryable, and
;  if > was, >_10 would mean [> _ 10]
;  or just say what the hell and make _ ssyntax for currying
; idea: make >10 ssyntax for [> _ 10]
; solution to the "problem" of improper lists: allow any atom as a list
;  terminator, not just nil.  means list recursion should terminate on
;  atom rather than nil, (def empty (x) (or (atom x) (is x "")))
; table should be able to take an optional initial-value.  handle in sref.
; warn about code of form (if (= )) -- probably mean is
; warn when a fn has a parm that's already defined as a macro.
;   (def foo (after) (after))
; idea: a fn (nothing) that returns a special gensym which is ignored
;  by map, so can use map in cases when don't want all the vals
; idea: anaph macro so instead of (aand x y) say (anaph and x y)
; idea: foo.bar!baz as an abbrev for (foo bar 'baz)
;  or something a bit more semantic?
; could uniq be (def uniq () (annotate 'symbol (list 'u))) again?
; idea: use x- for (car x) and -x for (cdr x)  (but what about math -?)
; idea: get rid of strings and just use symbols
; could a string be (#\a #\b . "") ?
; better err msg when , outside of a bq
; idea: parameter (p foo) means in body foo is (pair arg)
; idea: make ('string x) equiv to (coerce x 'string) ?  or isa?
;   quoted atoms in car valuable unused semantic space
; idea: if (defun foo (x y) ...), make (foo 1) return (fn (y) (foo 1 y))
;   probably would lead to lots of errors when call with missing args
;   but would be really dense with . notation, (foo.1 2)
; or use special ssyntax for currying: (foo@1 2)
; remember, can also double; could use foo::bar to mean something
; wild idea: inline defs for repetitive code
;  same args as fn you're in
; variant of compose where first fn only applied to first arg?
;  (> (len x) y)  means (>+len x y)
; use ssyntax underscore for a var?
;  foo_bar means [foo _ bar]
;  what does foo:_:bar mean?
; matchcase
; idea: atable that binds it to table, assumes input is a list
; crazy that finding the top 100 nos takes so long:
;  (let bb (n-of 1000 (rand 50)) (time10 (bestn 100 > bb)))
;  time: 2237 msec.  -> now down to 850 msec
