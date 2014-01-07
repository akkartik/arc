(require mzscheme)

(require "ac.scm")

(define-syntax test-scm
  (syntax-rules(:valueof :should)
    ((test msg :valueof expr :should predicate args ...)
     (let ((got expr))
       (if (predicate got args ...)
         (begin
           (display ". ")
           (display msg)
           (newline))
         (begin
           (display "F ")
           (display msg)
           (newline)
           (display "  got ")
           (display got)
           (newline)))))))

; evaluates expr twice
(define-syntax test-ac
  (syntax-rules(:valueof :should)
    ((test msg :valueof expr :should predicate args ...)
     (let ((got (arc-eval 'expr))
           (ans (arc-eval '(predicate expr args ...))))
       (if (ac-true? ans)
         (begin
           (display ". ")
           (display msg)
           (newline))
         (begin
           (display "F ")
           (display msg)
           (newline)
           (display "  got ")
           (display got)
           (newline)))))))

(define (ac-true? x)
  (and x
       (not (eq? x ()))))

(define-syntax pending
  (syntax-rules(:valueof :should)
    ((test msg :valueof expr :should predicate args ...)
     (begin
       (display "X ")
       (display msg)
       (newline)))))

(define be equal?)



(test-scm "vars-in-paramlist returns simple params"
  :valueof (vars-in-paramlist '(a b c))
  :should be '(a b c))

(test-scm "vars-in-paramlist returns varargs param"
  :valueof (vars-in-paramlist 'a)
  :should be '(a))

(test-scm "vars-in-paramlist returns rest param"
  :valueof (vars-in-paramlist '(a b c . d))
  :should be '(a b c d))

(test-scm "vars-in-paramlist returns optional params"
  :valueof (vars-in-paramlist '(a ? b 2 c 3 . d))
  :should be '(a b c d))

(test-scm "keyword-args works"
  :valueof (keyword-args '(1 2 :a 3) '(a))
  :should be '((a . 3)))

(test-scm "keyword-args works with vararg args"
  :valueof (keyword-args '(1 2 :a 3) 'a)
  :should be '((a 3)))

(test-scm "keyword-args works with rest args"
  :valueof (keyword-args '(1 2 :b 3) '(a . b))
  :should be '((b 3)))

(test-scm "keyword-args works with unknown keywords"
  :valueof (keyword-args '(1 2 :b 3) 'a)
  :should be '())

(test-scm "keyword-args works with unknown keywords - 2"
  :valueof (keyword-args '(1 2 :b) 'a)
  :should be '())

(test-scm "keyword-args works inside destructured lists"
  :valueof (keyword-args '((1 :a 2)) '((a b)))
  :should be '((a . 2)))

(test-scm "keyword-args works inside destructured lists after keyword args"
  :valueof (keyword-args '(:c 3 (1 :a 2)) '((a b) c))
  :should be '((c . 3) (a . 2)))

(test-scm "keyword-args works inside destructured lists before keyword args"
  :valueof (keyword-args '((1 :a 2) :c 3) '(c (a b)))
  :should be '((a . 2) (c . 3)))

(test-scm "keyword-args looks for keyword args only inside corresponding destructured param"
  :valueof (keyword-args '(:a 3) '((a b)))
  :should be '())

(test-scm "strip-keyword-args works"
  :valueof (strip-keyword-args '(1 2 :a 3 4) '(a))
  :should be '(1 2 4))

(test-scm "strip-keyword-args works on vararg args"
  :valueof (strip-keyword-args '(1 2 :a 3 4) 'a)
  :should be '(1 2))

(test-scm "strip-keyword-args works on rest args"
  :valueof (strip-keyword-args '(1 2 :b 3 4) '(a . b))
  :should be '(1 2))

(test-scm "strip-keyword-args works on destructured args"
  :valueof (strip-keyword-args '((1 2 :b 3 4)) '((a b)))
  :should be '((1 2 4)))

(test-scm "strip-keyword-args works on destructured rest args"
  :valueof (strip-keyword-args '((1 2 :b 3 4)) '((a . b)))
  :should be '((1 2)))

(test-scm "strip-keyword-args works with unknown keywords"
  :valueof (strip-keyword-args '(1 2 :b 3) 'a)
  :should be '(1 2 :b 3))

(test-scm "strip-keyword-args works with unknown keywords - 2"
  :valueof (strip-keyword-args '(1 2 :b) 'a)
  :should be '(1 2 :b))

(let ((args '(1 2 3)))
  ; hacky attempt at not breaking mutating functions; see ac-fn
  (test-scm "strip-keyword-args returns unchanged arg list when keywords are absent"
    :valueof (strip-keyword-args args 'a)
    :should eq? args))

(test-scm "optional-param-alist works - 1"
  :valueof (optional-param-alist 'a)
  :should be '())
(test-scm "optional-param-alist 1"
  :valueof (optional-param-alist '(a b c))
  :should be '())
(test-scm "optional-param-alist 2"
  :valueof (optional-param-alist '(a b c ? d e))
  :should be '((d . e)))
(test-scm "optional-param-alist 4"
  :valueof (optional-param-alist '(a b c ? d e f nil))
  :should be '((d . e) (f . nil)))
(test-scm "optional-param-alist 5"
  :valueof (optional-param-alist '(a b c ? d e f nil . g))
  :should be '((d . e) (f . nil)))
(test-scm "optional-param-alist 6"
  :valueof (optional-param-alist '(a (b c ? d e) ? f g . h))
  :should be '((d . e) (f . g)))

(test-scm "get-arg works for missing arg"
  :valueof (get-arg 'c 'a () 'a 1 ())
  :should be #f)

(test-scm "get-arg works for required args"
  :valueof (get-arg 'c '(a b c) () () '(1 2 3) ())
  :should be 3)

(test-scm "get-arg works for varargs"
  :valueof (get-arg 'a 'a () 'a '(1 2 3) ())
  :should be '(1 2 3))

(test-scm "get-arg works for rest args"
  :valueof (get-arg 'd '(a (b c) . d) () 'd '(1 (2 3) 4 5 6) ())
  :should be '(4 5 6))

(test-scm "get-arg works for destructured args"
  :valueof (get-arg 'b '(a (b c)) () ()
                       '(1 (2 3)) ())
  :should be 2)

(test-scm "get-arg works for destructured args - 2"
  :valueof (get-arg 'f '(a (b c (d e f))) () ()
                       '(1 (2 3 (4 5 6))) ())
  :should be 6)

(test-scm "get-arg works for missing destructured args"
  :valueof (get-arg 'x '(a (b c (d e f))) () ()
                       '(1 (2 3 (4 5 6))) ())
  :should be #f)

(test-scm "get-arg works for destructuring and rest args"
  :valueof (get-arg 'g '(a (b c (d e f) . g)) () 'g
                       '(1 (2 3 (4 5 6) 7 8)) ())
  :should be '(7 8))

(test-scm "get-arg works for destructuring and rest args - 2"
  :valueof (get-arg 'f '(a (b c (d e f) . g)) () 'g
                       '(1 (2 3 (4 5 6) 7 8)) ())
  :should be 6)

(test-scm "get-arg works for destructuring and rest args - 3"
  :valueof (get-arg 'x '(a (b c (d e f) . g)) () 'g
                       '(1 (2 3 (4 5 6) 7 8)) ())
  :should be #f)

(test-scm "get-arg works for destructuring and rest args - 4"
  :valueof (get-arg 'f '(a (b c (d e . f) . g)) () 'g
                       '(1 (2 3 (4 5 6 7) 7 8)) ())
  :should be '(6 7))

(test-scm "get-arg works for destructuring and rest args - 5"
  :valueof (get-arg 'f '(a (b c (d e . f) . g)) () 'g
                       '(1 (2 3 (4 5) 7 8)) ())
  :should be '())

(test-scm "get-arg works for keyword args"
  :valueof (get-arg 'a '(a (b c (d e f) . g)) () 'g
                       '((2 3 (4 5 6) 7 8)) '((a . 1)))
  :should be 1)

(test-scm "get-arg works on args with keywords"
  :valueof (get-arg 'd '(a (b c (d e f) . g)) () 'g
                       '((2 3 (4 5 6) 7 8)) '((a . 1)))
  :should be 4)

(test-scm "get-arg for missing rest args"
  :valueof (get-arg 'rest 'rest () 'rest () ())
  :should be ())

(test-scm "get-arg returns an illegal literal for missing, hopefully optional, params - 1"
  :valueof (get-arg 'a '(a) () () () ())
  :should be #f)

(test-scm "get-arg returns an illegal literal for missing, hopefully optional, params - 2"
  :valueof (get-arg 'e '(a b c e f . d) () 'd '(1 2 3) ())
  :should be #f)

(test-scm "get-arg skips optional params when rest param is present"
  :valueof (get-arg 'e '(a b c e f . d) '(e f) 'd '(1 2 3 4 5 6 7) ())
  :should be #f)

(test-scm "get-arg gets optional params from keyword args when rest param is present"
  :valueof (get-arg 'e '(a b c e f . d) '(e f) 'd '(1 2 3 4 5 6 7) '((e . 23)))
  :should be 23)

(test-scm "get-arg gets rest param after optionals"
  :valueof (get-arg 'd '(a b c e f . d) '(e f) 'd '(1 2 3 4 5 6 7) ())
  :should be '(4 5 6 7))

(test-scm "get-arg gets rest param after optionals from keyword arg"
  :valueof (get-arg 'd '(a b c e f . d) '(e f) 'd '(1 2 3) '((d 4 5 6 7)))
  :should be '(4 5 6 7))

(test-scm "get-arg gets rest param after destructured optionals"
  :valueof (get-arg 'c '((a ? b () . c)) () () '((1 2 3)) ())
  :should be '(2 3))



(require "brackets.scm")
(use-bracket-readtable)
(aload "arc.arc")

(arc-eval '(def satisfy (x f) (f x)))

(arc-eval '(assign foo (fn args 34)))
(test-ac "simple varargs fn"
  :valueof (foo 1 2 3)
  :should be 34)

(arc-eval '(assign foo (fn() 34)))
(test-ac "simple fn"
  :valueof (foo)
  :should be 34)

(arc-eval '(assign foo (fn args args)))
(test-ac "just a rest arg without parens"
    :valueof (foo 3 4 5)
    :should be '(3 4 5))

(arc-eval '(assign foo (fn(a . b) b)))
(test-ac "dotted rest"
  :valueof (foo 3 4)
  :should be '(4))

(test-ac "rest args are optional"
  :valueof (foo 3)
  :should be ())

(arc-eval '(assign foo (fn((a b)) b)))
(test-ac "destructured args"
  :valueof (foo '(3 4))
  :should be 4)

(arc-eval '(assign foo (fn((a b) . c) b)))
(test-ac "destructured args + dotted rest"
  :valueof (foo '(3 4) 5)
  :should be 4)

(arc-eval '(assign foo (fn(a ? b ()) (cons a b))))
(test-ac "optional param"
  :valueof (foo 3)
  :should be '(3))

(arc-eval '(assign foo (fn(a ? b 4) (cons a b))))
(test-ac "optional param with a default"
  :valueof (foo 3)
  :should be '(3 . 4))

(test-ac "optional named param"
  :valueof (foo :a 3)
  :should be '(3 . 4))

(test-ac "optional arg without naming"
  :valueof (foo 3 2)
  :should be '(3 . 2))

(test-ac "named args should override optional params"
  :valueof (foo 3 :b 2)
  :should be '(3 . 2))

(test-ac "nil overrides default for optional param without naming"
  :valueof (foo 3 '())
  :should be '(3))

(arc-eval '(assign foo (fn(a ? b 4 c ()) (cons b c))))
(test-ac "multiple optional params"
  :valueof (foo 3)
  :should be '(4))

(test-ac "allow optional named args out of order"
  :valueof (foo 3 :c 2 :b ())
  :should be '(() . 2))

(arc-eval '(assign foo (fn(a ? b 4 c b) (cons b c))))
(test-ac "optional params know of previous params"
  :valueof (foo 3)
  :should be '(4 . 4))

(test-ac "allow optional args in order without naming"
  :valueof (foo 3 () 2)
  :should be '(() . 2))

(test-ac "allow optional args in order without naming"
  :valueof (foo 3 nil 2)
  :should be '(() . 2))

(arc-eval '(assign foo (fn((a ? b ())) (cons a b))))
(test-ac "destructured args can also be optional"
  :valueof (foo '(3 4))
  :should be '(3 . 4))

(arc-eval '(assign foo (fn(a . b) b)))
(test-ac "rest args can be named"
  :valueof (foo 3 :b 4 5)
  :should be '(4 5))

(arc-eval '(assign foo (fn(a ? b 3 . c) (cons b c))))
(test-ac "optional + named rest args"
  :valueof (foo 2 :c 3)
  :should be '(3 3))

(test-ac "optional + named rest args - 2"
  :valueof (foo 2 :b 4 :c 3)
  :should be '(4 3))

(test-ac "optional + named rest args - 3"
  :valueof (foo 2 :b 4 3)
  :should be '(4 3))

(arc-eval '(assign foo (fn(a ? b () c 3 . body) (cons b (cons c body)))))
(test-ac "call with unnamed optional args gives precedence to rest params"
  :valueof (foo 23 24 25)
  :should be '(() 3 24 25))

(arc-eval '(assign foo (fn(a ? b () c 3 . body) (cons b (cons c body)))))
(test-ac "call with unnamed optional args can take handle rest keyword arg"
  :valueof (foo :body 23 24 25)
  :should be '(() 3 23 24 25))

(test-ac "call with some named optional and rest args"
  :valueof (foo 3 :c 4 :body 4 5)
  :should be '(() 4 4 5))

(test-ac "call with keyword syms that are not keyword args"
  :valueof (foo 3 :c 4 :body 4 :bar)
  :should be '(() 4 4 :bar))

(test-ac "call with keyword syms that are not keyword args - 2"
  :valueof (foo 3 :c :bar :body 4 5)
  :should be '(() :bar 4 5))

(arc-eval '(assign foo (fn(a ? b 2 c (fn() (+ 1 b)) . body) (c))))
(test-ac "defaults compile properly"
  :valueof (foo 3 :b 4)
  :should be 5)

(test-ac "destructured optionals"
  :valueof ((fn ((a ? b 34)) (list a b)) '(3))
  :should be '(3 34))

(test-ac "destructured optional keywords"
  :valueof ((fn ((a ? b 34)) (list a b)) '(2 :a 3))
  :should be '(3 2))

(test-ac "destructured keywords require destructuring"
  :valueof ((fn ((a b)) (list a b)) '(2 3) :a 4)
  :should be '(2 3))



(arc-eval '((fn() (mac foo () 34) (mac bar () (foo)))))
; call to foo inside bar is not macro-expanded yet
(test-ac "macro calls work after macroexpansion"
  :valueof (bar)
  :should be 34)



(aload "libs.arc")

(define (_contain str substr)
  (_findsubseq substr str))

(test-ac "sym.nil works"
  :valueof sym.nil
  :should be nil)

(test-ac "iso works on nil"
  :valueof (iso () ())
  :should be t)

(test-ac "iso works on nil - 2"
  :valueof (iso () t)
  :should be ())

(test-ac "iso works on nil - 3"
  :valueof (iso () 3)
  :should be ())

(test-ac "iso works on tables"
  :valueof (iso (obj 1 2 3 4) (obj 3 4 1 2))
  :should be t)

(test-ac "iso works on queues"
  :valueof (with (q1 (queue)
                  q2 (queue))
              (enq 1 q1)
              (enq 1 q2)
              (enq 2 q1)
              (enq 2 q2)
              (enq 3 q1)
              (enq 3 q2)
              (iso q1 q2))
  :should be t)

(test-ac "len works on nil"
  :valueof (len ())
  :should be 0)

(test-ac "len works on tables"
  :valueof (len (obj 1 2 3 4 5 6))
  :should be 3)

(test-ac "len works on queues"
  :valueof (let q (queue)
             (enq 1 q)
             (enq 2 q)
             (enq 3 q)
             (len q))
  :should be 3)

(test-ac "some 1 - string"
  :valueof (some (testify #\?) "a?b")
  :should be t)

(test-ac "some 2"
  :valueof (some (testify #\?) "abc")
  :should be ())

(test-ac "some 3 - list"
  :valueof (some testify.1 '(1 2 3))
  :should be t)

(test-ac "some 4"
  :valueof (some testify.4 '(1 2 3))
  :should be ())

(test-ac "some 5 - improper lists"
  :valueof (some odd '(2 4 5 . 6))
  :should be t)

(test-ac "some 6"
  :valueof (some testify.6 '(2 4 5 . 6))
  :should be t)

(test-ac "some 7"
  :valueof (some testify.7 '(2 4 5 . 6))
  :should be nil)

(test-ac "mem 1"
  :valueof (mem 6 '(2 4 5 6 7))
  :should be '(6 7))

(test-ac "mem 2 - improper lists"
  :valueof (mem 6 '(2 4 5 6 . 7))
  :should be '(6 . 7))

(test-ac "mem 3"
  :valueof (mem 6 '(2 4 5 . 6))
  :should be 6)

(test-ac "mem 4"
  :valueof (mem 7 '(2 4 5 . 6))
  :should be nil)

(test-ac "find 1 - string"
  :valueof (find #\? "a?b")
  :should be #\?)

(test-ac "find 2"
  :valueof (find #\? "abc")
  :should be ())

(test-ac "find 3 - list"
  :valueof (find #\? '(#\?))
  :should be #\?)

(test-ac "find 4"
  :valueof (find #\? '(1 2))
  :should be ())

(test-ac "find 5 - arbitrary predicate"
  :valueof (find odd '(1 2))
  :should be 1)

(test-ac "find 6"
  :valueof (find odd '(2 3))
  :should be 3)

(test-ac "find 7"
  :valueof (find odd '(3))
  :should be 3)

(test-ac "find 8"
  :valueof (find odd '(2))
  :should be nil)

(test-ac "find 9 - returns first match"
  :valueof (find odd '(2 3 5))
  :should be 3)

(test-ac "find 10 - improper lists"
  :valueof (find 'a '(a b . c))
  :should be 'a)

(test-ac "find 11"
  :valueof (find 'c '(a b . c))
  :should be 'c)

(test-ac "find 12"
  :valueof (find 'd '(a b . c))
  :should be nil)

(test-ac "all 1"
  :valueof (all odd '(1 3 5))
  :should be t)

(test-ac "all 2"
  :valueof (all odd '(1 2 3))
  :should be ())

(test-ac "all 3 - testify works"
  :valueof (all 1 '(1 1 1))
  :should be t)

(test-ac "all 4 - empty list"
  :valueof (all 1 '())
  :should be t)

(test-ac "all 5 - empty string"
  :valueof (all 1 "")
  :should be t)

(test-ac "rem 1"
  :valueof (rem odd '(1 2 3 4))
  :should be '(2 4))

(test-ac "rem 2 - string"
  :valueof (rem #\? "a?b")
  :should be "ab")

(test-ac "keep 1 - string"
  :valueof (keep (testify #\?) "a?b")
  :should be "?")

(arc-eval '(implicit implicit0 10))
(arc-eval '(def g() ++.implicit0)) ; fails if this is in a do block with the implicit decl. Bug?
(test-ac "implicit"
  :valueof (list (w/implicit0 20 (g) implicit0) implicit0)
  :should be '(21 10))

(test-ac "copy works on lists"
  :valueof (copy '(1 2 3))
  :should be '(1 2 3))

(test-ac "copy works on ()"
  :valueof (copy ())
  :should be ())

(test-ac "copy works on strings"
  :valueof (copy "abc")
  :should be "abc")

(test-ac "copy works on tables"
  :valueof (copy (obj 1 2 3 4))
  :should be (obj 1 2 3 4))

(test-ac "copy works on tables"
  :valueof (copy (obj 1 2 3 4))
  :should be (obj 1 2 3 4))

(test-ac "copy doesn't return the same object"
  :valueof (let s (obj 1 2 3 4) (is s copy.s))
  :should be ())

(test-ac "each works on nil"
  :valueof (each x () x)
  :should be ())

(test-ac "each works when treating lists as trees"
  :valueof (accum acc
             (each x (tree '(1 2 (3 4)))
               (acc x)))
  :should be '((1 2 (3 4))
               1
               (2 (3 4))
               2
               ((3 4))
               (3 4)
               3
               (4)
               4
               ()
               ()))

(test-ac "each works when treating lists as trees - 2"
  :valueof (accum acc
             (each x '(1 2 (3 4)) :like 'tree
               (acc x)))
  :should be '((1 2 (3 4))
               1
               (2 (3 4))
               2
               ((3 4))
               (3 4)
               3
               (4)
               4
               ()
               ()))

(test-ac "each works when treating lists as code"
  :valueof (accum acc
             (each x (code '(1 2 (3 4)))
               (acc x)))
  :should be '((1 2 (3 4))
               1
               2
               (3 4)
               3
               4))

(test-ac "each works when treating lists as code - 2"
  :valueof (accum acc
             (each x '(1 2 (3 4)) :like 'code
               (acc x)))
  :should be '((1 2 (3 4))
               1
               2
               (3 4)
               3
               4))

(test-ac "serialize works on nil"
  :valueof (serialize ())
  :should be ())

(test-ac "serialize works on lists"
  :valueof (serialize '(1 2 3))
  :should be '(1 2 3))

(test-ac "serialize works on strings"
  :valueof (serialize "abc")
  :should be "abc")

(test-ac "serialize works on tables"
  :valueof (serialize (obj 1 2 3 4))
  :should be '(tagged table ((3 4) (1 2))))

(test-ac "unserialize complements serialize for nil"
  :valueof (unserialize:serialize ())
  :should be ())

(test-ac "unserialize complements serialize for lists"
  :valueof (unserialize:serialize '(1 2 3))
  :should be '(1 2 3))

(test-ac "unserialize complements serialize for strings"
  :valueof (unserialize:serialize "abc")
  :should be "abc")

(test-ac "unserialize complements serialize for empty tables"
  :valueof (unserialize:serialize (table))
  :should be (table))

(test-ac "unserialize complements serialize for tables"
  :valueof (unserialize:serialize (obj 1 2 3 4))
  :should be (obj 1 2 3 4))

(test-ac "serialize operates on tables inside lists"
  :valueof (serialize `(1 ,(table) 2 3))
  :should be '(1 (tagged table ()) 2 3))

(test-ac "unserialize complements serialize for tables inside lists"
  :valueof (unserialize:serialize `(1 ,(table) 2 3))
  :should be `(1 ,(table) 2 3))

(test-ac "serialize operates on nested tables"
  :valueof (serialize (obj 1 (table) 2 3))
  :should be '(tagged table ((2 3) (1 (tagged table ())))))

(test-ac "unserialize complements serialize for nested tables"
  :valueof (unserialize:serialize (obj 1 (table) 2 3))
  :should be (obj 1 (table) 2 3))

(test-ac "freq works"
  :valueof (freq '(1 2 3 2 1))
  :should be (obj 1 2 2 2 3 1))

(test-ac "freq works on strings"
  :valueof (freq "abracadabra")
  :should be (obj #\a 5 #\b 2 #\c 1 #\d 1 #\r 2))

(test-ac "count-up works"
  :valueof (count-up '(1 2 3 3 2))
  :should be '((3 2) (2 2) (1 1)))

(test-ac "map works"
  :valueof (map [+ 1 _] '(1 2 3))
  :should be '(2 3 4))

(test-ac "map works on multiple lists"
  :valueof (map * '(1 2 3) '(1 2 3 4))
  :should be '(1 4 9))

(test-ac "map works with one string"
  :valueof (map (obj #\a #\m #\b #\n #\c #\o)
                "abc")
  :should be "mno")

(test-ac "map works with multiple strings"
  :valueof (map (fn (a b) (min a b))
                "dave" "john")
  :should be "dahe")

(test-ac "subst works on lists"
  :valueof (subst 1 2 '(1 2 3))
  :should be '(2 2 3))

(test-ac "subst can take functions"
  :valueof (rep:subst atom&odd 2 (tree '(1 2 3 (4 5 . 6) . 7)))
  :should be '(2 2 2 (4 2 . 6) . 2))

(test-ac "subst can take functions - 2"
  :valueof (rep:subst atom&odd [+ _ 1] (tree '(1 2 3 (4 5 . 6) . 7)))
  :should be '(2 2 4 (4 6 . 6) . 8))

(test-ac "subst can replace subtrees"
  :valueof (rep:subst '(1 2) '(3 4) (tree '((1 2) (5 6))))
  :should be '((3 4) (5 6)))

(test-ac "rev works"
  :valueof (rev '(1 2 (4 5)))
  :should be '((4 5) 2 1))

(test-ac "sumlist works"
  :valueof (sumlist len '("abc" "de" "f"))
  :should be 6)

(test-ac "map1 works"
  :valueof (map len '("abc" "de" "f"))
  :should be '(3 2 1))

(test-ac "mapn works"
  :valueof (mapn idfn 1 10)
  :should be (range 1 10))

(test-ac "sum works"
  :valueof (sum len '("abc" "de" "f"))
  :should be 6)

(test-ac "sumn works"
  :valueof (sumn idfn 0 4)
  :should be 10)



(test-ac "pushnew 1 - improper lists"
  :valueof (ret x 3
             (pushnew 2 x))
  :should be '(2 . 3))

(test-ac "pushnew 2"
  :valueof (ret x 3
             (pushnew 3 x))
  :should be 3)

(test-ac "pushnew 3"
  :valueof (ret x '(3)
             (pushnew nil x))
  :should be (list nil 3))

(test-ac "firsttime works"
  :valueof (ret counter 0
             (let lock nil
               (firsttime lock
                  ++.counter)
               (firsttime lock
                  ++.counter)))
  :should be 1)

(test-ac "updating doesn't update unnecessarily"
  :valueof (ret counter 0
             (let a 0
               (updating a :with 0
                  ++.counter)))
  :should be 0)

(test-ac "updating updates when necessary"
  :valueof (ret counter 0
             (let a 0
               (updating a 1
                  ++.counter)))
  :should be 1)

(test-ac "updating works with condition"
  :valueof (ret counter 0
             (let a 0
               (updating a :with 1 :unless >=
                  ++.counter)
               (updating a :with 1 :unless >=
                  ++.counter)
               (updating a :with 3 :unless >=
                  ++.counter)
               (updating a :with 3 :unless >=
                  ++.counter)))
    :should be 2)

(test-ac "updating doesn't evaluate unnecessarily"
  :valueof (ret counter 0
             (with (getindex (fn() ++.counter 0)
                    l   '(4))
               (updating (l (getindex)) 1)))
    :should be 1)

(test-ac "new case semantics"
  :valueof (with (x 1
                  y 2
                  z 3
                  w 2)
             (case w
               x 34
               y 35
               z 36))
  :should be 35)

(test-ac "case uses testify"
  :valueof (let x 3
             (case x
               odd 34
               even 36))
  :should be 34)

(test-ac "rand-choice should run without errors"
  :valueof (with (a 3
                  b 4)
             (rand-choice
               (+ a b)
               (* a b)))
  :should satisfy idfn)

(test-ac "updating works"
  :valueof (ret counter 0
             (ret max 0
               (each elem '(1 3 2 6 5)
                 (updating max :with elem :unless >=
                   (++ counter max)))))
  :should be 10)

(test-ac "if works with no then"
  :valueof (if 34)
  :should be 34)

(test-ac "if recognizes keyword else"
  :valueof (if nil 35 :else 36)
  :should be 36)

(test-ac "iflet works"
  :valueof (iflet a 34 a 253)
  :should be 34)

(test-ac "iflet works - 2"
  :valueof (iflet a nil a 253)
  :should be 253)

(test-ac "iflet works with no then"
  :valueof (iflet a 34)
  :should be 34)

(test-ac "iflet works with nil then"
  :valueof (iflet a nil nil 34 35)
  :should be 35)

(test-ac "iflet binds multiple conditions"
  :valueof (iflet a nil 34
                    35  (+ 1 a))
  :should be 36)

(test-ac "iflet destructures"
  :valueof (iflet (a b)
              '(3 4)  42)
  :should be 42)

(test-ac "iflet destructures - 2"
  :valueof (iflet (a b)
              nil   34
              '(4 5) (cons a b))
  :should be '(4 . 5))

(test-ac "aif works with no then"
  :valueof (aif 34)
  :should be 34)

(test-ac "aif works with nil then"
  :valueof (aif nil nil 34 35)
  :should be 35)

(test-ac "aif works with nil then - 2"
  :valueof (aif t nil)
  :should be nil)

(test-ac "markdown passes text through"
  :valueof (markdown "abc def") :should be "abc def")

(test-ac "markdown linkifies urls"
  :valueof (markdown "http://foo.com")
  :should be "<a href=\"http://foo.com\" rel=\"nofollow\">http://foo.com</a>")

(test-ac "markdown linkifies SSL urls"
  :valueof (markdown "https://foo.com")
  :should be "<a href=\"https://foo.com\" rel=\"nofollow\">https://foo.com</a>")

(test-ac "markdown can suppress linkification"
  :valueof (markdown "http://foo.com" nil t)
  :should be "http://foo.com")

(test-ac "markdown can trim long urls for display"
  :valueof (markdown "http://foo.com" 4)
  :should be "<a href=\"http://foo.com\" rel=\"nofollow\">http...</a>")

(test-ac "markdown handles urls with punctuation"
  :valueof (markdown "https://en.wikipedia.org/wiki/Sherlock_Holmes_(1984_TV_series)")
  :should contain "https://en.wikipedia.org/wiki/Sherlock_Holmes_(1984_TV_series)")

(test-ac "markdown segments paragraphs"
  :valueof (markdown "abc\n\ndef")
  :should be "abc<p>def")

(test-ac "markdown formats indented regions as code"
  :valueof (markdown "  abc")
  :should be "<p><pre><code>  abc</code></pre>")

(test-ac "markdown formats asterisks in italics"
  :valueof (markdown "*abc*")
  :should be "<i>abc</i>")

(test-ac "static-filetype filters files without extensions"
  :valueof (static-filetype "/abc")
  :should be nil)

(test-ac "static-filetype recognizes jpegs"
  :valueof (static-filetype "/abc.jpg")
  :should be 'jpg)

(test-ac "static-filetype handles paths containing slashes"
  :valueof (static-filetype "/abc/def.jpg")
  :should be 'jpg)

(arc-eval '(deftem foo field1 'default))
(arc-eval '(= f inst!foo))

(test-ac "templates pick up defaults"
  :valueof f!field1
  :should be 'default)

(test-ac "reading empty templates works"
  :valueof inst!foo2!field1
  :should be nil)

(arc-eval '(= f!field1 34))
(test-ac "reading and assigning templates works"
  :valueof f!field1
  :should be 34)

(arc-eval '(= foo2-var 0))
(arc-eval '(deftem foo2 s (seconds)))
(arc-eval '(= f2 inst!foo2))
(test-ac "default vars are persisted"
  :valueof (iso f2!s (do sleep.1 f2!s))
  :should be t)

(arc-eval '(= f!field1 nil))
(test-ac "assigning templates to nil works"
  :valueof f!field1
  :should be nil)

(test-ac "temlist works"
  :valueof (temlist 'foo (inst 'foo 'field1 34))
  :should be '((field1 34)))

(test-ac "temlist includes default fields"
  :valueof (normalize:temlist 'foo (inst 'foo 'new-field 3))
  :should be '((field1 default) (new-field 3)))

(test-ac "temlist keeps nil non-default fields"
  :valueof (temlist 'foo (inst 'foo 'field1 nil))
  :should be '((field1 ())))

(test-ac "temlist includes explicitly set default fields"
  :valueof (temlist 'foo (inst 'foo 'field1 'default))
  :should be '((field1 default)))

(test-ac "temlist includes unknown nil fields"
  :valueof (normalize:temlist 'foo (inst 'foo 'new-field1 nil 'new-field2 3))
  :should be '((field1 default) (new-field1 ()) (new-field2 3)))

(test-ac "listtem DOES NOT ignore unknown fields"
  :valueof (listtem 'foo '((new-field 34)))
  :should be (inst 'foo 'new-field 34))

(test-ac "listtem handles nil"
  :valueof (listtem 'foo nil)
  :should be (inst 'foo))

(test-ac "temlist and listtem are converses"
  :valueof (listtem 'foo (temlist 'foo (inst 'foo 'field1 34)))
  :should be (inst 'foo 'field1 34))

(test-ac "temread and temwrite are converses"
  :valueof (w/instring i (w/outstring o
                           (temwrite 'foo (inst 'foo 'field1 34) o)
                           (inside o))
             (temread 'foo i))
  :should be (inst 'foo 'field1 34))

(test-ac "temread and temwrite are converses - 2"
  :valueof (w/instring i (w/outstring o
                           (temwrite 'foo (inst 'foo 'field1 nil) o)
                           (inside o))
             (temread 'foo i))
  :should be (inst 'foo 'field1 nil))

(test-ac "nil in file overwrites default"
  :valueof (w/instring i (w/outstring o
                           (temwrite 'foo (inst 'foo 'field1 nil) o)
                           (inside o))
             ((temread 'foo i) 'field1))
  :should be nil)

(test-ac "templates can distinguish explicit defaults from implicit defaults"
  :valueof (let tem (inst 'foo)
             (= tem!field1 34)
             (= tem!field1 'default) ; explicit set
             rep.tem.1)
  :should be (obj field1 'default))

(test-ac "unserialize handles improper lists"
  :valueof (unserialize '(a . b))
  :should be '(a . b))

(test-ac "spliceable-list initializes without a list"
  :valueof (rep:spliceable-list 2)
  :should be (obj contents nil last nil suffix-len 3 suffix nil))

(test-ac "spliceable-list initializes with a list"
  :valueof (rep:spliceable-list 2 '(1))
  :should be (obj contents list.1 last list.1 suffix-len 3 suffix nil))

(arc-eval '(= l (spliceable-list 2 '(1))))
(test-ac "suffix returns nothing if list is too short"
  :valueof suffix.l
  :should be nil)

(arc-eval '(append list.2 l))
(test-ac "suffix returns list if just long enough"
  :valueof suffix.l
  :should be '(1 2))
(test-ac "appending to spliceable-list works"
  :valueof rep.l
  :should be (obj contents '(1 2)  last list.2   suffix nil  suffix-len 3))

(arc-eval '(append list.3 l))
(test-ac "suffix 3" :valueof suffix.l :should be '(2 3))
(test-ac "splicing a list without a suffix works"
  :valueof splice.l
  :should be '(1))

(arc-eval '(= l (spliceable-list 2 '(1 2 3))))
(arc-eval '(append list.4 l))
(test-ac "suffix 4" :valueof suffix.l :should be '(3 4))
(test-ac "appending to spliceable-list updates suffix"
  :valueof rep.l
  :should be (obj contents '(1 2 3 4)  last list.4   suffix '(2 3 4)   suffix-len 3))

(test-ac "splicing a list with suffix works"
  :valueof splice.l
  :should be '(1 2))

;; -----------------------------57651155441074198547161975
;; Content-Disposition: form-data; name="fnid"
;;
;; 2iJaTziJtr
;; -----------------------------57651155441074198547161975
;; Content-Disposition: form-data; name="someField"
;;
;; 33
;; -----------------------------57651155441074198547161975--
(test-ac "parse-multipart-args works"
  :valueof (parse-multipart-args "--abc" (instring "\r\n--abc\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n34\r\n--abc\r\nContent-Disposition: form-data; name=\"b\"\r\n\r\n209\r\n--abc--\r\n"))
  :should be `(("a" ,(obj "contents" "34")) ("b" ,(obj "contents" "209"))))

; currently fails; how to include binary data in string literals?
(test-ac "currently failing: parse-multipart-args returns lists of ints for non-ascii data"
  :valueof (parse-multipart-args "--abc" (instring "\r\n--abc\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n34\r\n--abc\r\nContent-Disposition: form-data; name=\"b\"\r\n\r\n\x80\r\n--abc--\r\n"))
  :should be `(("a" ,(obj "contents" "34")) ("b" ,(obj "contents" list.128)))) ; \x80 in decimal
