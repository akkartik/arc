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
       (if ans
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

(define-syntax pending
  (syntax-rules(:valueof :should)
    ((test msg :valueof expr :should predicate args ...)
     (begin
       (display "X ")
       (display msg)
       (newline)))))

(define (satisfy x f)
  (f x))
(xdef satisfy satisfy)

(define be equal?)
(xdef be be)

(define (idfn x)
  x)



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
  :valueof (keyword-args '(1 2 :a 3) '())
  :should be '((a . 3)))

(test-scm "keyword-args works with rest args"
  :valueof (keyword-args '(1 2 :a 3) 'a)
  :should be '((a 3)))

(test-scm "strip-keyword-args works"
  :valueof (strip-keyword-args '(1 2 :a 3 4) '())
  :should be '(1 2 4))

(test-scm "strip-keyword-args works on rest args"
  :valueof (strip-keyword-args '(1 2 :a 3 4) 'a)
  :should be '(1 2))

(test-scm "optional-params works - 1"
  :valueof (optional-params 'a)
  :should be '())
(test-scm "optional-params 1"
  :valueof (optional-params '(a b c))
  :should be '())
(test-scm "optional-params 2"
  :valueof (optional-params '(a b c ? d e))
  :should be '((d . e)))
(test-scm "optional-params 3"
  :valueof (optional-params '(a b c ? d e f))
  :should be '((d . e) (f)))
(test-scm "optional-params 4"
  :valueof (optional-params '(a b c ? d e f nil))
  :should be '((d . e) (f . nil)))
(test-scm "optional-params 5"
  :valueof (optional-params '(a b c ? d e f nil . g))
  :should be '((d . e) (f . nil)))

(test-scm "get-arg works for missing arg"
  :valueof (get-arg 'c 'a 1 ())
  :should be #f)

(test-scm "get-arg works for required args"
  :valueof (get-arg 'c '(a b c) '(1 2 3) ())
  :should be 3)

(test-scm "get-arg works for varargs"
  :valueof (get-arg 'a 'a '(1 2 3) ())
  :should be '(1 2 3))

(test-scm "get-arg works for rest args"
  :valueof (get-arg 'd '(a (b c) . d) '(1 (2 3) 4 5 6) ())
  :should be '(4 5 6))

(test-scm "get-arg works for destructured args"
  :valueof (get-arg 'b '(a (b c))
                       '(1 (2 3)) ())
  :should be 2)

(test-scm "get-arg works for destructured args - 2"
  :valueof (get-arg 'f '(a (b c (d e f)))
                       '(1 (2 3 (4 5 6))) ())
  :should be 6)

(test-scm "get-arg works for missing destructured args"
  :valueof (get-arg 'x '(a (b c (d e f)))
                       '(1 (2 3 (4 5 6))) ())
  :should be #f)

(test-scm "get-arg works for destructuring and rest args"
  :valueof (get-arg 'g '(a (b c (d e f) . g))
                       '(1 (2 3 (4 5 6) 7 8)) ())
  :should be '(7 8))

(test-scm "get-arg works for destructuring and rest args - 2"
  :valueof (get-arg 'f '(a (b c (d e f) . g))
                       '(1 (2 3 (4 5 6) 7 8)) ())
  :should be 6)

(test-scm "get-arg works for destructuring and rest args - 3"
  :valueof (get-arg 'x '(a (b c (d e f) . g))
                       '(1 (2 3 (4 5 6) 7 8)) ())
  :should be #f)

(test-scm "get-arg works for destructuring and rest args - 4"
  :valueof (get-arg 'f '(a (b c (d e . f) . g))
                       '(1 (2 3 (4 5 6 7) 7 8)) ())
  :should be '(6 7))

(test-scm "get-arg works for destructuring and rest args - 5"
  :valueof (get-arg 'f '(a (b c (d e . f) . g))
                       '(1 (2 3 (4 5) 7 8)) ())
  :should be '())

(test-scm "get-arg works for keyword args"
  :valueof (get-arg 'a '(a (b c (d e f) . g))
                       '((2 3 (4 5 6) 7 8)) '((a . 1)))
  :should be 1)

(test-scm "get-arg works on args with keywords"
  :valueof (get-arg 'd '(a (b c (d e f) . g))
                       '((2 3 (4 5 6) 7 8)) '((a . 1)))
  :should be 4)



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

(test-scm "get-arg for missing rest args"
  :valueof (get-arg 'rest 'rest () ())
  :should be ())

(test-scm "get-arg returns an illegal literal for missing, hopefully optional, params - 1"
  :valueof (get-arg 'a '(a) () ())
  :should be #f)

(test-scm "get-arg returns an illegal literal for missing, hopefully optional, params - 2"
  :valueof (get-arg 'e '(a b c e f . d) '(1 2 3) '())
  :should be #f)

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

(arc-eval '(assign foo (fn(a . b) b)))
(test-ac "rest args can be named"
  :valueof (foo 3 :b 4 5)
  :should be '(4 5))

(arc-eval '(assign foo (fn(a ? b 3 . c) (cons b c))))
(test-ac "optional + named rest args"
  :valueof (foo 2 :c 3)
  :should be '(3 3))

(test-ac "optional + named rest args - 2"
  :valueof (foo 2 4 :c 3)
  :should be '(4 3))

(test-ac "optional + named rest args - 3"
  :valueof (foo 2 :b 4 3)
  :should be '(4 3))

(arc-eval '(assign foo (fn(a ? b () c 3 . body) (cons b (cons c body)))))
(test-ac "call with some optional and rest args without naming"
  :valueof (foo 3 4 :body 4 5)
  :should be '(4 3 4 5))

(test-ac "call with some named optional and rest args"
  :valueof (foo 3 :c 4 :body 4 5)
  :should be '(() 4 4 5))

(arc-eval '(assign foo (fn(a ? b 2 c (fn() (+ 1 b)) . body) (c))))
(test-ac "defaults compile properly"
  :valueof (foo 3 :b 4)
  :should be 5)



(require "brackets.scm")
(use-bracket-readtable)
(aload "arc.arc")
(aload "libs.arc")

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
  :valueof (some (testify #\@) "a@b")
  :should be t)

(test-ac "some 2"
  :valueof (some (testify #\@) "abc")
  :should be ())

(test-ac "some 3 - list"
  :valueof (some testify.1 '(1 2 3))
  :should be t)

(test-ac "some 4"
  :valueof (some testify.4 '(1 2 3))
  :should be ())

(test-ac "find 1 - string"
  :valueof (find #\@ "a@b")
  :should be #\@)

(test-ac "find 2"
  :valueof (find #\@ "abc")
  :should be ())

(test-ac "find 3 - list"
  :valueof (find #\@ '(#\@))
  :should be #\@)

(test-ac "find 4"
  :valueof (find #\@ '(1 2))
  :should be ())

(test-ac "find 5"
  :valueof (find odd '(1 2))
  :should be 1)

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
  :valueof (rem #\@ "a@b")
  :should be "ab")

(test-ac "keep 1 - string"
  :valueof (keep (testify #\@) "a@b")
  :should be "@")

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
  :should be '(table ((3 4) (1 2))))

(test-ac "unserialize complements serialize for nil"
  :valueof (unserialize:serialize ())
  :should be ())

(test-ac "unserialize complements serialize for lists"
  :valueof (unserialize:serialize '(1 2 3))
  :should be '(1 2 3))

(test-ac "unserialize complements serialize for strings"
  :valueof (unserialize:serialize "abc")
  :should be "abc")

(test-ac "unserialize complements serialize for tables"
  :valueof (unserialize:serialize (obj 1 2 3 4))
  :should be (obj 1 2 3 4))

(test-ac "serialize operates on tables inside lists"
  :valueof (serialize `(1 ,(table) 2 3))
  :should be '(1 (table ()) 2 3))

(test-ac "unserialize complements serialize for tables inside lists"
  :valueof (unserialize:serialize `(1 ,(table) 2 3))
  :should be `(1 ,(table) 2 3))

(test-ac "serialize operates on nested tables"
  :valueof (serialize (obj 1 (table) 2 3))
  :should be '(table ((2 3) (1 (table ())))))

(test-ac "unserialize complements serialize for nested tables"
  :valueof (unserialize:serialize (obj 1 (table) 2 3))
  :should be (obj 1 (table) 2 3))

(test-ac "freq works"
  :valueof (freq '(1 2 3 2 1))
  :should be (obj 1 2 2 2 3 1))

(test-ac "freq works on strings"
  :valueof (freq "abracadabra")
  :should be (obj #\a 5 #\b 2 #\c 1 #\d 1))

(test-ac "count-up works"
  :valueof (count-up '(1 2 3 3 2))
  :should be '(2 2 3 2 1 1))

(test-ac "map works"
  :valueof (map [+ 1 _] '(1 2 3))
  :should be '(2 3 4))

(test-ac "map works on multiple lists"
  :valueof (map * '(1 2 3) '(1 2 3 4))
  :should be '(1 4 9))
