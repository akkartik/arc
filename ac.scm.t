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

(test-scm "3"
  :valueof (optional-params 'a)
  :should be '())
(test-scm "4"
  :valueof (optional-params '(a b c))
  :should be '())
(test-scm "5"
  :valueof (optional-params '(a b c ? d e))
  :should be '((d . e)))
(test-scm "6"
  :valueof (optional-params '(a b c ? d e f))
  :should be '((d . e) (f)))
(test-scm "7"
  :valueof (optional-params '(a b c ? d e f nil))
  :should be '((d . e) (f . nil)))
(test-scm "8"
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



(require "brackets.scm")
(use-bracket-readtable)
(aload "arc.arc")

(test-ac "iso works on nil"
  :valueof (iso () ())
  :should be 't)

(test-ac "iso works on nil - 2"
  :valueof (iso () t)
  :should be ())

(test-ac "iso works on nil - 3"
  :valueof (iso () 3)
  :should be ())

(test-ac "iso works on tables"
  :valueof (iso (obj 1 2 3 4) (obj 3 4 1 2))
  :should be 't)

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
  :should be 't)

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
  :should be 't)

(test-ac "some 2"
  :valueof (some (testify #\@) "abc")
  :should be ())

(test-ac "some 3 - list"
  :valueof (some testify.1 '(1 2 3))
  :should be 't)

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
  :should be 't)

(test-ac "all 2"
  :valueof (all odd '(1 2 3))
  :should be ())

(test-ac "all 3 - testify works"
  :valueof (all 1 '(1 1 1))
  :should be 't)

(test-ac "all 4 - empty list"
  :valueof (all 1 '())
  :should be 't)

(test-ac "all 5 - empty string"
  :valueof (all 1 "")
  :should be 't)

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
