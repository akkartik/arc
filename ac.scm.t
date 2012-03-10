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
       (if (ac-false? ans)
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

(define (ac-false? x)
  (or (not x)
      (not (eq? x ()))))



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
               (updating a 0
                  :body
                    ++.counter)))
  :should be 0)

(test-ac "updating updates when necessary"
  :valueof (ret counter 0
             (let a 0
               (updating a 1
                  :body
                    ++.counter)))
  :should be 1)

(test-ac "updating works with condition"
  :valueof (ret counter 0
             (let a 0
               (updating a :unless >= 1
                  :body
                    ++.counter)
               (updating a :unless >= 1
                  :body
                    ++.counter)
               (updating a :unless >= 3
                  :body
                    ++.counter)
               (updating a :unless >= 2
                  :body
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

(arc-eval '(= f!field1 34))
(test-ac "reading and assigning templates works"
  :valueof f!field1
  :should be 34)

(arc-eval '(= f!field1 nil))
(test-ac "assigning templates to nil works"
  :valueof f!field1
  :should be nil)

(test-ac "temlist works"
  :valueof (temlist 'foo (inst 'foo 'field1 34))
  :should be '((field1 34)))

(test-ac "temlist skips default fields"
  :valueof (temlist 'foo (inst 'foo 'new-field 3))
  :should be '((new-field 3)))

(test-ac "temlist keeps nil non-default fields"
  :valueof (temlist 'foo (inst 'foo 'field1 nil))
  :should be '((field1 ())))

(test-ac "temlist includes explicitly set default fields"
  :valueof (temlist 'foo (inst 'foo 'field1 'default))
  :should be '((field1 default)))

(test-ac "temlist skips unknown nil fields"
  :valueof (temlist 'foo (inst 'foo 'new-field1 nil 'new-field2 3))
  :should be '((new-field2 3)))

(test-ac "listtem DOES NOT ignore unknown fields"
  :valueof (listtem 'foo '((new-field 34)))
  :should be (inst 'foo 'new-field 34))

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
