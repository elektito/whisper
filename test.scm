;; let's start by testing some comments

#;100
#;#f
#;(a
   b ;; foo
   c)

#|
testing nested comments
#|
still a comment #||#
|#
and still a comment
|#; a comment right after another

;; and now to more important things! |# <---make emacs scheme mode happy

(import (whisper))

;; this is needed separately because we use some primcalls not exported
;; in (whisper), like environment-bind!
(import (whisper core))

;; needed for the environment/eval tests below
(import (scheme eval))

;; defines

(eq? 1 1) ; also test datum comment at the end of a list
(eq? 'foo 'foo)

(eq? #f '#f)
(eq? #t '#t)
(eq? 10 '10)
(eq? #\space #\space)
(eq? '() '())
(not (eq? '() 'nil))
(equal? "foo" '"foo")

(equal? '(a b) '(a b))

(eqv? #t #t)
(eqv? #f #f)
(eqv? 1 1)
(eqv? 'foo 'foo)
(eqv? #\space #\x20)
(eqv? '() '())
(let ((x "foo"))
  (eqv? x x))
(let ((x (lambda (a) a)))
  (eqv? x x))

(symbol=? 'foo 'foo)
(not (symbol=? 'foo 'bar))

(boolean=? #t #t)
(not (boolean=? #t #f))

(string=? "foo" (symbol->string 'foo))
(eq? 'bar (string->symbol "bar"))

(not (eq? 'abc 'AbC))

(eq? (string->symbol "hello") (string->symbol "hello"))
(eq? 'hello (string->symbol "hello"))
(string? (symbol->string (gensym)))
(string? (symbol->string (gensym "prefix")))
(not (eq? (gensym "foo") (gensym "foo")))

(void? (void))

;;

(equal? "1750" (number->string 1000 8))
(equal? "1111101000" (number->string 1000 2))
(equal? "7d0" (number->string 2000 16))

(= 1000 (string->number "3e8" 16))
(= -1000 (string->number "-3e8" 16))
(= 1000 (string->number "+3e8" 16))
(= 1000 (string->number "#x3e8"))
(= -1000 (string->number "#x-3e8"))
(= 1000 (string->number "#x+3e8"))
(= 1000 (string->number "#X3e8"))
(= 1000 (string->number "#x3e8" 10))
(= 1000 (string->number "1000"))
(= -1000 (string->number "-1000"))
(= 1000 (string->number "+1000"))
(= 1000 (string->number "1000" 10))
(= 1000 (string->number "#d1000"))
(= 1000 (string->number "#D1000"))
(= 1000 (string->number "1750" 8))
(= 1000 (string->number "#o1750"))
(= 1000 (string->number "#O1750"))
(= 1000 (string->number "1111101000" 2))
(= 1000 (string->number "#b1111101000"))
(= 1000 (string->number "#B1111101000"))
(guard (e (else #t))
       (string->number "1000" 1))
(not (string->number "foo"))

(= 1 (max 1))
(= 2 (max 1 2))
(= 8 (max 4 1 8 7 -9))

(= 1 (min 1))
(= 1 (min 1 2))
(= -9 (min 4 1 8 7 -9))

(= 9 (square 3))
(= 9 (square -3))

;;

(eq? 100 (if #t 100 200))
(eq? 200 (if #f 100 200))
(eq? 100 (if 0 100 200))
(eq? 100 (if 1 100 200))
(eq? 100 (if "foo" 100 200))
(eq? 100 (if (lambda (x) x) 100 200))
(eq? 100 (if #t 100))
(eq? (void) (if #f 100))

(cond (else #t))
(void? (let ((else #f))
         (cond (else 10))))
(= 200 (cond ((= 10 20) 50 100)
             ((= 5 5) 80 200)
             ((= 40 40) 300)))
(= 300 (cond (#f 100)
             (#f 200)
             (300)
             (else 400)))

(eq? 'composite
     (case (* 2 3)
       ((2 3 5 7) 'prime)
       ((1 4 6 8 9) 'composite)))

(eq? 0 (+))
(eq? 2 (+ 2))
(eq? 100 (+ 30 70))
(eq? 100 (+ 130 -30))
(eq? 25 (+ 2 5 7 11))

(eq? -2 (- 2))
(eq? -40 (- 30 70))
(eq? 160 (- 130 -30))
(eq? -21 (- 2 5 7 11))

(eq? 1 (*))
(eq? 10 (* 10))
(eq? 50 (* 10 5))
(eq? 300 (* 10 5 6))

(eq? 1 (/ 1))
(eq? 2 (/ 10 5))
(eq? 20 (/ 3000 10 5 3))

(eq? #f (not #t))
(eq? #t (not #f))
(eq? #f (not '()))
(eq? #f (not 'nil))
(eq? #f (not 1))
(eq? #f (not 0))


(< 10 20)
(not (< 20 10))
(not (< 10 10))
(< -10 1)
(< -20 0)

(<= 10 20)
(not (<= 20 10))
(<= 10 10)
(<= -10 1)
(<= -20 0)

(> 81 5)
(not (> 5 81))
(not (> 5 5))
(> 1 -50)
(> 0 -10)

(>= 81 5)
(not (>= 5 81))
(>= 10 10)
(>= 1 -10)
(>= 0 -20)

(zero? 0)
(not (zero? 1))
(not (zero? -1))

(negative? -1)
(not (negative? 0))
(not (negative? 1))

(positive? 1)
(not (positive? 0))
(not (positive? -1))

(eq? 10 ((lambda (x y) x) 10 20))
(eq? 20 ((lambda (x y) y) 10 20))

(eq? 3 (and 1 2 3))
(eq? #f (and 1 2 #f 3))
(eq? 1 (or 1 2 3))
(eq? 1 (or #f #f 1 2 3))

(eq? 100 (car (cons 100 '())))
(eq? 'bar (cadr '(foo bar spam eggs)))

;; type predicates

(null? '())
(not (null? '(1)))
(not (null? '(1 2)))
(not (null? '(1 . 2)))
(not (null? 1))
(not (null? "foo"))
(not (null? 'foo))
(not (null? (lambda (x) x)))

(pair? '(1))
(pair? '(1 2))
(pair? '(1 2 3))
(pair? '(1 . 2))
(not (pair? '()))
(not (pair? 'foo))
(not (pair? "foo"))
(not (pair? 1))
(not (pair? (lambda (x) x)))

(list? '())
(list? '(1))
(list? '(1 2))
(not (list? '(1 . 2)))
(not (list? '(1 2 . 3)))
(not (list? (lambda (x) x)))

(symbol? 'foo)
(not (symbol? '()))
(not (symbol? 1))
(not (symbol? "foo"))
(not (symbol? '(1 . 2)))
(not (symbol? '(1 2)))
(not (symbol? (lambda (x) x)))

(boolean? #f)
(boolean? #t)
(not (boolean? -1))
(not (boolean? 0))
(not (boolean? 1))
(not (boolean? 'foo))
(not (boolean? '()))
(not (boolean? '(1 2)))
(not (boolean? (lambda (x) x)))


(string? "")
(string? "foo")
(not (string? 'foo))
(not (string? 1))
(not (string? '()))
(not (string? '(1)))
(not (string? '(1 2)))
(not (string? '(1 . 2)))
(not (string? (lambda (x) x)))

(procedure? (lambda (x) x))
(procedure? (lambda () 10))
(not (procedure? 'foo))
(not (procedure? 1))
(not (procedure? "foo"))
(not (procedure? '()))
(not (procedure? '(1)))
(not (procedure? '(1 2)))
(not (procedure? '(1 . 2)))

(char? #\a)
(char? #\tab)
(char? #\x09)
(not (char? '()))
(not (char? '(1)))
(not (char? '(1 . 2)))
(not (char? "f"))
(not (char? 'f))
(not (char? (lambda (x) x)))

;; list

(equal? 1 (caar '((1) 2)))
(equal? 2 (cadr '(1 2)))
(equal? '(2) (cdar '((1 2) 3)))
(equal? '(3 4) (cddr '(1 2 3 4)))

(equal? 1 (caaar '(((1 2) 8 9) 20 30)))
(equal? 5 (caadr '(1 (5 6) 7 100)))
(equal? '(2 3) (cadar '((1 (2 3)) 100)))
(equal? 3 (caddr '(1 2 3 4)))
(equal? '(2 3) (cdaar '(((1 2 3) 50) 100)))
(equal? '(3) (cdadr '(1 (2 3) 4 5)))
(equal? '(3) (cddar '((1 2 3) 4)))
(equal? '(4 5) (cdddr '(1 2 3 4 5)))

(equal? 1 (caaaar '((((1 2) 80) 90) 100)))
(equal? 2 (caaadr '(1 ((2)) 3 4)))
(equal? '(2 3) (caadar '((1 ((2 3) 4) 5 6 7) 100)))
(equal? 3 (caaddr '(1 2 (3 10) 4 5)))
(equal? 2 (cadaar '(((1 2 3 4) 90) 100)))
(equal? 10 (cadadr '(1 (2 10 20 30) 3 4 5)))
(equal? 3 (caddar '((1 2 3 4 5) 100)))
(equal? 4 (cadddr '(1 2 3 4 5)))
(equal? '(2 3) (cdaaar '((((1 2 3) 80) 90) 100)))
(equal? '(20) (cdaadr '(1 ((10 20) 2) 3 4)))
(equal? '(3) (cdadar '((1 (2 3) 4 5) 6 7)))
(equal? '(10 20) (cdaddr '(1 2 (3 10 20) 4 5)))
(equal? '(3 4) (cddaar '(((1 2 3 4) 90) 100)))
(equal? '(20) (cddadr '(1 (2 10 20) 3)))
(equal? '(4) (cdddar '((1 2 3 4) 100)))
(equal? '(5 6) (cddddr '(1 2 3 4 5 6)))

(eq? 0 (length '()))
(eq? 1 (length '(1)))
(eq? 2 (length '(1 2)))
(eq? 3 (length '(1 2 3)))

(equal? '() (reverse '()))
(equal? '(1) (reverse '(1)))
(equal? '(1 2 3) (reverse '(3 2 1)))
(equal? '(1 (2 3) 4 5) (reverse '(5 4 (2 3) 1)))

(equal? '(c d e f) (list-tail '(a b c d e f) 2))
(equal? '(a b c d e f) (list-tail '(a b c d e f) 0))

(equal? 'c (list-ref '(a b c d e f) 2))
(equal? 'a (list-ref '(a b c d e f) 0))

(let ((x '(1 2 3 4 5)))
  (list-set! x 2 'abc)
  (equal? '(1 2 abc 4 5) x))

(let ((x '(1 2 3)))
  (set-car! x 10)
  (equal? x '(10 2 3)))

(let ((x '(1 2 3)))
  (set-cdr! x 10)
  (equal? x '(1 . 10)))

(null? (list-copy '()))
(equal? '(1 2 3) (list-copy '(1 2 3)))
(equal? '(1 2 3 . 4) (list-copy '(1 2 3 . 4)))
(let ((x #(1 2 3)))
  ;; non-list objects should be returned unchanged
  (eq? x (list-copy x)))

;; letrec

(letrec ((is-even? (lambda (n)
                     (or (= n 0)
                         (is-odd? (- n 1)))))
         (is-odd? (lambda (n)
                    (and (not (= n 0))
                         (is-even? (- n 1))))))
  (is-odd? 11))

(letrec ()
  #t)

(letrec ((fib (lambda (n)
                (if (< n 2)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))))))
  (= 55 (fib 10)))

;; letrec*

(letrec* ((is-even? (lambda (n)
                      (or (= n 0)
                          (is-odd? (- n 1)))))
          (is-odd? (lambda (n)
                     (and (not (= n 0))
                          (is-even? (- n 1))))))
  (is-odd? 11))

(letrec* ()
  #t)

(letrec* ((fib (lambda (n)
                 (if (< n 2)
                     n
                     (+ (fib (- n 1)) (fib (- n 2)))))))
  (= 55 (fib 10)))

(letrec* ((x 10)
          (y x))
  (= y 10))

;; map

(equal? '() (map (lambda (x) (* x x)) '()))
(equal? '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3)))
(equal? '(11 22 33) (map + '(1 2 3) '(10 20 30)))
(equal? '(15 27) (map + '(1 2) '(4 5 6) '(10 20 30)))

;; for-each

(let ((result '()))
  (for-each (lambda (x) (set! result (cons x result))) '(1 2 3))
  (equal? '(3 2 1) result))

(let ((result '()))
  (for-each (lambda (x y) (set! result (cons (+ x y) result))) '(1 2 3) '(10 20 30))
  (equal? '(33 22 11) result))

(let ((count 0))
  (for-each (lambda (x) (set! count (+ count 1))) '())
  (= 0 count))

(let ((result '()))
  (for-each (lambda (x y) (set! result (cons (list x y) result))) '(1 2 3) '(a b))
  (equal? '((2 b) (1 a)) result))

(eq? (void) (for-each (lambda (x) x) '(1 2 3)))

(let ((result '()))
  (for-each (lambda args (set! result (cons args result))) '(1 2 3 4 5) '(10 20 30 40))
  (equal? '((4 40) (3 30) (2 20) (1 10)) result))

;; equality

(not (eq? (gensym) (gensym)))
(let ((gs (gensym)))
  (eq? gs gs))

(equal? "foo" "foo")
(equal? #\A #\A)
(equal? 'foo 'foo)
(equal? 1 1)
(not (equal? 1 2))
(equal? '(1 foo 2) '(1 foo 2))
(not (equal? '(1 foo 2) '(1 foo 2 3)))
(equal? '(1 (foo) 2) '(1 (foo) 2))
(equal? '(1 (foo "bar") 2) '(1 (foo "bar") 2))
(not (equal? '(1 (foo "bar") 2) '(1 (foo "bar" 10) 2)))

(eq? 6 (apply + '(1 2 3)))
(eq? 10 (apply + '(1 2 3 4)))
(eq? 0 (apply + '()))
(equal? '(1 2 3 foo bar)
        (apply list 1 2 3 '(foo bar)))

(= 1)
(= 1 1)
(= 1 1 1)
(not (= 1 1 2 1))

;; characters
(eq? #\space #\ )
(eq? #\space #\x20)
(eq? #\x #\x78)
(eq? #\( #\x28)
(eq? #\) #\x29)
(eq? #\alarm #\x7)
(eq? #\backspace #\x8)
(eq? #\delete #\x7f)
(eq? #\newline #\xA)
(eq? #\null #\x0)
(eq? #\return #\x0d)
(eq? #\tab #\x09)

(char? #\space)
(char? #\A)
(char? #\x40)

(char=? #\space)
(char=? #\space #\space)
(char=? #\space #\  #\space)
(not (char=? #\A #\B))
(not (char=? #\A #\B #\space))

(char<? #\a)
(char<? #\a #\b)
(char<? #\a #\b #\c)
(char<? #\A #\a)
(not (char<? #\b #\a))
(not (char<? #\a #\a #\b #\c))

(char<=? #\a)
(char<=? #\a #\a)
(char<=? #\a #\b)
(char<=? #\a #\b #\c)
(char<=? #\A #\a)
(not (char<=? #\b #\a))
(char<=? #\a #\a #\b #\c)
(char<=? #\a #\b #\b #\c)

(char>? #\a)
(char>? #\b #\a)
(char>? #\c #\b #\a)
(char>? #\a #\A)
(not (char>? #\a #\b))
(not (char>? #\c #\b #\a #\a))

(char>=? #\a)
(char>=? #\a #\a)
(char>=? #\b #\a)
(char>=? #\c #\b #\a)
(char>=? #\a #\A)
(not (char>=? #\a #\b))
(char>=? #\c #\b #\a #\a)
(char>=? #\c #\b #\b #\a)

(char-ci=? #\A #\a)
(char-ci=? #\A #\A)
(char-ci=? #\a #\a)
(char-ci=? #\1 #\1)
(char-ci=? #\A #\a #\a)
(not (char-ci=? #\A #\b #\A))
(char-ci=? #\space)
(char-ci=? #\space #\space)
(char-ci=? #\space #\  #\space)
(not (char-ci=? #\A #\B))
(not (char-ci=? #\A #\B #\space))

(char-ci<? #\a)
(char-ci<? #\a #\b)
(char-ci<? #\a #\b #\c)
(not (char-ci<? #\A #\a))
(not (char-ci<? #\b #\a))
(not (char-ci<? #\a #\a #\b #\c))
(char-ci<? #\a #\B)

(char-ci<=? #\a)
(char-ci<=? #\a #\a)
(char-ci<=? #\A #\a)
(char-ci<=? #\a #\A)
(char-ci<=? #\a #\b)
(char-ci<=? #\a #\B)
(char-ci<=? #\a #\b #\c)
(char-ci<=? #\a #\b #\C)
(not (char-ci<=? #\b #\a))
(not (char-ci<=? #\B #\a))
(char-ci<=? #\a #\a #\b #\c)
(char-ci<=? #\a #\b #\b #\c)
(char-ci<=? #\a #\A #\b #\c)
(char-ci<=? #\a #\b #\B #\c)

(char-ci>? #\a)
(char-ci>? #\b #\a)
(char-ci>? #\B #\a)
(char-ci>? #\b #\A)
(char-ci>? #\c #\b #\a)
(char-ci>? #\c #\B #\a)
(not (char-ci>? #\a #\A))
(not (char-ci>? #\a #\b))
(not (char-ci>? #\A #\b))
(not (char-ci>? #\c #\b #\a #\a))
(not (char-ci>? #\c #\b #\A #\a))
(not (char-ci>? #\c #\B #\a #\a))

(char-ci>=? #\a)
(char-ci>=? #\a #\a)
(char-ci>=? #\a #\A)
(char-ci>=? #\A #\a)
(char-ci>=? #\b #\a)
(char-ci>=? #\B #\a)
(char-ci>=? #\b #\A)
(char-ci>=? #\c #\b #\a)
(char-ci>=? #\c #\B #\a)
(char-ci>=? #\C #\b #\a)
(not (char-ci>=? #\a #\b))
(not (char-ci>=? #\A #\b))
(not (char-ci>=? #\a #\B))
(char-ci>=? #\c #\b #\a #\a)
(char-ci>=? #\c #\b #\b #\a)
(char-ci>=? #\c #\B #\a #\a)
(char-ci>=? #\c #\b #\b #\A)

(char-alphabetic? #\A)
(char-alphabetic? #\a)
(not (char-alphabetic? #\1))

(char-upper-case? #\A)
(not (char-upper-case? #\a))

(char-lower-case? #\a)
(not (char-lower-case? #\A))

(char-whitespace? #\space)
(char-whitespace? #\tab)
(char-whitespace? #\newline)
(char-whitespace? #\return)
(not (char-whitespace? #\a))
(not (char-whitespace? #\1))

(char-numeric? #\0)
(char-numeric? #\5)
(not (char-numeric? #\A))
(not (char-numeric? #\space))
(not (char-numeric? #\tab))

(eq? (char->integer #\A) 65)
(eq? (integer->char 32) #\space)

;; string

;(string=? "Hello" "H\x65;llo")
;(string=? "foobar" "foo\
;                    bar")
(char=? #\tab (string-ref "\t" 0))
(char=? #\newline (string-ref "\n" 0))
(char=? #\return (string-ref "\r" 0))
(char=? #\alarm (string-ref "\a" 0))
(char=? #\backspace (string-ref "\b" 0))
(char=? #\" (string-ref "\"" 0))
;(char=? #\x7c (string-ref "\|" 0))
;(char=? #\null (string-ref "\x0;" 0))

(string=? (make-string 10 #\A) "AAAAAAAAAA")
(eq? 10 (string-length (make-string 10 #\A)))
(eq? 10 (string-length (make-string 10)))

(string=? "" (string))
(string=? "A" (string #\A))
(string=? "ABC" (string #\A #\B #\C))

(eq? #\A (string-ref "ABC" 0))
(eq? #\B (string-ref "ABC" 1))
(eq? #\C (string-ref "ABC" 2))

(let ((s (make-string 3 #\space)))
  (string-set! s 0 #\X)
  (string-set! s 2 #\Z)
  (string=? s "X Z"))

(equal? "cde" (substring "abcdefg" 2 5))
(equal? "" (substring "abcdefg" 4 4))

;; an empty substring at the very end of the string. r7rs does not say
;; whether start may equal the end, but chez, chibi, and chicken all
;; allow it, and string-split relies on it to produce the empty last
;; component of something like "a/b/".
(equal? "" (substring "abcdefg" 7 7))
(equal? "" (substring "" 0 0))
(equal? '("a" "b" "") (string-split "a/b/" #\/))
(equal? '("" "a") (string-split "/a" #\/))

(string<? "a" "b")
(string<? "a" "b" "c")
(string<? "A" "a")
(not (string<? "b" "a"))
(not (string<? "a" "a" "b" "c"))
(string<? "ab" "abc")
(not (string<? "abc" "ab"))
(string<? "" "a")
(string<? "abc" "abd")

(string<=? "a" "a")
(string<=? "a" "b")
(string<=? "a" "b" "c")
(string<=? "A" "a")
(not (string<=? "b" "a"))
(string<=? "a" "a" "b" "c")
(string<=? "a" "b" "b" "c")

(string>? "b" "a")
(string>? "c" "b" "a")
(string>? "a" "A")
(not (string>? "a" "b"))
(not (string>? "c" "b" "a" "a"))
(string>? "abc" "ab")

(string>=? "a" "a")
(string>=? "b" "a")
(string>=? "c" "b" "a")
(string>=? "a" "A")
(not (string>=? "a" "b"))
(string>=? "c" "b" "a" "a")
(string>=? "c" "b" "b" "a")

(string-ci=? "foo" "foo")
(string-ci=? "foo" "FOO")
(string-ci=? "Hello" "hELLO")
(not (string-ci=? "foo" "bar"))
(string-ci=? "a" "A" "a")
(not (string-ci=? "a" "A" "b"))
(string-ci=? "aa" "aa" (string #\a #\a))

(string-ci<? "bar" "Foo" "spaM")
(not (string-ci<? "BAR" "bar" "foo" "spam"))

(string-ci<=? "bar" "FOO" "Spam")
(string-ci<=? "BAR" "bar" "Foo" "Spam")

(string-ci>? "spam" "Foo" "bar")
(not (string-ci>? "spam" "Foo" "Bar" "bar"))

(string-ci>=? "spam" "foo" "bar")
(string-ci>=? "spam" "foo" "bar" "BAR")
(string-ci>=? "spam" "Foo" "Bar" "bar")

(equal? "cde" (string-copy "abcdefg" 2 5))
(equal? "" (string-copy "abcdefg" 4 4))
(equal? "cdefg" (string-copy "abcdefg" 2))
(let ((s "abcd"))
  (let ((r (string-copy s)))
    (and (equal? s r)
         (not (eq? s r)))))

(equal? "" (string-append))
(let ((s "12"))
  (let ((a (string-append s)))
    (and (not (eq? s a)) ;; the return value should be a newly allocated string
         (equal? "12" a))))
(equal? "123456" (string-append "12" "" "3456"))

(equal? (string->list "") '())
(equal? (string->list "ABC") '(#\A #\B #\C))
(equal? (string->list "ABCDEFG" 2 5) '(#\C #\D #\E))
(equal? (string->list "ABCDEFG" 2) '(#\C #\D #\E #\F #\G))
(equal? (list->string '(#\A #\B #\C)) "ABC")
(equal? (list->string '()) "")

(equal? "" (string-map char-upcase ""))
(equal? "foobar1" (string-map char-downcase "FoOBaR1"))

(let ((result '()))
  (string-for-each (lambda (c)
                     (set! result (cons c result)))
                   "foo")
  (equal? '(#\o #\o #\f) result))

;; make sure primitive functions are available as normal functions

(procedure? cons)
(equal? '(1 2) (apply cons '(1 (2))))

(procedure? car)
(equal? 1 (apply car '((1 2))))

(procedure? cdr)
(equal? '(2) (apply cdr '((1 2))))

(procedure? +)
(equal? 3 (apply + '(1 2)))

(procedure? -)
(equal? -1 (apply - '(1 2)))

(procedure? *)
(equal? 2 (apply * '(1 2)))

(procedure? /)
(equal? 2 (apply / '(4 2)))

(procedure? <)
(apply < '(1 2))

(procedure? <=)
(apply <= '(1 1))

(procedure? eq?)
(apply eq? '(foo foo))

(procedure? gensym)
(symbol? (apply gensym '()))

(procedure? char->integer)
(equal? 65 (apply char->integer '(#\A)))

(procedure? integer->char)
(equal? #\A (apply integer->char '(65)))

(procedure? char-upcase)
(equal? #\A (apply char-upcase '(#\a)))

(procedure? char-downcase)
(equal? #\a (apply char-downcase '(#\A)))

(procedure? make-string)
(equal? "AAA" (apply make-string '(3 #\A)))
(equal? 3 (string-length (apply make-string '(3))))

(procedure? string-ref)
(equal? #\C (string-ref "ABCD" 2))

(procedure? string-set!)
(let ((x (make-string 4 #\A)))
  (apply string-set! (list x 2 #\X))
  (equal? "AAXA" x))

(procedure? string-length)
(equal? 4 (apply string-length '("AAAA")))

;; quasiquote tests

(eq? `() '())
(eq? `a 'a)
(eq? `,10 10)
(equal? '(a . b) `(a . b))
(equal? '(a b . c) `(a b . c))
(equal? '(a . 4) `(a . ,(+ 2 2)))
(equal? '(a b . 4) `(a b . ,(+ 2 2)))
(equal? '(1 2 3 . `(10 ,(+ 2 2) 20))
        `(1 2 3 . `(10 ,(+ 2 2) 20)))
(equal? '(1 2 3 . `(10 ,4 20))
        `(1 2 3 . `(10 ,,(+ 2 2) 20)))
(equal? '(1 2 3 . 4)
        `(1 ,@(list 2 3) . 4))
(let ((x '(3 4)))
  (equal? `(1 2 ,@x 5)
          '(1 2 3 4 5)))
(let ((x '(3 4)))
  (equal? `(1 2 ((,@x)) 5)
          '(1 2 ((3 4)) 5)))
(let ((x 10) (y 20) (z 30))
  (equal? `(x ,y z) '(x 20 z)))
(let ((x 10) (y 20) (z 30))
  (equal? `(,x ,y ,z) '(10 20 30)))
(let ((x 10) (y 20) (z 30))
  (equal? `(x y z) '(x y z)))
(let ((x 10) (y 20) (z 30))
  (equal? `(x ((,y)) z) '(x ((20)) z)))
(let ((x 10) (y 20) (z 30))
  (equal? ```(x ,,,y z) '``(x ,,20 z)))
(equal? ``(a ,,(+ 1 2) ,(+ 2 3))
        '`(a ,3 ,(+ 2 3)))
(equal? ``,,3 '`,3)
(equal? ```,,,3 '``,,3)
(let ((a 1) (b '(2 3)))
  (equal? `(,a . ,b) '(1 2 3)))
(let ((a 1) (b 2))
  (equal? `(,a . ,b) (cons 1 2)))

;; vectors and quasiquotes

(let ((square (lambda (x) (* x x))))
  (equal? #(10 5 4 16 9 8)
          `#(10 5 ,(square 2) ,@(map square '(4 3)) 8)))
(equal? `#(a `(b ,(foo ,(car '(3 6))) c) d)
        '#(a `(b ,(foo 3) c) d))

(let ((x 10) (y 20) (z 30))
  (equal? ```#(x ,,,y z) '``#(x ,,20 z)))
(equal? ``#(a ,,(+ 1 2) ,(+ 2 3))
        '`#(a ,3 ,(+ 2 3)))
(equal? `(x y #(z (w #(1 ,(+ 2 2)) a) b) c)
        '(x y #(z (w #(1 4) a) b) c))
(equal? ``(x y #(z (w #(1 ,,(+ 2 2)) a) b) c)
        '`(x y #(z (w #(1 ,4) a) b) c))
(equal? '(a b . #(1 2 3 4))
        `(a b . #(1 ,@(list 2 3) 4)))
(equal? '(1 `(2 . ,(+ 1 2)))
        `(1 `(2 . ,(+ 1 2))))
(equal? '(1 `(2 . ,3))
        `(1 `(2 . ,,(+ 1 2))))

;; the following are adopted from husk scheme test suite. see
;; https://github.com/justinethier/husk-scheme/blob/master/tests/t-backquote.scm
(equal? `(list ,(car '(3 6)) 4)
         '(list 3 4))
(equal? (let ((name 'a)) `(list ,name ',name))
        '(list a (quote a)))
(equal? (let ((name 'a)) '(list ,name ',name))
        '(list (unquote name) (quote (unquote name))))
(equal? (let ((name 'a)) `(list ,name (,name)))
        '(list a (a)))
(equal? (let ((name 'a)) `(list ,name ((,name))))
        '(list a ((a))))
(equal? `(a `(b ,(car '(3 6)) ,(foo ,(car '(3 6)) d) e) f)
        '(a `(b ,(car '(3 6)) ,(foo 3 d) e) f))
(equal? (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,,name2 d) e))
        '(a `(b ,x ,y d) e))
(equal? (let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))
        '(a `(b ,x ,'y d) e))
(equal? (quasiquote (list (unquote (car '(3 6))) 4))
        '(list 3 4))
(equal? '(quasiquote (list (unquote (car '(3 6))) 4))
        '`(list ,(car '(3 6)) 4))
(equal? `(a `(b ,(foo ,(car '(3 6))) c) d)
        '(a `(b ,(foo 3) c) d))
(equal? '(x `(,@'(a b c)))
        `(x `(,@'(a b c))))
(equal? '`(,@(+ 1 1))
        ``(,@(+ 1 1)))
(equal? '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
        `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

;; from chez scheme docs at: https://www.scheme.com/tspl2d/control.html
(equal? '(1 . 2)
        (let ((a 1) (b 2))
          `(,a . ,b)))
(equal? '(1 . 2)
        (let ((a 1) (b 2))
          `(,a ,@b)))
(equal? ''(a . b)
        `',(cons 'a 'b))

(let ((append 10)
      (list 20)
      (list* 30)
      (cons 40))
  (equal? '(1 2 3 4 5)
          `(1 ,@'(2 3) ,(+ 2 2) 5)))

;; vectors

(atom? #(1 2 3))
(vector? #(1 2 3))

(equal? '#(1 2 (a b) 3) #(1 2 (a b) 3))
(not (equal? #(1 2 '(a b) 3) #(1 2 (a b) 3)))

;;(equal? #(a a a a a) (make-vector 5 'a))
(= 5 (vector-length (make-vector 5)))

(= 0 (vector-length #()))
(= 3 (vector-length #(1 2 3)))
;;(= 3 (vector-length #0=#(1 2 #0#)))
;;(let ((v #0=#(1 2 #0#)))
;;  (eq? v (vector-ref v 2)))
;;(let ((v #(1 2 #0=(10) #0#)))
;;  (eq? (vector-ref v 2) (vector-ref v 3)))
;;(let ((v #0=#(1 (2 #0#) 3)))
;;  (eq? v (cadr (vector-ref v 1))))
;;(let ((v '#0=(1 #(2 #0#) 3)))
;;  (eq? v (vector-ref (cadr v) 1)))

(= 2 (vector-ref #(1 2 3) 1))

(let ((v #(1 2 3 4)))
  (vector-set! v 1 20)
  (equal? #(1 20 3 4) v))

(eq? '() (vector->list #()))
(equal? '(a b c) (vector->list #(a b c)))
;;(equal? '(1 2 #0=#(1 2 #0#)) (vector->list #1=#(1 2 #1#)))

(equal? #(41 62) (vector-map (lambda (x y z) (+ x y z))
                             #(1 2)
                             #(10 20)
                             #(30 40)))

(let ((result '()))
  (vector-for-each (lambda (x y)
                     (set! result (cons (cons x y) result)))
                   #(10 20 30 40 50)
                   #(a b c))
  (equal? '((30 . c) (20 . b) (10 . a))
          result))

(equal? #() (vector))
(equal? #(a b c) (vector 'a 'b 'c))

(equal? #() (string->vector ""))
(equal? #(#\1 #\2 #\3) (string->vector "123"))
(equal? #(#\c #\d #\e) (string->vector "abcde" 2))
(equal? #(#\c #\d) (string->vector "abcde" 2 4))

(equal? "" (vector->string #()))
(equal? "123" (vector->string #(#\1 #\2 #\3)))
(equal? "cde" (vector->string #(#\a #\b #\c #\d #\e) 2))
(equal? "cd" (vector->string #(#\a #\b #\c #\d #\e) 2 4))

(let ((v #(1 2 3 4 5)))
  (vector-fill! v 'a)
  (equal? #(a a a a a) v))
(let ((v #(1 2 3 4 5)))
  (vector-fill! v 'a 2)
  (equal? #(1 2 a a a) v))
(let ((v #(1 2 3 4 5)))
  (vector-fill! v 'a 2 4)
  (equal? #(1 2 a a 5) v))

(let* ((v #(1 2))
       (r (vector-copy v)))
  (and (not (eq? v r))
       (equal? v r)))
(equal? #(3 4 5) (vector-copy #(1 2 3 4 5) 2))
(equal? #(3 4) (vector-copy #(1 2 3 4 5) 2 4))

(let ((v #(1 2 3 4 5 6 7)))
  (vector-copy! v 2 #(a b))
  (equal? v #(1 2 a b 5 6 7)))
(let ((v #(1 2 3 4 5 6 7)))
  (vector-copy! v 2 #(a b c d e) 3)
  (equal? v #(1 2 d e 5 6 7)))
(let ((v #(1 2 3 4 5 6 7)))
  (vector-copy! v 3 #(a b c d e) 2 4)
  (equal? v #(1 2 3 c d 6 7)))

(equal? #() (vector-append))
(let* ((v #(1 2))
       (a (vector-append v)))
  (and (not (eq? v a)) ;; the return value should be a newly allocated vector
       (equal? #(1 2) a)))
(equal? #(1 2 3 4 5 6) (vector-append #(1 2) #() #(3 4 5 6)))

(let ((b (box 10)))
  (and (box? b)
       (= (unbox b) 10)))
(let ((b (box 10)))
  (set-box! b 20)
  (and (box? b)
       (= (unbox b) 20)))

(let ((x 10) (y 20))
  (set! x 100)
  (and (= x 100) (= y 20)))

(let loop ((x 10) (y 20))
  (set! x 100)
  (and (= x 100) (= y 20)))

(= 120 ((lambda (x y)
          (set! x 100)
          (+ x y))
        10 20))

;; hash tables

(let ((ht (make-eq-hash-table)))
  (hash-table? ht))

(let ((ht (make-hash-table)))
  (hash-table? ht))

(let ((ht (make-hash-table string-ci=? string-ci-hash)))
  (hash-table-set! ht "foo" 100)
  (hash-table-set! ht "bar" 200)
  (hash-table-set! ht "FOO" 1000)
  (and (= 1000 (hash-table-ref ht "FoO"))
       (= 200 (hash-table-ref ht "bar"))))

(let ((ht (make-eq-hash-table)))
  (hash-table-set! ht 'foo 100)
  (hash-table-set! ht 'bar 200)
  (and (= 100 (hash-table-ref ht 'foo))
       (= 200 (hash-table-ref ht 'bar))))

(let ((ht (make-hash-table string-ci=?)))
  ((hash-table-equivalence-function ht) "foo" "Foo"))

(let ((ht (make-hash-table string-ci=? string-ci-hash)))
  (= ((hash-table-hash-function ht) "foo")
     ((hash-table-hash-function ht) "Foo")))

(let ((ht (alist->hash-table '((foo . 100) (bar . 200) (to-del . 10)))))
  (hash-table-delete! ht 'to-del)
  (and (= 100 (hash-table-ref ht 'foo))
       (= 200 (hash-table-ref ht 'bar))
       (= 300 (hash-table-ref ht 'spam (lambda () 300)))
       (= 400 (hash-table-ref/default ht 'spam 400))
       (not (hash-table-exists? ht 'to-del))))

(let ((ht (make-eq-hash-table)))
  (hash-table-set! ht 'foo 100)
  (hash-table-update! ht 'foo (lambda (x) (+ x 1)))
  (hash-table-update! ht 'bar (lambda (x) (+ x 2)) (lambda () 200))
  (hash-table-update!/default ht 'spam (lambda (x) (+ x 3)) 300)
  (and (= 101 (hash-table-ref ht 'foo))
       (= 202 (hash-table-ref ht 'bar))
       (= 303 (hash-table-ref ht 'spam))))

(let ((ht (alist->hash-table '((foo . 100) (bar . 200)))))
  (= 2 (hash-table-size ht)))

(let ((ht (alist->hash-table '((foo . 100) (bar . 200)))))
  (or (equal? (hash-table-keys ht)
              '(foo bar))
      (equal? (hash-table-keys ht)
              '(bar foo))))

(let ((ht (alist->hash-table '((foo . 100) (bar . 200)))))
  (or (equal? (hash-table-values ht)
              '(100 200))
      (equal? (hash-table-values ht)
              '(200 100))))

(let ((ht (alist->hash-table '((foo . 100) (bar . 200)))))
  (let ((x 0))
    (hash-table-walk ht (lambda (k v) (set! x (+ x v))))
    (= x 300)))

(let ((ht (hash-table-merge! (alist->hash-table '((foo . 100) (bar . 200)))
                             (alist->hash-table '((spam . 300) (eggs . 400))))))
  (and (= 4 (hash-table-size ht))
       (= 100 (hash-table-ref ht 'foo))
       (= 200 (hash-table-ref ht 'bar))
       (= 300 (hash-table-ref ht 'spam))
       (= 400 (hash-table-ref ht 'eggs))))

(let ((ht1 (alist->hash-table '((foo . 100) (bar . 200)))))
  (let ((ht2 (hash-table-copy ht1)))
    (and (not (eq? ht1 ht2))
         (= 2 (hash-table-size ht2))
         (= 100 (hash-table-ref ht2 'foo))
         (= 200 (hash-table-ref ht2 'bar)))))

;; environments
;;
;; environment-lookup returns the raw stored value for every kind,
;; including 'primcall, whose value is the canonical name symbol rather
;; than a closure. actually calling a bound primcall requires a read
;; through env_ref, which has no way to achieve in scheme atm.

(environment? (make-empty-environment))
(not (environment? 42))
(not (environment? '()))

(let ((e (make-empty-environment)))
  (environment-bind! e 'x 'value 42)
  (= 42 (cdr (environment-lookup e 'x))))

(let ((e (make-empty-environment)))
  (environment-bind! e 'x 'value 1)
  (environment-bind! e 'x 'value 2)
  (= 2 (cdr (environment-lookup e 'x))))

(let ((e (make-empty-environment)))
  (environment-bind! e 'a 'value 10)
  (environment-bind! e 'b 'value 20)
  (= 30 (+ (cdr (environment-lookup e 'a)) (cdr (environment-lookup e 'b)))))

(not (environment-lookup (make-empty-environment) 'this-name-is-unbound))

(let ((e (make-empty-environment)))
  (environment-bind! e 'l 'special 'lambda)
  (environment-bind! e 'x 'aux 'else)
  (environment-bind! e 'c 'primcall 'car)
  (environment-bind! e 'm 'macro '(a transformer))
  (and (equal? (environment-lookup e 'l) '(special . lambda))
       (equal? (environment-lookup e 'x) '(aux . else))
       (equal? (environment-lookup e 'c) '(primcall . car))
       (equal? (environment-lookup e 'm) '(macro a transformer))))

;; make sure each iteration of a named let gets a fresh variable of its
;; own
(equal? '(0 1 2)
        (let loop ((i 0) (thunks '()))
          (if (= i 3)
              (map (lambda (t) (t)) (reverse thunks))
              (loop (+ i 1) (cons (lambda () i) thunks)))))

;; redefining a name as a different kind fully replaces the old entry
(let ((e (make-empty-environment)))
  (environment-bind! e 'x 'macro '(a transformer))
  (environment-bind! e 'x 'value 42)
  (equal? (environment-lookup e 'x) '(value . 42)))

;; regression test. this used to fail with a confusing runtime error
;; about record types, caused by the fact that we instantiated the
;; (scheme eval) library more than once and corrupted the record type
;; ids. the nesting matters: e imports (scheme eval), and the inner
;; environment call runs inside e, re-entering the same running .so
;; that the outer eval is already executing. a non-reentrant program
;; (just calling environment once) never hits this.
(let* ((e  (environment '(whisper) '(scheme eval)))
       (e2 (eval '(environment '(scheme eval)) e)))
  (environment? e2))

;; assoc is a library defined value (not a primcall), so it should be
;; the same in any environment that imports it, because per r7rs each
;; library should only be instantiated once.
(let ((e1 (environment '(scheme base)))
      (e2 (environment '(scheme base))))
  (eq? (eval 'assoc e1) (eval 'assoc e2)))

(equal? 3 (eval '(+ 1 2) (environment '(scheme base))))

;; (scheme eval) is statically linked into this program (imported at the
;; top of this file) and also dynamically loaded here via
;; environment/eval. those two should hold the same exact reference to
;; the environment function.
(eq? environment (eval 'environment (environment '(scheme eval))))

;; eval should pass on multiple values
(call-with-values (lambda () (eval '(values 1 2) (environment '(scheme base))))
                  (lambda (a b) (and (equal? 1 a) (equal? 2 b))))

;; eval should pass on multiple values, this time created by a captured
;; continuation
(call-with-values (lambda () (eval '(call/cc (lambda (k) (k 1 2))) (environment '(scheme base))))
                  (lambda (a b) (and (equal? 1 a) (equal? 2 b))))

;; a primcall closure's identity must survive a runtime library load,
;; not just be re-created fresh as we used to do. this is probably not
;; required by r7rs but it's still nice so we check for it.
(let ((f car))
  (eval '1 (environment '(scheme base)))
  (eq? f car))

;; quoted data containing binding-form-shaped lists must not be
;; interpreted as code by the preprocessor
(equal? '(lambda 5 6) '(lambda 5 6))
(equal? '(let) '(let))
(equal? '(define x) '(define x))
(eq? (car '(lambda x x)) 'lambda)
(eq? (length '(a b c)) 3)

;; regression: set! on a captured (free) variable must write through
;; the closure environment, not refer to a nonexistent C local.
;; before the fix, compile-set! only handled global and local, causing
;; a C compile error like: '__35n__1' undeclared.
(let ((make-counter (lambda ()
                      (let ((n 0))
                        (lambda ()
                          (set! n (+ n 1))
                          n)))))
  (let ((c (make-counter)))
    (and (= (c) 1) (= (c) 2) (= (c) 3))))

;; macros

;; regression: set! introduced by a macro expansion must correctly trigger
;; boxing for the target variable. previously this failed at runtime with
;; "set-box! first argument is not a box".
(define-syntax increment!
  (syntax-rules ()
    ((_ var) (set! var (+ var 1)))))

(let ((x 0))
  (increment! x)
  (= x 1))

;; template-introduced binding must not capture a same-named global
(define a1-t 5)
(define-syntax a1-or
  (syntax-rules ()
    ((_) #f)
    ((_ e) e)
    ((_ e1 e2 ...) (let ((a1-t e1)) (if a1-t a1-t (a1-or e2 ...))))))
(= 5 (a1-or #f a1-t))
(= 50 (let ((a1-t 50)) (a1-or #f a1-t)))
(= 5 (a1-or #f #f a1-t))

;; set! inside a macro argument must write through to the user's
;; global, not the template's own same-named binding
(define a2-acc 99)
(define-syntax a2-twice
  (syntax-rules () ((_ e) (let ((a2-acc 0)) e e a2-acc))))
(and (= 0 (a2-twice (set! a2-acc (+ a2-acc 1))))
     (= 101 a2-acc))

(define-syntax a2-addx (syntax-rules () ((_ e) (let ((x e)) (+ x x)))))
(= 20 (let ((x 10)) (a2-addx x)))

;; macro that ignores its argument must not choke on the argument's
;; shape, since it is inert data until the macro examines it
(define-syntax b1-ignore-it (syntax-rules () ((_ x) 'ok)))
(eq? 'ok (b1-ignore-it (lambda 5 6)))

;; a macro that only inspects the head of a let-shaped argument must not
;; have that argument parsed as a real let
(define-syntax b2-fst (syntax-rules () ((_ (a . rest)) (quote a))))
(eq? 'let (b2-fst (let ((1 2)) 3)))

;; internal defines are converted to letrec*
(define (c1-f x)
  (define y (+ x 1))
  (define z (* y 2))
  z)
(= 10 (c1-f 4))

;; internal defines see each other, including forward references
(define (c1b-f)
  (define (even2? n) (if (= n 0) #t (odd2? (- n 1))))
  (define (odd2? n) (if (= n 0) #f (even2? (- n 1))))
  (even2? 10))
(c1b-f)

;; let-syntax introduces a local macro
(= 42 (let-syntax ((c2-dbl (syntax-rules () ((_ x) (* 2 x)))))
        (c2-dbl 21)))

;; letrec-syntax siblings see each other, including self-reference
(equal? '(5) (letrec-syntax ((c2b-m1 (syntax-rules () ((_ x) (c2b-m2 x))))
                             (c2b-m2 (syntax-rules () ((_ x) (list x)))))
               (c2b-m1 5)))
(equal? '(((0))) (letrec-syntax ((c2b-cnt (syntax-rules ()
                                            ((_ ()) 0)
                                            ((_ (a . r)) (list (c2b-cnt r))))))
                   (c2b-cnt (x y z))))

;; under let-syntax (not letrec-syntax), siblings do NOT see each other;
;; a use of the sibling name stays an ordinary (unexpanded) call
(define (lrs3-p2 x) (list 'plain x))
(equal? '(plain 5)
        (let-syntax ((lrs3-p1 (syntax-rules () ((_ x) (lrs3-p2 x))))
                     (lrs3-p2 (syntax-rules () ((_ x) (list 'macro x)))))
          (lrs3-p1 5)))

;; body-level define-syntax
(define (c3-f)
  (define-syntax c3-q (syntax-rules () ((_ x) (+ x 1))))
  (c3-q 5))
(= 6 (c3-f))

;; `_` is a non-capturing wildcard, and two underscores in one pattern
;; do not conflict with each other
(define-syntax d1-second (syntax-rules () ((_ (a b _)) b)))
(= 20 (d1-second (10 20 30)))
(define-syntax d1-two-wild (syntax-rules () ((_ (a _ _)) a)))
(= 1 (d1-two-wild (1 2 3)))

;; vector patterns, matched against (not just built by, as in E11)
(define-syntax d1-vfirst (syntax-rules () ((_ #(a b ...)) a)))
(= 1 (d1-vfirst #(1 2 3)))

;; a macro expanding to a pair of top-level defines (hidden state) must
;; not capture, and must not be captured by, a same-named global
(define-syntax e1-def-counter
  (syntax-rules ()
    ((_ name)
     (begin
       (define n 0)
       (define (name) (set! n (+ n 1)) n)))))
(define n 'user)
(e1-def-counter e1-tick)
(equal? (list 1 2 'user) (list (e1-tick) (e1-tick) n))

;; independent expansions of the same macro get independent hidden
;; state, since each expansion gets a fresh hygienic rename
(e1-def-counter e2-a)
(e1-def-counter e2-b)
(let ()
  (e2-a)
  (e2-a)
  (e2-b)
  (equal? '(3 2) (list (e2-a) (e2-b))))

;; the same hidden-state macro used inside a body goes through the
;; internal-define (letrec*) path instead of top-level begin splicing:
;; the two introduced defines must still connect and the introduced n
;; must still be boxed
(equal? '(1 2)
        (let ()
          (e1-def-counter tick)
          (list (tick) (tick))))

;; a macro can generate a usable macro; the outer pattern variable flows
;; into and is frozen inside the inner template
(define-syntax e4-def-adder
  (syntax-rules ()
    ((_ name k)
     (define-syntax name
       (syntax-rules () ((_ x) (+ x k)))))))
(e4-def-adder e4-add5 5)
(= 15 (e4-add5 10))

;; two-level referential transparency: a generated macro's reference to
;; a helper traces back through the outer macro's definition site, even
;; under a use-site shadow of the same name. rt-helper is deliberately
;; shared with other tests too.
(define (rt-helper x) (* x 100))
(define-syntax e5-make-user
  (syntax-rules ()
    ((_ name)
     (define-syntax name
       (syntax-rules () ((_ e) (rt-helper e)))))))
(e5-make-user e5-u)
(= 500 (let ((rt-helper (lambda (x) (- x)))) (e5-u 5)))

;; a generated macro can recurse on its own use-site name
(define-syntax e6-def-len
  (syntax-rules ()
    ((_ name)
     (define-syntax name
       (syntax-rules ()
         ((_ ()) 0)
         ((_ (a . rest)) (+ 1 (name rest))))))))
(e6-def-len e6-len)
(= 3 (e6-len (a b c)))

;; a generated macro's literal (here `=>`) is aliased consistently
;; between its declaration and its pattern, so it is still recognized as
;; a literal (not a pattern variable) after renaming
(define-syntax e8-def-arrow
  (syntax-rules ()
    ((_ name)
     (define-syntax name
       (syntax-rules (=>) ((_ (a => b)) (list a b)))))))
(e8-def-arrow e8-arrow)
(equal? '(1 2) (e8-arrow (1 => 2)))

;; the (... ...) escape emits a literal ... into a generated macro
;; without the outer ellipsis consuming it
(define-syntax e10-be-like-begin
  (syntax-rules ()
    ((_ name)
     (define-syntax name
       (syntax-rules () ((_ e (... ...)) (begin e (... ...))))))))
(e10-be-like-begin e10-seq)
(= 4 (e10-seq 1 2 3 4))

;; the general escape (... <template>) driving a generated macro that
;; itself uses a real ellipsis. the escape covers a whole compound
;; template (not just a bare ...), so it must copy the pattern variable
;; and the literal ellipsis through together
(define-syntax e10b-def-wrap
  (syntax-rules ()
    ((_ name wrapper)
     (define-syntax name
       (syntax-rules ()
         ((... (_ args ...)) (... (wrapper (quote tag) args ...))))))))
(e10b-def-wrap e10b-listw list)
(equal? '(tag 1 2 3) (e10b-listw 1 2 3))

;; a custom ellipsis declared inside a generated macro is picked up over
;; the default ...
(define-syntax e11-def-cl
  (syntax-rules ()
    ((_ name)
     (define-syntax name
       (syntax-rules ::: () ((_ x :::) (vector x :::)))))))
(e11-def-cl e11-vec)
(equal? (vector 1 2 3) (e11-vec 1 2 3))

;; an introduced define-syntax and an introduced define connect to each
;; other through one expansion, while staying invisible to the outside
(define-syntax e12-def-doubler
  (syntax-rules ()
    ((_ name)
     (begin
       (define-syntax dbl (syntax-rules () ((_ x) (* 2 x))))
       (define (name v) (dbl v))))))
(e12-def-doubler e12-fdbl)
(= 42 (e12-fdbl 21))

;; the same macro used inside a body: the introduced define-syntax and
;; the introduced define both land in a letrec* and must still connect
(= 42
   (let ()
     (e12-def-doubler doubler)
     (doubler 21)))

;; a macro expanding to (begin (define ...) (expr using it ...))
;; connects the definition and use through one expansion (body position)
(define-syntax e13-with-secret-body
  (syntax-rules () ((_) (begin (define e13-foo 100) (+ e13-foo 200)))))
(define (e13-body-test) (e13-with-secret-body))
(= 300 (e13-body-test))

(define (e13-body-sibling)
  (let ((e13-foo 'sibling-value))
    (e13-with-secret-body)
    e13-foo))
(eq? 'sibling-value (e13-body-sibling))

;; the same shape at top level splices into two top-level forms, and a
;; later top-level define of the same source name creates a distinct
;; global rather than redefining the introduced one
(define-syntax e13-with-secret-top
  (syntax-rules ()
    ((_) (begin
           (define e13-top-foo 100)
           (= 300 (+ e13-top-foo 200))))))
(e13-with-secret-top)
(define e13-top-foo 'unrelated)
(eq? 'unrelated e13-top-foo)

;; introduced temp does not capture a user local across set! (swap)
(define-syntax k2-swp
  (syntax-rules ()
    ((_ a b) (let ((tmp a))
               (set! a b)
               (set! b tmp)))))
(equal? '(2 1) (let ((tmp 1)
                     (y 2))
                 (k2-swp tmp y)
                 (list tmp y)))

;; referential transparency to a primitive under a local shadow
(define-syntax k3-pairup
  (syntax-rules ()
    ((_ x) (cons x x))))
(equal? '(7 . 7) (let ((cons (lambda (a b) 'hijacked)))
                   (k3-pairup 7)))

;; referential transparency to a global procedure under a local shadow
(define-syntax k4-usehelper
  (syntax-rules ()
    ((_ e) (rt-helper e))))
(= 500 (let ((rt-helper (lambda (x) (- x))))
         (k4-usehelper 5)))

;; recursive macro, nested ellipsis, dotted/tail patterns, and
;; non-identifier literals
(define-syntax k5-let*
  (syntax-rules ()
    ((_ ((v e) ...) body)
     (let ((v e) ...) body))))
(= 3 (k5-let* ((a 1) (b 2)) (+ a b)))
(define-syntax k5-flatten2
  (syntax-rules ()
    ((_ ((x ...) ...))
     (quote (x ... ...)))))
(equal? '(1 2 3 4 5) (k5-flatten2 ((1 2) (3 4 5))))

;; nested ellipsis where the inner ellipsis is inside its own
;; parentheses, and there are different inner lengths per outer
;; repetition. this used to fail to a bug in how nested ellipses were
;; handled.
(define-syntax k5-nested-ragged
  (syntax-rules ()
    ((_ (tag item ...) ...)
     (list (list 'tag item ...) ...))))
(equal? '((a 1 2) (b 3 4 5))
        (k5-nested-ragged (a 1 2) (b 3 4 5)))

;; same shape as k5-flatten2, but with vector patterns/templates instead
;; of list ones
(define-syntax k5-vec-flatten
  (syntax-rules ()
    ((_ #(#(x ...) ...))
     (vector x ... ...))))
(equal? #(1 2 3 4 5) (k5-vec-flatten #(#(1 2) #(3 4 5))))

;; same shape as k5-nested-ragged, but with vector patterns/templates
;; instead of list ones
(define-syntax k5-vec-nested-ragged
  (syntax-rules ()
    ((_ #(#(tag item ...) ...))
     (vector (vector 'tag item ...) ...))))
(equal? #(#(a 1 2) #(b 3 4 5))
        (k5-vec-nested-ragged #(#(a 1 2) #(b 3 4 5))))

;; a list of vectors, each vector with its own nested ellipsis: list
;; and vector nesting combined, ragged inner lengths
(define-syntax k5-combo-list-of-vecs
  (syntax-rules ()
    ((_ #(tag item ...) ...)
     (list (vector 'tag item ...) ...))))
(equal? '(#(a 1 2) #(b 3 4 5))
        (k5-combo-list-of-vecs #(a 1 2) #(b 3 4 5)))

;; the reverse combination: a vector of lists, each list with its own
;; nested ellipsis
(define-syntax k5-combo-vec-of-lists
  (syntax-rules ()
    ((_ #((tag item ...) ...))
     (vector (list 'tag item ...) ...))))
(equal? #((a 1 2) (b 3 4 5))
        (k5-combo-vec-of-lists #((a 1 2) (b 3 4 5))))

;; three levels of ellipsis, ragged at every level
(define-syntax k5-depth3-ragged
  (syntax-rules ()
    ((_ ((b ...) ...) ...)
     (list (list (list b ...) ...) ...))))
(equal? '(((1 2) (3 4 5)) ((6)))
        (k5-depth3-ragged ((1 2) (3 4 5)) ((6))))

;; four levels of ellipsis, ragged at every level
(define-syntax k5-depth4-ragged
  (syntax-rules ()
    ((_ (((b ...) ...) ...) ...)
     (list (list (list (list b ...) ...) ...) ...))))
(equal? '((((1 2) (3)) ((4 5 6))) (((7))))
        (k5-depth4-ragged (((1 2) (3)) ((4 5 6))) (((7)))))

(define-syntax k5-double-via-aux
  (syntax-rules ()
    ((_ "go" x) (* x 2))
    ((_ x) (k5-double-via-aux "go" x))))
(= 42 (k5-double-via-aux 21))

;; macro-introduced top-level definitions via begin splicing
(define-syntax k6-def2
  (syntax-rules ()
    ((_ a b v) (begin
                 (define a v)
                 (define b v)))))
(k6-def2 k6-p k6-q 42)
(= 84 (+ k6-p k6-q))

;; the same begin-spliced defines inside a body: the use-site names are
;; bound as internal defines (letrec*) and stay referenceable afterward
(= 84
   (let ()
     (k6-def2 p q 42)
     (+ p q)))

;; referential transparency to lexical variables
(let ((x 5))
  (define-syntax foo (syntax-rules ()
                       ((_) x)))
  (let ((x 10))
    (= (foo) 5)))

;; quasiquote inside macro
(define-syntax qqm (syntax-rules ()
                     ((_ a) `(x ,a ,@(list a a)))))
(equal? '(x 5 5 5) (qqm 5))

;; let-syntax shadowing
(= 2 (let-syntax ((m (syntax-rules () ((_) 1))))
       (let-syntax ((m (syntax-rules () ((_) 2))))
         (m))))

;; mix internal define-syntax and define
(define (mixf)
  (define-syntax getx (syntax-rules () ((_) x)))
  (define x 42)
  (getx))
(= 42 (mixf))

;; an inner ellipsis consumes the depth of the variable it repeats, so
;; the outer ellipsis must not slice that variable as well. here the
;; outer ellipsis iterates over x alone while y stays whole.
(let ()
  (define-syntax foo
    (syntax-rules ()
      ((_ "A" (x y) ...)
       '(AAA (x y) ...))
      ((_ "B" (x y) ...)
       '(BBB (x y ...) ...))))
  (and (equal? '(AAA (a 10) (b 20) (c 30))
               (foo "A" (a 10) (b 20) (c 30)))
       (equal? '(BBB (a 10 20 30) (b 10 20 30) (c 10 20 30))
               (foo "B" (a 10) (b 20) (c 30)))))

;; z has depth 2 and the inner ellipsis consumes only one level, so
;; unlike y above, the outer ellipsis does still expand it
(let ()
  (define-syntax bar
    (syntax-rules ()
      ((_ (x y z ...) ...)
       '(AA (100 200 z ...) ...))))
  (equal? '(AA (100 200 3) (100 200) (100 200 c))
          (bar (1 2 3) (10 20) (a b c))))

;; two ellipses in a row flatten a depth 2 variable onto a vector
;; subtemplate
(let ()
  (define-syntax foo
    (syntax-rules (xx)
      ((_ xx (x ...) ...)
       '(XX #(x) ... ...))))
  (equal? (foo xx (1 2 3) (a b))
          '(XX #(1) #(2) #(3) #(a) #(b))))

;; tco: self tail calls compile to a goto. capture of an unmutated loop
;; var must see that iteration's own value, not the final one.
(equal? '(0 1 2)
        (let loop ((i 0) (thunks '()))
          (if (= i 3)
              (map (lambda (t) (t)) (reverse thunks))
              (loop (+ i 1) (cons (lambda () i) thunks)))))

;; tco: a captured AND set!-mutated loop var must get a fresh box each
;; iteration, so earlier closures don't alias the final value.
(equal? '(1 2 3)
        (let loop ((i 0) (thunks '()))
          (if (= i 3)
              (map (lambda (t) (t)) (reverse thunks))
              (let ((c (lambda () i)))
                (set! i (+ i 1))
                (loop i (cons c thunks))))))

;; tco: a non-tail self call must still go through the normal call path
;; and compute correctly.
(= 15 (let loop ((n 5))
        (if (= n 0) 0 (+ n (loop (- n 1))))))

;; tco: a loop closure that escapes and is called again later from a
;; different function must go through the real closure call, not a goto.
(let ((escaped-loop #f))
  (let ((result (let loop ((i 0) (acc 0))
                  (set! escaped-loop loop)
                  (if (= i 5) acc (loop (+ i 1) (+ acc i))))))
    (and (= 10 result)
         (= 110 (escaped-loop 0 100)))))

;; tco: a deep iteration count that would previously overflow the C
;; stack must run in constant stack space.
(= 10000000 (let loop ((i 0)) (if (= i 10000000) i (loop (+ i 1)))))

;; calls f with itself and a decreasing counter until n hits 0. this is
;; used in the following tests instead of writing a function that calls
;; itself, so that we're sure the call cannot be optimized into a goto
;; by the compiler (since the function is passed as a parameter and not
;; named directly).
(define (deep-loop f n) (if (= n 0) n (f f (- n 1))))

;; a tail call through a parameter, many iterations, must run in
;; constant stack.
(= 0 (deep-loop deep-loop 10000000))

;; apply in tail position, many iterations, must run in constant stack.
(define (apply-loop . args)
  (let ((n (car args)))
    (if (= n 0)
        'done
        (apply apply-loop (list (- n 1))))))
(eq? 'done (apply-loop 10000000))

;; a tail call with 9 arguments (more than it fits inline) must still
;; pass them all through correctly.
(define (sum9 a b c d e f g h i) (+ a b c d e f g h i))
(define (call-with-9 g) (g 1 2 3 4 5 6 7 8 9))
(= 45 (call-with-9 sum9))

;; a hash-table-ref default thunk, and hash-table-update!'s default
;; thunk and update function, must produce their real result correctly
;; even if their own body tail-recurses deeply before returning it.
(and (= 0 (hash-table-ref (make-eq-hash-table) 'missing (lambda () (deep-loop deep-loop 1000000))))
     (let ((ht (make-eq-hash-table)))
       (hash-table-update! ht 'missing
                            (lambda (x) (deep-loop deep-loop 1000000))
                            (lambda () (deep-loop deep-loop 1000000)))
       (= 0 (hash-table-ref ht 'missing))))

;; a custom hash/equivalence function passed to make-hash-table must
;; work correctly even when its own tail call chain (not just some
;; nested non-tail call inside it) is what tail-recurses deeply before
;; producing its result. eq_fn_wrapper/hash_fn_wrapper call these
;; directly and must resolve the escape themselves.
(define (deep-eq-loop f n a b) (if (= n 0) (string-ci=? a b) (f f (- n 1) a b)))
(define (my-eq? a b) (deep-eq-loop deep-eq-loop 1000000 a b))
(define (deep-hash-loop f n k) (if (= n 0) (string-ci-hash k) (f f (- n 1) k)))
(define (my-hash k) (deep-hash-loop deep-hash-loop 1000000 k))
(let ((ht (make-hash-table my-eq? my-hash)))
  (hash-table-set! ht "FoO" 1000)
  (= 1000 (hash-table-ref ht "foo")))

;; multiple values: the r7rs examples.
(= 5 (call-with-values (lambda () (values 4 5)) (lambda (a b) b)))
(= -1 (call-with-values * -))

;; a lone value passes through unwrapped: values of one argument is the
;; identity, so it works in an ordinary single-value context.
(= 6 (+ 1 (values 5)))
(= 5 (call-with-values (lambda () (values 5)) (lambda (x) x)))

;; zero values consumed by a thunk consumer.
(eq? 'ok (call-with-values (lambda () (values)) (lambda () 'ok)))

;; the consumer's continuation is call-with-values's continuation, so a
;; call-with-values in the tail of a producer forwards its consumer's
;; multiple values outward (nested case).
(equal? '(2 1)
        (call-with-values
          (lambda () (call-with-values (lambda () (values 1 2)) (lambda (a b) (values b a))))
          (lambda (x y) (list x y))))

;; multiple values threaded out of a self-tail-recursive named let: the
;; goto path preserves flags, so ACCEPTS_MVALUES survives the loop and
;; the tail (values 'a 'b) is still legal at the base case.
(equal? '(a b)
        (call-with-values
          (lambda ()
            (let loop ((i 1000000))
              (if (= i 0) (values 'a 'b) (loop (- i 1)))))
          (lambda (x y) (list x y))))

;; (apply values ...) in tail position of the producer, past the inline
;; tail-call arg limit.
(= 45 (call-with-values (lambda () (apply values '(1 2 3 4 5 6 7 8 9)))
                        (lambda args (apply + args))))

;; produce 5000 values and count them in a variadic consumer
(= 5000 (call-with-values (lambda () (apply values (iota 5000)))
                          (lambda args (length args))))

;; sum 1..1000 delivered as 1000 separate values.
(= 500500 (call-with-values (lambda () (apply values (iota 1000 1)))
                            (lambda args (apply + args))))

;; r7rs requires that: the continuations of all non-final expressions in
;; a sequence (lambda, case-lambda, begin, let, let*, letrec, letrec*,
;; let-values, let*-values, let-syntax, letrec-syntax, parameterize,
;; guard, case, cond, when, and unless) accept any number of values,
;; since they discard whatever they're given regardless. begin, let,
;; letrec, letrec*, and lambda are the primitives; everything else on
;; that list is a macro that bottoms out in one of them.
(begin (values 1 2 3) #t)
(begin (values) #t)
(let () (values 1 2 3) #t)
(letrec () (values 1 2 3) #t)
(letrec* () (values 1 2 3) #t)
(= 1 ((lambda () (values 1 2 3) 1)))
(guard (e (#t #f)) (values 1 2 3) #t)
(let-values (((a b) (values 1 2))) (values 3 4 5) #t)
(= 99 ((case-lambda ((x) (values 1 2 3) x)) 99))

;; an if's branches are exactly as discarded (or not) as the if itself,
;; so both arms may return multiple values here regardless of which one
;; runs.
(begin (if #t (values 1 2 3) (values 4 5 6)) #t)
(begin (if #f (values 1 2 3) (values 4 5 6)) #t)

;; a discarded position stays discarded through further nested discarded
;; positions.
(= 1 ((lambda ()
        (let ()
          (begin (values 1 2) (values 3 4 5))
          1))))

;; cond, when, and unless are on the same r7rs list, via their bodies
;; lowering to begin.
(cond (#t (values 1 2 3) #t))
(when #t (values 1 2 3) #t)
(unless #f (values 1 2 3) #t)

;; "and" and "or" are not on that list: their non-final operands are
;; real test expressions whose value is used (to decide whether to keep
;; going), not unconditionally discarded, so they still require exactly
;; one value even outside tail position. some scheme implementations
;; allow this, but chez scheme (correctly) disallows it.
(guard (e (#t #t))
  (and (values 1 2 3) #t)
  #f)

;; invoking the continuation abandons the pending (+ 10 ...)
(= 30 (+ 10 (call/cc (lambda (k) (k 20)))))

;; invoking the continuation abandons a pending (+ 2 5 ...)
(= 3 (call/cc (lambda (k) (+ 2 5 (k 3)))))

;; the thunk returns normally when the continuation is never invoked
(= 100 (call/cc (lambda (k) 100)))

;; the reified continuation is a procedure
(procedure? (call/cc (lambda (k) k)))

;; try the longer alias for call/cc
(= 42 (call-with-current-continuation (lambda (k) (+ 5 (k 42)))))

;; early exit out of a map
(= -3 (call/cc
       (lambda (exit)
         (map (lambda (x) (if (negative? x) (exit x) x))
              '(54 0 37 -3 245 19))
         #t)))

;; the inner continuation escapes all the way out to the outer one, so
;; the outer (+ 10 ...) never runs
(= 35 (+ 3 (call/cc
            (lambda (k1)
              (+ 10 (call/cc (lambda (k2) (k1 32))))))))

;; escaping only to the inner continuation, so the outer add still applies
(= 45 (+ 3 (call/cc
            (lambda (k1)
              (+ 10 (call/cc (lambda (k2) (k2 32))))))))

;; a continuation delivering several values to call-with-values
(equal? '(4 5)
        (call-with-values (lambda () (call/cc (lambda (k) (k 4 5))))
                          list))
(equal? '()
        (call-with-values (lambda () (call/cc (lambda (k) (k))))
                          list))
(= 6 (call-with-values (lambda () (call/cc (lambda (k) (k 1 2 3))))
                       (lambda (a b c) (+ a b c))))

;; short circuit a product on the first zero, so nothing past it multiplies
(= 0 (call/cc
      (lambda (k)
        (let loop ((l '(1 2 3 0 4 5)))
          (cond ((null? l) 1)
                ((= (car l) 0) (k 0))
                (else (* (car l) (loop (cdr l)))))))))
(= 120 (call/cc
        (lambda (k)
          (let loop ((l '(1 2 3 4 5)))
            (cond ((null? l) 1)
                  ((= (car l) 0) (k 0))
                  (else (* (car l) (loop (cdr l)))))))))

;; a saved continuation used as a backward goto to count to 5. the counter
;; is a boxed variable on the heap, so it survives each re-entry while the
;; stack image is reinstated.
(let ((k #f) (n 0))
  (call/cc (lambda (c) (set! k c)))
  (set! n (+ n 1))
  (if (< n 5) (k #f))
  (= n 5))

;; invoke a continuation many times, stressing repeated save and restore
;; of the stack image
(let ((k #f) (n 0))
  (call/cc (lambda (c) (set! k c)))
  (set! n (+ n 1))
  (if (< n 1000) (k #f))
  (= n 1000))

;; capture deep in a recursion and re-invoke from the shallower let body,
;; which forces the stack image to be reinstated above the current frame
(let ((k #f) (n 0))
  (define (deep m)
    (if (= m 0)
        (begin (call/cc (lambda (c) (set! k c))) 0)
        (+ 1 (deep (- m 1)))))
  (deep 300)
  (set! n (+ n 1))
  (if (< n 4) (k #f))
  (= n 4))

;; churn allocation between re-entries so a collection runs while a
;; captured continuation is live, exercising the scan of its saved stack
(let ((k #f) (n 0) (payload (list 1 2 3 4 5)))
  (call/cc (lambda (c) (set! k c)))
  (set! n (+ n 1))
  (let loop ((i 0))
    (if (< i 5000) (begin (cons i i) (loop (+ i 1)))))
  (if (< n 20) (k #f))
  (equal? payload '(1 2 3 4 5)))

;; escape out of a deep non-tail recursion in a single longjmp
(eq? 'done
     (call/cc
      (lambda (k)
        (let loop ((n 2000))
          (if (= n 0) (k 'done) (+ n (loop (- n 1))))))))

;; call/cc should pass multiple-values normally
(equal? '(1 2 3)
        (call-with-values (lambda ()
                            (call/cc (lambda (k) (values 1 2 3))))
          list))

;;

(let* ((ls '())
       (r (do ((i 0 (+ i 1))
               (j 3 (- j 1))
               (x 100))
              ((>= i 3)
               (set! ls (cons 'foo ls))
               (set! ls (cons 'bar ls))
               1000)
            (set! ls (cons (list i j x) ls)))))
  (and (= r 1000)
       (equal? ls '(bar foo (2 1 100) (1 2 100) (0 3 100)))))

(= 100 (do () (#t 100)))

;;

(let ()
  (define (foo)
    (values 100 200))
  (define-values (x y) (foo))
  (and (= x 100)
       (= y 200)))

(let*-values (((a b) (values 1 2))
              ((x y) (values a b)))
  (equal? '(1 2 1 2) (list a b x y)))

(let*-values (((a b) (values 1 2))
              (x (values a b)))
  (equal? '(1 2 (1 2)) (list a b x)))

(let-values (((x y) (values 1 2)))
  (and (= x 1)
       (= y 2)))

(let-values (((x y) (values 1 2))
             (foo (values 10 20))
             ((a b c) (values 3 4 5)))
  (and (= x 1)
       (= y 2)
       (= a 3)
       (= b 4)
       (equal? foo '(10 20))))

(let-values ((x (values 1 2)))
  (equal? x '(1 2)))

(let-values ((() (values)))
  #t)

;; dynamic-wind

(equal?
 '(connect talk1 disconnect
   connect talk2 disconnect)
 (let ((path '())
       (c #f))
   (let ((add (lambda (s)
                (set! path (cons s path)))))
     (dynamic-wind
         (lambda () (add 'connect))
         (lambda ()
           (add (call/cc
                 (lambda (c0)
                   (set! c c0)
                   'talk1))))
         (lambda () (add 'disconnect)))
     (if (< (length path) 4)
         (c 'talk2)
         (reverse path)))))

;; dynamic-wind: before, during, after run in that order
(equal? '(in body out)
        (let ((log '()))
          (dynamic-wind
              (lambda () (set! log (cons 'in log)))
              (lambda () (set! log (cons 'body log)))
              (lambda () (set! log (cons 'out log))))
          (reverse log)))

;; dynamic-wind: the body's value is the value of the whole dynamic-wind
(= 42 (dynamic-wind (lambda () #f) (lambda () 42) (lambda () #f)))

;; dynamic-wind: multiple values from the body pass through unchanged
(equal? '(1 2 3)
        (call-with-values
         (lambda () (dynamic-wind (lambda () #f)
                                  (lambda () (values 1 2 3))
                                  (lambda () #f)))
         list))

;; dynamic-wind: zero values from the body pass through unchanged
(equal? '()
        (call-with-values
         (lambda () (dynamic-wind (lambda () #f)
                                  (lambda () (values))
                                  (lambda () #f)))
         list))

;; dynamic-wind: the after thunk runs when the body escapes through a
;; continuation
(equal? '(in out)
        (let ((log '()))
          (call/cc
           (lambda (k)
             (dynamic-wind
                 (lambda () (set! log (cons 'in log)))
                 (lambda () (k #f))
                 (lambda () (set! log (cons 'out log))))))
          (reverse log)))

;; dynamic-wind: escaping past two nested extents runs both after thunks
;; inside out
(equal? '(in1 in2 out2 out1)
        (let ((log '()))
          (call/cc
           (lambda (k)
             (dynamic-wind
                 (lambda () (set! log (cons 'in1 log)))
                 (lambda ()
                   (dynamic-wind
                       (lambda () (set! log (cons 'in2 log)))
                       (lambda () (k #f))
                       (lambda () (set! log (cons 'out2 log)))))
                 (lambda () (set! log (cons 'out1 log))))))
          (reverse log)))

;; dynamic-wind: re-entering the extent through a saved continuation
;; re-runs the before thunk, and each normal exit runs the after thunk
(let ((log '()) (k #f) (count 0))
  (dynamic-wind
      (lambda () (set! log (cons 'in log)))
      (lambda () (call/cc (lambda (c) (set! k c))))
      (lambda () (set! log (cons 'out log))))
  (set! count (+ count 1))
  (if (< count 3) (k #f))
  (equal? (reverse log) '(in out in out in out)))

;; dynamic-wind: jumping to a continuation captured between two extents
;; unwinds only the inner one, then the outer completes normally
(equal? '(in-outer in-inner out-inner out-outer)
        (let ((log '()) (k #f))
          (dynamic-wind
              (lambda () (set! log (cons 'in-outer log)))
              (lambda ()
                (call/cc (lambda (c) (set! k c)))
                (when k
                  (let ((saved k))
                    (set! k #f)
                    (dynamic-wind
                        (lambda () (set! log (cons 'in-inner log)))
                        (lambda () (saved #f))
                        (lambda () (set! log (cons 'out-inner log)))))))
              (lambda () (set! log (cons 'out-outer log))))
          (reverse log)))

;; dynamic-wind: on a plain non-escaping run each guard fires exactly
;; once
(= 1 (let ((ins 0) (outs 0))
       (dynamic-wind
           (lambda () (set! ins (+ ins 1)))
           (lambda () #t)
           (lambda () (set! outs (+ outs 1))))
       (if (and (= ins 1) (= outs 1)) 1 0)))

;; dynamic-wind: a generator built from call/cc and dynamic-wind: the
;; after thunk runs on every yield out of the extent, the before thunk
;; on every resume back in
(equal? '(enter leave 1 enter leave 2 enter leave 3)
        (let ((log '()) (resume #f) (return #f))
          (define (yield v)
            (call/cc
             (lambda (k)
               (set! resume k)
               (return v))))
          (define (driver)
            (dynamic-wind
                (lambda () (set! log (cons 'enter log)))
                (lambda () (yield 1) (yield 2) (yield 3))
                (lambda () (set! log (cons 'leave log)))))
          (define (step)
            (call/cc
             (lambda (k)
               (set! return k)
               (if resume (resume #f) (driver)))))
          (set! log (cons (step) log))
          (set! log (cons (step) log))
          (set! log (cons (step) log))
          (reverse log)))

;; exceptions

;; with-exception-handler's handler escapes via a call/cc captured
;; before with-exception-handler was entered
(equal? '(handled foo)
        (call/cc (lambda (k)
                   (with-exception-handler
                    (lambda (e)
                      (k (list 'handled e)))
                    (lambda ()
                      (raise 'foo))))))

;; raise-continuable's handler returns normally, and its value becomes
;; the value of the raise-continuable call
(equal? '(back handled foo)
        (with-exception-handler
         (lambda (e)
           (list 'handled e))
         (lambda ()
           (cons 'back (raise-continuable 'foo)))))

;; nested with-exception-handler: raise dispatches to the innermost
;; handler, which itself is called with the outer handler still
;; installed (the outer handler is not active while the inner one runs)
(let* ((results '())
       (add (lambda (x)
              (set! results (cons x results)))))
  (add (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (e)
             (add 'd)
             (k 'handled))
           (lambda ()
             (with-exception-handler
              (lambda (e)
                (add 'c))
              (lambda ()
                (add 'a)
                (raise 'foo)
                (add 'b))))))))
  (equal? '(handled d c a) results))

;; a handler that itself raises dispatches to the next handler out,
;; not back to itself
(let* ((results '())
       (add (lambda (x)
              (set! results (cons x results)))))
  (add (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (e)
             (add 'd)
             (add e)
             (k 'handled))
           (lambda ()
             (with-exception-handler
              (lambda (e)
                (add 'c)
                (add e)
                (raise 'bar))
              (lambda ()
                (add 'a)
                (raise 'foo)
                (add 'b))))))))
  (equal? '(handled bar d foo c a) results))

;; a native runtime error (car of non-pair) reaches an installed
;; with-exception-handler the same way a user raise does
(call/cc (lambda (k)
           (with-exception-handler
            (lambda (e)
              (k #t))
            (lambda ()
              (car '())
              #f))))

;; with-exception-handler's handler is only consulted if the body
;; actually raises
(= 200 (with-exception-handler
        (lambda (e) 100)
        (lambda () 200)))

;; guard: the matching clause's => result becomes guard's value
(= 42
   (guard (condition
           ((assq 'a condition) => cdr)
           ((assq 'b condition)))
          (raise (list (cons 'a 42)))))

(equal? '(b . 23)
        (guard (condition
                ((assq 'a condition) => cdr)
                ((assq 'b condition)))
               (raise (list (cons 'b 23)))))

;; guard: no clause matches, else fires
(equal? 'foo
        (guard (condition
                ((assq 'a condition) => cdr)
                ((assq 'b condition))
                (else 'foo))
               (raise (list (cons 'c 100)))))

;; guard: no clause matches and there is no else, so guard re-raises to
;; the next handler out
(= 200
   (call/cc (lambda (k)
     (with-exception-handler
      (lambda (e) (k (+ 100 (cdar e))))
      (lambda ()
        (guard (condition
                ((assq 'a condition) => cdr)
                ((assq 'b condition)))
               (raise (list (cons 'c 100)))))))))

;; parameters

(let* ((results '())
       (add (lambda (x)
              (set! results (cons x results)))))
  (let ((p1 (make-parameter 10))
        (p2 (make-parameter 20 (lambda (x) (+ x 1)))))
    (define out)
    (call/cc (lambda (k)
               (set! out k)))
    (add (p1))
    (add (p2))
    (parameterize ((p1 100) (p2 200))
      (add (p1))
      (add (p2))
      (when out
        (let ((k out))
          (set! out #f)
          (k #f)))
      (add (p1))
      (add (p2)))
    (add (p1))
    (add (p2)))
  (equal? '(10 21 100 201 10 21 100 201 100 201 10 21)
          (reverse results)))

;; io

;; display/write/write-char/newline default to (current-output-port)
(let ((out (open-output-string)))
  (parameterize ((current-output-port out))
    (display "abc")
    (write '(1 2))
    (write-char #\!)
    (newline))
  (equal? "abc(1 2)!\n" (get-output-string out)))

;; an explicit output port argument bypasses (current-output-port)
(let ((redirected (open-output-string))
      (explicit (open-output-string)))
  (parameterize ((current-output-port redirected))
    (display "x" explicit)
    (write 'y explicit)
    (write-char #\z explicit)
    (newline explicit))
  (and (equal? "" (get-output-string redirected))
       (equal? "xyz\n" (get-output-string explicit))))

;; peek-char/read-char/unread-char/read-line default to
;; (current-input-port)
(let* ((in (open-input-string "ab\ncd\n"))
       (results
        (parameterize ((current-input-port in))
          (let* ((c1 (peek-char))
                 (c2 (read-char)))
            (unread-char c2)
            (let* ((c3 (peek-char))
                   (c4 (read-char))
                   (c5 (read-char))
                   (c6 (read-line))
                   (c7 (read-line))
                   (c8 (eof-object? (read-line))))
              (list c1 c2 c3 c4 c5 c6 c7 c8))))))
  (equal? (list #\a #\a #\a #\a #\b "" "cd" #t) results))

;; an explicit input port argument bypasses (current-input-port)
(let* ((redirected (open-input-string "X"))
       (explicit (open-input-string "Y"))
       (result (parameterize ((current-input-port redirected))
                 (read-char explicit))))
  (equal? #\Y result))

(let* ((port (open-output-string))
       (result (output-port? port)))
  (close-output-port port)
  result)

(let* ((port (open-input-string "foo"))
       (result (input-port? port)))
  (close-input-port port)
  result)
