(include "utils.scm")

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
;(not (= 10 (let ((else #f))
;             (cond (else 10)))))
(= 200 (cond ((= 10 20) 50 100)
             ((= 5 5) 80 200)
             ((= 40 40) 300)))
;(= 300 (cond (#f 100)
;             (#f 200)
;             (300)
;             (else 400)))

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
