(include "utils.scm")

;; defines

(eq? 1 1 #;(spam eggs)) ; also test datum comment at the end of a list
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
;(= 200 (cond ((= 10 20) 50 100)
;             ((= 5 5) 80 200)
;             ((= 40 40) 300)))
;(= 300 (cond (#f 100)
;             (#f 200)
;             (300)
;             (else 400)))
