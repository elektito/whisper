(define (eqv? x y)
  (eq? x y))

(define (not x)
  (if (eq? x #f) #t #f))

(define (zero? x)
  (eq? x 0))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (caddr x) (car (cdr (cdr x))))

(define (null? x)
  (eq? '() x))

(define (list . x) x)

(define (list-tail ls k)
  (if (zero? k)
      ls
      (list-tail (cdr ls) (- k 1))))

(define (list-ref ls k)
  (car (list-tail ls k)))

(define (length ls)
  (cond ((null? ls) 0)
        ((not (pair? ls))
         (error "length: argument not a list"))
        ((null? (cdr ls))
         1)
        ((not (pair? (cdr ls)))
         (error "length: argument not a proper list"))
        (else (+ 1 (length (cdr ls))))))

(define (list-set! ls k obj)
  (if (zero? k)
      (set-car! ls obj)
      (list-set! (cdr ls) (- k 1) obj)))

(define (list->string x)
  (if (null? x)
      ""
      (string-append (make-string 1 (car x)) (list->string (cdr x)))))

(define (%reverse ls acc)
  (if (null? ls)
      acc
      (%reverse (cdr ls) (cons (car ls) acc))))

(define (reverse ls)
  (%reverse ls '()))

(define (any? values)
  (if (null? values)
      #f
      (if (car values)
          #t
          (any? (cdr values)))))

(define (mapcar func args)
  (if (null? args)
      '()
      (cons (func (car args))
            (mapcar func (cdr args)))))

(define (%map func arg-lists acc)
  (if (any? (mapcar null? arg-lists))
      (reverse acc)
      (%map func
            (mapcar cdr arg-lists)
            (cons (apply func (mapcar car arg-lists)) acc))))

(define (map func . arg-lists)
  (%map func arg-lists '()))

(define (char=? c1 c2)
  (eq? c1 c2))

(define (char-whitespace? c)
  (or (char=? #\space c)
      (char=? #\tab c)
      (char=? #\newline c)
      (char=? #\return c)))

(define (char-numeric? ch)
  (let ((code (char->integer ch))
        (zero (char->integer #\0))
        (nine (char->integer #\9)))
    (and (>= code zero) (<= code nine))))

(define (positive? n)
  (> n 0))

(define (negative? n)
  (< n 0))

(define (string-join strs sep)
  (let loop ((result "") (strs strs))
    (if (null? strs)
        result
        (if (string=? "" result)
            (loop (car strs) (cdr strs))
            (loop (string-append result sep (car strs)) (cdr strs))))))

(define (print . x)
  (let loop ((x x))
    (if (null? x)
        (void)
        (begin
          (write (car x))
          (display #\space)
          (loop (cdr x)))))
  (newline))
