(define (eqv? x y)
  (eq? x y))

(define (symbol=? x y)
  (eq? x y))

(define (boolean=? x y)
  (eq? x y))

(define (equal? x y)
  (cond ((eq? x y) #t)
        ((eqv? x y) #t)
        ((and (string? x) (string? y) (string=? x y)) #t)
        ((not (and (pair? x) (pair? y))) #f)
        (else (and (equal? (car x) (car y))
                   (equal? (cdr x) (cdr y))))))

(define (atom? x)
  (not (pair? x)))

(define (not x)
  (if (eq? x #f) #t #f))

(define (zero? x)
  (eq? x 0))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (null? x)
  (eq? '() x))

(define (list? v)
  (if (null? v)
      #t
      (if (pair? v)
          (list? (cdr v))
          #f)))

(define (list . x) x)

;; from srfi 1 (named cons* in there)
(define (list* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
        (cons x (recur (car rest) (cdr rest)))
        x)))

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

(define (last ls)
  (list-ref ls (- (length ls) 1)))

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

(define (%append ls1 ls2)
  (if (not (list? ls1))
      (error "append: not a proper list"))

  (if (null? ls1)
      ls2
      (if (null? (cdr ls1))
          (cons (car ls1) ls2)
          (%append (cdr ls1) (cons (car ls1) ls2)))))

(define (append . lists)
  (if (null? lists)
      '()
      (if (null? (cdr lists))
          (car lists)
          (if (null? (cddr lists))
              (%append (reverse (car lists)) (cadr lists))
              (%append (reverse (car lists))
                       (apply append (cdr lists)))))))

(define (any? values)
  (if (null? values)
      #f
      (if (car values)
          #t
          (any? (cdr values)))))

(define (all? values)
  (if (null? values)
      #t
      (if (car values)
          (all? (cdr values))
          #f)))

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

(define (%member obj list compare)
  (if (null? list)
      #f
      (if (compare obj (car list))
          list
          (%member obj (cdr list) compare))))

(define (memq obj ls)
  (%member obj ls eq?))

(define (memv obj ls)
  (%member obj ls eqv?))

;; utility

;; apply the given function to pairs of the given list and return the results as
;; a list.
;;
;; for example, (pairwise list '(1 2 3 4)) would result in (1 2) (2 3) (3 4)
(define (pairwise fn ls)
  (cond ((< (length ls) 2)
         (error "Invalid number of arguments for pairwise"))
        ((eqv? (length ls) 2)
         (list (fn (car ls) (cadr ls))))
        (else
         (cons (fn (car ls) (cadr ls))
               (pairwise fn (cdr ls))))))

(define (char=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise eq? chars))))

(define (char<? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise < (map char->integer chars)))))

(define (char>? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise > (map char->integer chars)))))

(define (char<=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise <= (map char->integer chars)))))

(define (char>=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise >= (map char->integer chars)))))

(define (char-whitespace? c)
  (or (char=? #\space c)
      (char=? #\tab c)
      (char=? #\newline c)
      (char=? #\return c)))

(define (char-alphabetic? ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))))

(define (char-numeric? ch)
  (and (char>=? ch #\0) (char<=? ch #\9)))

(define (char-upper-case? ch)
  (and (char>=? ch #\A)
       (char<=? ch #\Z)))

(define (char-lower-case? ch)
  (and (char>=? ch #\a)
       (char<=? ch #\z)))

(define (positive? n)
  (> n 0))

(define (negative? n)
  (< n 0))

(define (!= m n)
  (not (= m n)))

(define (string-join strs sep)
  (let loop ((result "") (strs strs))
    (if (null? strs)
        result
        (if (string=? "" result)
            (loop (car strs) (cdr strs))
            (loop (string-append result sep (car strs)) (cdr strs))))))

(define (string-append-char str ch)
  (string-append str (make-string 1 ch)))

(define (%string s chars i)
  (if (null? chars)
      s
      (begin
        (string-set! s i (car chars))
        (%string s (cdr chars) (+ i 1)))))

(define (string . chars)
  (let ((s (make-string (length chars))))
    (%string s chars 0)))

(define (print . x)
  (let loop ((x x))
    (if (null? x)
        (void)
        (begin
          (write (car x))
          (display #\space)
          (loop (cdr x)))))
  (newline))

(define (vector . args)
  (let ((vec (make-vector (length args))))
    (let loop ((args args) (i 0))
      (if (pair? args)
          (begin
            (vector-set! vec i (car args))
            (loop (cdr args) (+ i 1)))))
    vec))

(define (list->vector ls)
  (if (not (list? ls))
      (error "list->vector needs a proper list"))
  (let loop ((i 0) (ls ls) (vec (make-vector (length ls))))
    (if (null? ls)
        vec
        (begin
          (vector-set! vec i (car ls))
          (loop (+ i 1) (cdr ls) vec)))))
