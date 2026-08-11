(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x y ...) (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x y ...) (let ((tmp x))
                   (if tmp tmp (or y ...))))))

(define-syntax when
  (syntax-rules ()
    ((_ condition body1 body2 ...)
     (if condition (begin body1 body2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ condition body1 body2 ...)
     (if condition (void) (begin body1 body2 ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 ...) ...)
     (lambda args
       (letrec-syntax
           ;; arity tests, expanded inline as pair?/null? chains so that
           ;; dispatch costs no calls at all (as opposed to calling
           ;; length which we used to do). the parameter list is only
           ;; used as a counter, so the pattern variable p is never
           ;; referenced in the template.
           ((args=? (syntax-rules ::: ()
                      ((_ a ())
                       (null? a))
                      ((_ a (p . rest))
                       (and (pair? a) (args=? (cdr a) rest)))))
            (args>=? (syntax-rules ::: ()
                       ((_ a ())
                        #t)
                       ((_ a (p . rest))
                        (and (pair? a) (args>=? (cdr a) rest)))))
            ;; binds the parameters directly out of the argument list,
            ;; avoiding the closure allocation and variadic re-dispatch
            ;; that going through apply would cost.
            (bind (syntax-rules ::: ()
                    ((_ a () b :::)
                     (begin b :::))
                    ((_ a (p . rest) b :::)
                     (let ((p (car a)))
                       (bind (cdr a) rest b :::)))
                    ((_ a t b :::)
                     (let ((t a)) b :::))))
            (cl (syntax-rules ::: ()
                  ((cl)
                   (error "no matching clause"))
                  ((cl ((p :::) . body) . rest)
                   (if (args=? args (p :::))
                       (bind args (p :::) . body)
                       (cl . rest)))
                  ((cl ((p ::: . tail) . body)
                       . rest)
                   (if (args>=? args (p :::))
                       (bind args (p ::: . tail) . body)
                       (cl . rest))))))
         (cl (params body0 ...) ...))))))

(define-syntax do-step
  (syntax-rules ()
    ((_ x) x)
    ((_ x y) y)))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec
         ((loop
           (lambda (var ...)
             (if test
                 (begin
                   (void)
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (do-step var step ...)
                         ...))))))
       (loop init ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bind"
       (binding ...) () (begin body0 body1 ...)))
    ((let-values "bind" () tmps body)
     (let tmps body))
    ((let-values "bind" ((b0 e0)
                         binding ...) tmps body)
     (let-values "mktmp" b0 e0 ()
                 (binding ...) tmps body))
    ((let-values "mktmp" () e0 args
                 bindings tmps body)
     (call-with-values
         (lambda () e0)
       (lambda args
         (let-values "bind"
           bindings tmps body))))
    ((let-values "mktmp" (a . b) e0 (arg ...)
                 bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x)
                 bindings (tmp ... (a x)) body))
    ((let-values "mktmp" a e0 (arg ...)
                 bindings (tmp ...) body)
     (call-with-values
         (lambda () e0)
       (lambda (arg ... . x)
         (let-values "bind"
           bindings (tmp ... (a x)) body))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body0 body1 ...)
     (let () body0 body1 ...))
    ((let*-values (binding0 binding1 ...)
       body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
         body0 body1 ...)))))

(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values (lambda () expr)
         (lambda args #f))))
    ((define-values (var) expr)
     (define var expr))
    ((define-values (var0 var1 ... varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
           (define varn
             (let ((v (cadr var0)))
               (set! var0 (car var0))
               v))))
    ((define-values (var0 var1 ... . varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
           (define varn
             (let ((v (cdr var0)))
               (set! var0 (car var0))
               v))))
    ((define-values var expr)
     (define var
       (call-with-values (lambda () expr)
         list)))))

(define (symbol=? x y)
  (eq? x y))

(define (boolean=? x y)
  (eq? x y))

(define (equal? x y)
  (cond ((eq? x y) #t)
        ((eqv? x y) #t)
        ((and (string? x) (string? y) (string=? x y)) #t)
        ((and (vector? x) (vector? y))
         (all? (vector->list (vector-map (lambda (a b)
                                           (equal? a b))
                                         x y))))
        ((not (and (pair? x) (pair? y))) #f)
        (else (and (equal? (car x) (car y))
                   (equal? (cdr x) (cdr y))))))

(define (atom? x)
  (not (pair? x)))

(define (zero? x)
  (eq? x 0))

(define (integer? n)
  ;; TODO we need to change this if/when we have other kinds of numbers
  (number? n))

(define (positive? n)
  (> n 0))

(define (negative? n)
  (< n 0))

(define (even? n)
  (unless (integer? n)
    (error "not an integer"))
  (zero? (floor-remainder n 2)))

(define (odd? n)
  (unless (integer? n)
    (error "not an integer"))
  (not (zero? (floor-remainder n 2))))

(define (!= m n)
  (not (= m n)))

(define (expt base power)
  (cond ((< power 0) (error "expt: negative exponent not supported"))
        ((= power 0) 1)
        (else
         (let loop ((b base) (p power) (acc 1))
           (cond ((= p 0) acc)
                 ((even? p) (loop (* b b) (truncate-quotient p 2) acc))
                 (else (loop (* b b) (truncate-quotient p 2) (* acc b))))))))

(define (abs x)
  (if (< x 0) (- x) x))

(define (gcd2 a b)
  (let loop ((x (abs a)) (y (abs b)))
    (if (= y 0)
        x
        (loop y (truncate-remainder x y)))))

(define (lcm2 a b)
  (if (or (= a 0) (= b 0))
      0
      (* (truncate-quotient (abs a) (gcd2 a b)) (abs b))))

(define (gcd . args)
  (if (null? args)
      0
      (let loop ((rest (cdr args)) (acc (car args)))
        (if (null? rest)
            (abs acc)
            (loop (cdr rest) (gcd2 acc (car rest)))))))

(define (lcm . args)
  (if (null? args)
      1
      (let loop ((rest (cdr args)) (acc (car args)))
        (if (null? rest)
            (abs acc)
            (loop (cdr rest) (lcm2 acc (car rest)))))))

(define min
  (case-lambda
   ((x) x)
   ((x y) (if (< x y) x y))
   ((x y . rest) (apply min (min x y) rest))))

(define max
  (case-lambda
   ((x) x)
   ((x y) (if (> x y) x y))
   ((x y . rest) (max x (apply max y rest)))))

(define (square z)
  (* z z))

(define (truncate-quotient m n)
  (/ m n))

(define (truncate-remainder m n)
  (- m (* n (truncate-quotient m n))))

(define (truncate/ m n)
  (values (truncate-quotient m n)
          (truncate-remainder m n)))

(define (floor-quotient m n)
  (let ((q (truncate-quotient m n))
        (r (truncate-remainder m n)))
    (if (and (not (zero? r))
             (not (eqv? (negative? m) (negative? n))))
        (- q 1)
        q)))

(define (floor-remainder n d)
  (let ((r (truncate-remainder n d)))
    (if (and (not (zero? r))
             (not (eqv? (negative? n) (negative? d))))
        (+ r d)
        r)))

(define (floor/ m n)
  (values (floor-quotient m n)
          (floor-remainder m n)))

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

(define (list? v)
  (if (null? v)
      #t
      (if (pair? v)
          (list? (cdr v))
          #f)))

(define (list-tail ls k)
  (if (zero? k)
      ls
      (list-tail (cdr ls) (- k 1))))

(define (list-ref ls k)
  (car (list-tail ls k)))

(define (list-copy lis)
  (let recur ((lis lis))
    (if (pair? lis)
        (cons (car lis) (recur (cdr lis)))
        lis)))

(define (list-set! ls k obj)
  (if (zero? k)
      (set-car! ls obj)
      (list-set! (cdr ls) (- k 1) obj)))

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

(define (list->string ls)
  (apply string ls))

(define (%reverse ls acc)
  (if (null? ls)
      acc
      (%reverse (cdr ls) (cons (car ls) acc))))

(define (reverse ls)
  (%reverse ls '()))

(define make-list
  (case-lambda
   ((n) (make-list n (void)))
   ((n x) (let loop ((n n) (ls '()))
            (if (zero? n)
                ls
                (loop (- n 1) (cons x ls)))))))

(define iota
  (case-lambda
   ((count) (iota count 0 1))
   ((count start) (iota count start 1))
   ((count start step)
    (let loop ((i (- count 1))
               (val (+ start (* (- count 1) step)))
               (ls '()))
      (if (< i 0)
          ls
          (loop (- i 1) (- val step) (cons val ls)))))))

(define (range start end)
  (iota (- end start) start))

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

;; single-list map. kept separate from %map because the one-list case is
;; overwhelmingly the common one, and %map costs three mapcar traversals
;; plus an any? per element to handle the n-ary case.
(define (%map1 func ls acc)
  (if (null? ls)
      (reverse acc)
      (%map1 func (cdr ls) (cons (func (car ls)) acc))))

(define (map func . arg-lists)
  (cond ((null? arg-lists) '())
        ((null? (cdr arg-lists)) (%map1 func (car arg-lists) '()))
        (else (%map func arg-lists '()))))

(define (%for-each1 proc ls)
  (unless (null? ls)
    (proc (car ls))
    (%for-each1 proc (cdr ls))))

(define (for-each proc . arg-lists)
  (cond ((null? arg-lists) (void))
        ((null? (cdr arg-lists)) (%for-each1 proc (car arg-lists)))
        (else
         (let loop ((arg-lists arg-lists))
           (unless (any? (mapcar null? arg-lists))
             (apply proc (mapcar car arg-lists))
             (loop (mapcar cdr arg-lists)))))))

(define (filter pred ls)
  (let loop ((ls ls) (acc '()))
    (cond ((null? ls) (reverse acc))
          ((pred (car ls)) (loop (cdr ls) (cons (car ls) acc)))
          (else (loop (cdr ls) acc)))))

(define (%member obj list compare)
  (if (null? list)
      #f
      (if (compare obj (car list))
          list
          (%member obj (cdr list) compare))))

(define member
  (case-lambda
   ((obj list) (%member obj list equal?))
   ((obj list compare) (%member obj list compare))))

(define (memq obj ls)
  (%member obj ls eq?))

(define (memv obj ls)
  (%member obj ls eqv?))

(define assoc
  (case-lambda
   ((obj alist) (assoc obj alist equal?))
   ((obj alist compare)
    (if (null? alist)
        #f
        (if (compare obj (caar alist))
            (car alist)
            (assoc obj (cdr alist) compare))))))

(define (assq obj alist)
  (assoc obj alist eq?))

(define (assv obj alist)
  (assoc obj alist eqv?))

;; utility

;; apply the given function to pairs of the given list and return the results as
;; a list.
;;
;; for example, (pairwise list '(1 2 3 4)) would result in (1 2) (2 3) (3 4)
(define (pairwise fn ls)
  (cond ((or (null? ls) (null? (cdr ls)))
         (error "Invalid number of arguments for pairwise"))
        ((null? (cddr ls))
         (list (fn (car ls) (cadr ls))))
        (else
         (cons (fn (car ls) (cadr ls))
               (pairwise fn (cdr ls))))))

(define (sort lst less?)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((less? (car b) (car a))
           (cons (car b) (merge a (cdr b))))
          (else
           (cons (car a) (merge (cdr a) b)))))
  ;; split (1 2 3 4 5) into p1=(1 3 5) and p2=(2 4), returned as a pair
  ;; (p1 . p2)
  (define (split lst)
    (if (or (null? lst) (null? (cdr lst)))
        (cons lst '())
        (let ((rest (split (cddr lst))))
          (cons (cons (car lst) (car rest))
                (cons (cadr lst) (cdr rest))))))
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((halves (split lst)))
        (merge (sort (car halves) less?) (sort (cdr halves) less?)))))

(define (char=? . chars)
  (cond ((null? (cdr chars)) #t)
        ((null? (cddr chars)) (eq? (car chars) (cadr chars)))
        (else (all? (pairwise eq? chars)))))

(define (char<? . chars)
  (cond ((null? (cdr chars)) #t)
        ((null? (cddr chars))
         (< (char->integer (car chars)) (char->integer (cadr chars))))
        (else (all? (pairwise < (map char->integer chars))))))

(define (char>? . chars)
  (cond ((null? (cdr chars)) #t)
        ((null? (cddr chars))
         (> (char->integer (car chars)) (char->integer (cadr chars))))
        (else (all? (pairwise > (map char->integer chars))))))

(define (char<=? . chars)
  (cond ((null? (cdr chars)) #t)
        ((null? (cddr chars))
         (<= (char->integer (car chars)) (char->integer (cadr chars))))
        (else (all? (pairwise <= (map char->integer chars))))))

(define (char>=? . chars)
  (cond ((null? (cdr chars)) #t)
        ((null? (cddr chars))
         (>= (char->integer (car chars)) (char->integer (cadr chars))))
        (else (all? (pairwise >= (map char->integer chars))))))

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

(define (char-foldcase ch)
  (char-downcase ch))

(define (char-ci=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char=? (map char-foldcase chars)))))

(define (char-ci<? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char<? (map char-foldcase chars)))))

(define (char-ci>? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char>? (map char-foldcase chars)))))

(define (char-ci<=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char<=? (map char-foldcase chars)))))

(define (char-ci>=? . chars)
  (if (null? (cdr chars))
      #t
      (all? (pairwise char>=? (map char-foldcase chars)))))

(define (string-join strs sep)
  (let loop ((result #f) (strs strs))
    (if (null? strs)
        (or result "")
        (if (not result)
            (loop (car strs) (cdr strs))
            (loop (string-append result sep (car strs)) (cdr strs))))))

(define (string-split str sep)
  (if (string=? str "")
      '()
      (let ((len (string-length str)))
        (let loop ((start 0) (i 0) (acc '()))
          (cond ((= i len)
                 (reverse (cons (substring str start i) acc)))
                ((char=? (string-ref str i) sep)
                 (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
                (else
                 (loop start (+ i 1) acc)))))))

(define (string-suffix? suffix str)
  (let ((suffix-len (string-length suffix))
        (str-len (string-length str)))
    (and (>= str-len suffix-len)
         (string=? suffix (substring str (- str-len suffix-len) str-len)))))

(define (%string<? a b)
  (let ((len-a (string-length a)) (len-b (string-length b)))
    (let loop ((i 0))
      (cond ((= i len-a) (< len-a len-b))
            ((= i len-b) #f)
            ((char<? (string-ref a i) (string-ref b i)) #t)
            ((char<? (string-ref b i) (string-ref a i)) #f)
            (else (loop (+ i 1)))))))

(define (string<? . strings)
  (cond ((null? strings) (error "string<?: requires at least one argument"))
        ((null? (cdr strings)) #t)
        ((null? (cddr strings)) (%string<? (car strings) (cadr strings)))
        (else (all? (pairwise %string<? strings)))))

(define (string>? . strings)
  (cond ((null? strings) (error "string>?: requires at least one argument"))
        ((null? (cdr strings)) #t)
        ((null? (cddr strings)) (%string<? (cadr strings) (car strings)))
        (else (all? (pairwise (lambda (a b) (%string<? b a)) strings)))))

(define (string<=? . strings)
  (cond ((null? strings) (error "string<=?: requires at least one argument"))
        ((null? (cdr strings)) #t)
        ((null? (cddr strings)) (not (%string<? (cadr strings) (car strings))))
        (else (all? (pairwise (lambda (a b) (not (%string<? b a))) strings)))))

(define (string>=? . strings)
  (cond ((null? strings) (error "string>=?: requires at least one argument"))
        ((null? (cdr strings)) #t)
        ((null? (cddr strings)) (not (%string<? (car strings) (cadr strings))))
        (else (all? (pairwise (lambda (a b) (not (%string<? a b))) strings)))))

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

(define string->list
  (case-lambda
   ((s) (string->list s 0 (string-length s)))
   ((s start) (string->list s start (string-length s)))
   ((s start end) (map (lambda (n) (string-ref s n))
                       (range start end)))))

(define (string-map proc . args)
  (let* ((shortest (apply min (map string-length args)))
         (result (make-string shortest)))
    (do ((i 0 (+ i 1)))
        ((= i shortest) result)
      (string-set! result i (apply proc (mapcar (lambda (x) (string-ref x i)) args))))))

(define (string-for-each proc . args)
  (let ((shortest (apply min (map string-length args))))
    (do ((i 0 (+ i 1)))
        ((= i shortest) (void))
      (apply proc (mapcar (lambda (x) (string-ref x i)) args)))))

(define (string-downcase s)
  (string-map char-downcase s))

(define (string-foldcase s)
  (string-map char-foldcase s))

(define (string-upcase s)
  (string-map char-upcase s))

(define (string-ci=? s1 s2 . rest)
  (apply string=?
         (string-foldcase s1)
         (string-foldcase s2)
         (map string-foldcase rest)))

(define (string-ci<? s1 s2 . rest)
  (apply string<?
         (string-foldcase s1)
         (string-foldcase s2)
         (map string-foldcase rest)))

(define (string-ci<=? s1 s2 . rest)
  (apply string<=?
         (string-foldcase s1)
         (string-foldcase s2)
         (map string-foldcase rest)))

(define (string-ci>? s1 s2 . rest)
  (apply string>?
         (string-foldcase s1)
         (string-foldcase s2)
         (map string-foldcase rest)))

(define (string-ci>=? s1 s2 . rest)
  (apply string>=?
         (string-foldcase s1)
         (string-foldcase s2)
         (map string-foldcase rest)))

(define string-fill!
  (case-lambda
   ((str fill) (string-fill! str fill 0 (string-length str)))
   ((str fill start) (string-fill! str fill start (string-length str)))
   ((str fill start end) (do ((i start (+ i 1)))
                             ((= i end) str)
                           (string-set! str i fill)))))

(define string-copy
  (case-lambda
   ((str) (substring str 0 (string-length str)))
   ((str start) (substring str start (string-length str)))
   ((str start end) (substring str start end))))

(define string-copy!
  (case-lambda
   ((to at from) (string-copy! to at from 0 (string-length from)))
   ((to at from start) (string-copy! to at from start (string-length from)))
   ((to at from start end) (let ((n (- end start)))
                             (do ((from-idx start (+ from-idx 1))
                                  (to-idx at (+ to-idx 1)))
                                 ((= from-idx end) to)
                               (string-set! to to-idx (string-ref from from-idx)))))))

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

(define (vector-map proc . args)
  (let* ((shortest (apply min (map vector-length args)))
         (result (make-vector shortest)))
    (let loop ((i 0))
      (if (= i shortest)
          result
          (begin
            (vector-set! result i (apply proc (mapcar (lambda (x) (vector-ref x i)) args)))
            (loop (+ i 1)))))))

(define (vector-for-each proc . args)
  (let* ((shortest (apply min (map vector-length args))))
    (do ((i 0 (+ i 1)))
        ((= i shortest) (void))
      (apply proc (mapcar (lambda (x) (vector-ref x i)) args)))))

(define (%vector-copy vector start end)
  (let ((n (- end start)))
    (let loop ((r (make-vector n))
               (vidx start)
               (ridx 0))
      (if (= ridx n)
          r
          (begin
            (vector-set! r ridx (vector-ref vector vidx))
            (loop r (+ 1 vidx) (+ 1 ridx)))))))

(define vector-copy
  (case-lambda
   ((v) (%vector-copy v 0 (vector-length v)))
   ((v start) (%vector-copy v start (vector-length v)))
   ((v start end) (%vector-copy v start end))))

(define (vector->list vec)
  (let ((result '()))
    (let loop ((i 0)
               (result '()))
      (if (< i (vector-length vec))
          (loop (+ i 1) (cons (vector-ref vec i) result))
          (reverse result)))))

(define (%vector-append v1 v2)
  (let ((result (make-vector (+ (vector-length v1) (vector-length v2)))))
    (vector-copy! result 0 v1)
    (vector-copy! result (vector-length v1) v2)))

(define vector-append
  (case-lambda
   (() #())
   ((v) (vector-copy v))
   ((v1 v2) (%vector-append v1 v2))
   ((v1 v2 . rest) (%vector-append (%vector-append v1 v2)
                                   (apply vector-append rest)))))

(define (%vector-copy! to at from start end)
  (let ((n (- end start)))
    (let loop ((from-idx start) (to-idx at))
      (if (= from-idx end)
          to
          (begin
            (vector-set! to to-idx (vector-ref from from-idx))
            (loop (+ from-idx 1) (+ to-idx 1)))))))

(define vector-copy!
  (case-lambda
   ((to at from) (%vector-copy! to at from 0 (vector-length from)))
   ((to at from start) (%vector-copy! to at from start (vector-length from)))
   ((to at from start end) (%vector-copy! to at from start end))))

(define (%string->vector str start end)
  (let ((n (- end start)))
    (let loop ((s (make-vector n))
               (vidx 0)
               (sidx start))
      (if (= vidx n)
          s
          (begin
            (vector-set! s vidx (string-ref str sidx))
            (loop s (+ vidx 1) (+ sidx 1)))))))

(define string->vector
  (case-lambda
   ((str) (%string->vector str 0 (string-length str)))
   ((str start) (%string->vector str start (string-length str)))
   ((str start end) (%string->vector str start end))))

(define (%vector->string vector start end)
  (let ((n (- end start)))
    (let loop ((s (make-string n))
               (vidx start)
               (sidx 0))
      (if (= sidx n)
          s
          (begin
            (string-set! s sidx (vector-ref vector vidx))
            (loop s (+ vidx 1) (+ sidx 1)))))))

(define vector->string
  (case-lambda
   ((v) (%vector->string v 0 (vector-length v)))
   ((v start) (%vector->string v start (vector-length v)))
   ((v start end) (%vector->string v start end))))

(define vector-fill!
  (case-lambda
   ((v fill) (vector-fill! v fill 0 (vector-length v)))
   ((v fill start) (vector-fill! v fill start (vector-length v)))
   ((v fill start end) (let loop ((i start))
                         (if (= i end)
                             v
                             (begin
                               (vector-set! v i fill)
                               (loop (+ i 1))))))))

;; record types

;; adapted from SRFI 9
;; https://srfi.schemers.org/srfi-9/srfi-9.html
;; see NOTICES file
(define-syntax define-record-type
  (syntax-rules ()
    ((_ type
        (constructor constructor-tag ...)
        predicate
        (field-tag accessor . more) ...)
     (begin
       (define type
         (make-record-type 'type '(field-tag ...)))
       (define constructor
         (record-constructor type '(constructor-tag ...)))
       (define predicate
         (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ...))))

(define-syntax define-record-field
  (syntax-rules ()
    ((_ type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((_ type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))

;; When we encounter a (define-record-type foo ...) form, we first create a
;; record-type object for it. This is bound to the type name itself ("foo" in
;; this example).
;;
;; The type of this "record type object" is itself a wrapped vector, very
;; similar to the records themselves. Its unique id is a gensym stored in
;; record-type-meta-type-id and is used to check if a given object is a record
;; type itself (not to be confused with the record object itself).
;;
;; This "meta type" has its own predicate, constructor and accessors. For
;; example, for a foo record type, (record-type? foo) is true. But if you create
;; a record of type foo using its constructor (record-type? foo-obj) is not
;; true.

;; a unique identifier used to identify the record type "type".
;; this means the record type itself is a unique type
(define record-type-meta-type-id (gensym "record-type"))

(define (make-record-type name fields)
  ;; a record "type" is a manually constructed wrapped vector itself.
  (wrap (vector (gensym (symbol->string name))
                fields
                name)
        record-type-meta-type-id))

(define (record-type? obj)
  (and (wrapped? obj)
       (eq? (wrapped-kind obj) record-type-meta-type-id)))

(define (record-type-type-id record-type-obj)
  (vector-ref (unwrap record-type-obj) 0))

(define (record-type-fields record-type-obj)
  (vector-ref (unwrap record-type-obj) 1))

(define (record-type-name record-type-obj)
  (vector-ref (unwrap record-type-obj) 2))

;; helper function to get the index of a field in the underlying vector, given
;; the field's tag. "start-idx" should be initially passed 0, and "fields"
;; should be the list of field tags for the record type.
(define (record-type-field-idx field-tag start-idx fields)
  (if (eq? field-tag (car fields))
      start-idx
      (record-type-field-idx field-tag (+ start-idx 1) (cdr fields))))

(define (record-constructor record-type tags)
  (unless (record-type? record-type)
    (error (format "record-constructor: not a record type: ~s" record-type)))
  (let ((type-id (record-type-type-id record-type))
        (fields (record-type-fields record-type)))
    (lambda args
      (unless (= (length tags) (length args))
        (error (format "~s constructor: expected ~a arguments, got ~a"
                       (record-type-name record-type) (length tags) (length args))))
      (let loop ((vec (make-vector (length fields)))
                 (tags tags)
                 (args args))
        (if (null? tags)
            (wrap vec type-id)
            (begin
              (vector-set! vec
                           (record-type-field-idx (car tags) 0 fields)
                           (car args))
              (loop vec (cdr tags) (cdr args))))))))

(define (record-predicate record-type)
  (unless (record-type? record-type)
    (error (format "record-predicate: not a record type: ~s" record-type)))
  (lambda (obj)
    (and (wrapped? obj)
         (eq? (wrapped-kind obj) (record-type-type-id record-type)))))

(define (record-accessor record-type tag)
  (unless (record-type? record-type)
    (error (format "record-accessor: not a record type: ~s" record-type)))
  (let ((idx (record-type-field-idx tag 0 (record-type-fields record-type)))
        (type-id (record-type-type-id record-type)))
    (lambda (obj)
      (unless (and (wrapped? obj)
                   (eq? (wrapped-kind obj) type-id))
        (error (format "~s accessor: expected ~s, got ~s"
                       tag (record-type-name record-type) obj)))
      (vector-ref (unwrap obj) idx))))

(define (record-modifier record-type tag)
  (unless (record-type? record-type)
    (error (format "record-modifier: not a record type: ~s" record-type)))
  (let ((idx (record-type-field-idx tag 0 (record-type-fields record-type)))
        (type-id (record-type-type-id record-type)))
    (lambda (obj value)
      (unless (and (wrapped? obj)
                   (eq? (wrapped-kind obj) type-id))
        (error (format "~s modifier: expected ~s, got ~s"
                       tag (record-type-name record-type) obj)))
      (vector-set! (unwrap obj) idx value))))

(define (record-set-print record-type print-proc)
  (wrapped-set-print (vector-ref (unwrap record-type) 0)
                     print-proc))

(define (make-eq-hash-table)
  (%make-hash-table #f #f))

(define make-hash-table
  (case-lambda
   (() (%make-hash-table #f #f))
   ((eq-fn) (%make-hash-table eq-fn #f))
   ((eq-fn hash-fn) (%make-hash-table eq-fn hash-fn))))

(define alist->hash-table
  (case-lambda
   ((alist) (alist->hash-table alist #f #f))
   ((alist eq-fn) (alist->hash-table alist eq-fn #f))
   ((alist eq-fn hash-fn)
    (let ((ht (%make-hash-table eq-fn hash-fn)))
      (let loop ((alist alist))
        (unless (null? alist)
          (hash-table-set! ht (caar alist) (cdar alist))
          (loop (cdr alist))))
      ht))))

(define (gc-stats)
  (let ((stats (%gc-stats)))
    (list (cons 'marks (vector-ref stats 0))
          (cons 'full-sweeps (vector-ref stats 1))
          (cons 'lazy-reclaims (vector-ref stats 2))
          (cons 'live-objects (vector-ref stats 3))
          (cons 'manual-mode (vector-ref stats 4))
          (cons 'pools (vector-ref stats 5)))))

;; the following implementation of call/cc and dynamic-wind is based on:
;; https://www.scheme.com/tspl4/control.html#./control:s56

(define winders '())

(define (common-tail x y)
  (let ((lx (length x))
        (ly (length y)))
    (do ((x (if (> lx ly) (list-tail x (- lx ly)) x)
            (cdr x))
         (y (if (> ly lx) (list-tail y (- ly lx)) y)
            (cdr y)))
        ((eq? x y) x))))

(define (do-wind new)
  (let ((tail (common-tail new winders)))
    ;; we'll loop on winders, starting from the first element and moving forward
    ;; until we reach the common tail.
    (let f ((ls winders))
      (unless (eq? ls tail)
        (set! winders (cdr ls))
        ((cdar ls)) ; call out-guard
        (f (cdr ls))))
    ;; now we'll loop over the new list of winders, starting from right before
    ;; the last non-common element, moving backwards to the first element,
    ;; calling in-guards.
    (let f ((ls new))
      (unless (eq? ls tail)
        (f (cdr ls))
        ((caar ls)) ; call in-guard
        (set! winders ls)))))

(define (call/cc f)
  (%call/cc
   (lambda (k)
     (f (let ((save winders))
          (lambda x
            (unless (eq? save winders)
              (do-wind save))
            (apply k x)))))))

(define (call-with-current-continuation f)
  (call/cc f))

(define (dynamic-wind in body out)
  (in)
  (set! winders (cons (cons in out) winders))
  (let-values ((ans* (body)))
    (set! winders (cdr winders))
    (out)
    (apply values ans*)))

;; exceptions

(define exception-handlers '())

(define-record-type <error>
  (make-error message irritants kind)
  error-object?
  (message error-object-message)
  (irritants error-object-irritants)
  (kind error-object-kind))

(record-set-print
 <error>
 (lambda (obj port)
   (display "#<" port)
   (unless (eq? (error-object-kind obj) 'normal)
     (write (error-object-kind obj) port)
     (display "-" port))
   (display "error " port)
   (write (error-object-message obj) port)
   (for-each (lambda (irritant)
               (display " " port)
               (write irritant port))
             (error-object-irritants obj))
   (display ">" port)))

(define (error msg . irritants)
  (raise (make-error msg irritants 'normal)))

(define (terminate-with-exception e)
  (if (error-object? e)
      (abort (error-object-message e))
      (abort (format "~s" e))))

(define (raise e)
  (when (null? exception-handlers)
    (terminate-with-exception e))

  (let ((old-handlers exception-handlers)
        (cur-handler #f))
    (dynamic-wind
        (lambda ()
          (set! old-handlers exception-handlers)
          (set! cur-handler (car exception-handlers))
          (set! exception-handlers (cdr exception-handlers)))
        (lambda ()
          (cur-handler e)
          (raise (error "an exception handler returned" 'wrapped-exception e)))
        (lambda ()
          (set! exception-handlers old-handlers)))))

(define (raise-continuable e)
  (when (null? exception-handlers)
    (raise
     (error "a continuable exception happened, but there was no error handler"
            'wrapped-exception e)))

  (let ((old-handlers exception-handlers)
        (cur-handler #f))
    (dynamic-wind
        (lambda ()
          (set! old-handlers exception-handlers)
          (set! cur-handler (car exception-handlers))
          (set! exception-handlers (cdr exception-handlers)))
        (lambda ()
          (cur-handler e))
        (lambda ()
          (set! exception-handlers old-handlers)))))

(define (with-exception-handler handler thunk)
  (dynamic-wind
      (lambda ()
        (set! exception-handlers
              (cons handler exception-handlers)))
      (lambda () (thunk))
      (lambda ()
        (set! exception-handlers (cdr exception-handlers)))))

(define (file-error? obj)
  (and (error-object? obj)
       (eq? 'file (error-object-kind obj))))

(define (file-error msg . irritants)
  (raise (make-error msg irritants 'file)))

(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
       (lambda (guard-k)
         (with-exception-handler
          (lambda (condition)
            ((call/cc
              (lambda (handler-k)
                (guard-k
                 (lambda ()
                   (let ((var condition))
                     (guard-aux
                      (handler-k
                       (lambda ()
                         (raise-continuable condition)))
                      clause ...))))))))
          (lambda ()
            (call-with-values
                (lambda () e1 e2 ...)
              (lambda args
                (guard-k
                 (lambda ()
                   (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))

;; TODO change this to a simple lambda with two arguments after a
;; version bump. the case-lambda is for while we transition to the new
;; two-argument system.
(set-system-exception-handler
 (case-lambda
  ((msg) (raise (make-error msg '() 'system)))
  ((kind msg) (raise (make-error msg '() kind)))))

;; parameters

(define make-parameter
  (case-lambda
   ((init) (make-parameter init (lambda (x) x)))
   ((init converter) (let ((value (converter init)))
                       (case-lambda
                        (() value)
                        ((new-value) (set! value (converter new-value)))
                        ((new-value dont-convert) (if dont-convert
                                                      (set! value new-value)
                                                      (set! value (converter new-value)))))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
       ((param value p old new) ...)
       ()
       body)
     (let ((p param) ...)
       (let ((old (p)) ...
             (new value) ...)
         (dynamic-wind
             (lambda () (p new) ...)
             (lambda () . body)
             (lambda () (p old #t) ...)))))
    ((parameterize ("step")
       args
       ((param value) . rest)
       body)
     (parameterize ("step")
       ((param value p old new) . args)
       rest
       body))
    ((parameterize ((param value) ...) . body)
     (parameterize ("step")
       ()
       ((param value) ...)
       body))))

;; io

(define current-input-port (make-parameter (stdin)))
(define current-output-port (make-parameter (stdout)))
(define current-error-port (make-parameter (stderr)))

(define display
  (case-lambda
   ((obj) (%display obj (current-output-port)))
   ((obj port) (%display obj port))))

(define newline
  (case-lambda
   (() (%newline (current-output-port)))
   ((port) (%newline port))))

(define peek-char
  (case-lambda
   (() (%peek-char (current-input-port)))
   ((port) (%peek-char port))))

(define read-char
  (case-lambda
   (() (%read-char (current-input-port)))
   ((port) (%read-char port))))

(define read-line
  (case-lambda
   (() (%read-line (current-input-port)))
   ((port) (%read-line port))))

(define unread-char
  (case-lambda
   ((ch) (%unread-char ch (current-input-port)))
   ((ch port) (%unread-char ch port))))

(define write
  (case-lambda
   ((obj) (%write obj (current-output-port)))
   ((obj port) (%write obj port))))

(define write-char
  (case-lambda
   ((ch) (%write-char ch (current-output-port)))
   ((ch port) (%write-char ch port))))

(define write-string
  (case-lambda
   ((str) (write-string str (current-output-port) 0 (string-length str)))
   ((str port) (write-string str port 0 (string-length str)))
   ((str port start) (write-string str port start (string-length str)))
   ((str port start end) (%display (substring str start end) port))))

(define (call-with-port port proc)
  (let ((result (proc port)))
    (close-port port)
    result))

(define (call-with-input-file filename proc)
  (let ((port (open-input-file filename)))
    (call-with-port port proc)))

(define (call-with-output-file filename proc)
  (let ((port (open-output-file filename)))
    (call-with-port port proc)))

(define (with-input-from-file filename thunk)
  (let ((port (open-input-file filename)))
    (parameterize ((current-input-port port))
      (thunk)
      (close-input-port port))))

(define (with-output-to-file filename thunk)
  (let ((port (open-output-file filename)))
    (parameterize ((current-output-port port))
      (thunk)
      (close-output-port port))))

(define (close-input-port port)
  (unless (input-port? port)
    (raise (file-error "Not an input port")))
  (close-port port))

(define (close-output-port port)
  (unless (output-port? port)
    (raise (file-error "Not an output port")))
  (close-port port))
