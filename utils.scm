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
        ((and (vector? x) (vector? y))
         (all? (vector->list (vector-map (lambda (a b)
                                           (equal? a b))
                                         x y))))
        ((not (and (pair? x) (pair? y))) #f)
        (else (and (equal? (car x) (car y))
                   (equal? (cdr x) (cdr y))))))

(define (atom? x)
  (not (pair? x)))

(define (not x)
  (if (eq? x #f) #t #f))

(define (zero? x)
  (eq? x 0))

(define (min . args)
  (if (null? args)
      (error "not enough arguments to min")
      (if (null? (cdr args))
          (car args)
          (let ((min-rest (apply min (cdr args))))
            (if (< (car args) min-rest)
                (car args)
                min-rest)))))

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

(define (make-list . args)
  ;; case-lambda
  (cond ((= (length args) 1)
         (make-list (car args) (void)))
        ((= (length args) 2)
         (let loop ((n (car args)) (ls '()))
           (if (zero? n)
               ls
               (loop (- n 1) (cons (cadr args) ls)))))
        (else (error "invalid number of arguments to make-list"))))

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

(define (assoc . args)
  (cond ((= 2 (length args))
         (apply assoc (append args (list equal?))))
        ((= 3 (length args))
         (let ((obj (car args))
               (alist (cadr args))
               (compare (caddr args)))
           (if (null? alist)
               #f
               (if (compare obj (caar alist))
                   (car alist)
                   (assoc obj (cdr alist) compare)))))
        (else (error "invalid number of arguments to assoc"))))

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

(define (vector-map proc . args)
  ;; let*
  (let ((shortest (apply min (map vector-length args))))
    (let ((result (make-vector shortest)))
      (let loop ((i 0))
        (if (= i shortest)
            result
            (begin
              (vector-set! result i (apply proc (mapcar (lambda (x) (vector-ref x i)) args)))
              (loop (+ i 1))))))))

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

(define (vector-copy . args)
  ;; case-lambda
  (cond ((= (length args) 1)
         (%vector-copy (car args) 0 (vector-length (car args))))
        ((= (length args) 2)
         (%vector-copy (car args) (cadr args) (vector-length (car args))))
        ((= (length args) 3)
         (%vector-copy (car args) (cadr args) (caddr args)))
        (else (error "invalid number of arguments to vector-copy"))))

(define (vector->list vec)
  (let ((result '()))
    (let loop ((i 0)
               (result '()))
      (if (< i (vector-length vec))
          (loop (+ i 1) (cons (vector-ref vec i) result))
          (reverse result)))))

(define (list->vector ls)
  (if (not (list? ls))
      (error "list->vector needs a proper list"))
  (let loop ((i 0) (ls ls) (vec (make-vector (length ls))))
    (if (null? ls)
        vec
        (begin
          (vector-set! vec i (car ls))
          (loop (+ i 1) (cdr ls) vec)))))

(define (%vector-append v1 v2)
  (let ((result (make-vector (+ (vector-length v1) (vector-length v2)))))
    (vector-copy! result 0 v1)
    (vector-copy! result (vector-length v1) v2)))

(define (vector-append . args)
  ;; case-lambda
  (cond ((= (length args) 0)
         #())
        ((= (length args) 1)
         (vector-copy (car args)))
        ((= (length args) 2)
         (%vector-append (car args) (cadr args)))
        (else (%vector-append (%vector-append (car args) (cadr args))
                              (apply vector-append (cddr args))))))

(define (%vector-copy! to at from start end)
  (let ((n (- end start)))
    (let loop ((from-idx start) (to-idx at))
      (if (= from-idx end)
          to
          (begin
            (vector-set! to to-idx (vector-ref from from-idx))
            (loop (+ from-idx 1) (+ to-idx 1)))))))

(define (vector-copy! . args)
  ;; case-lambda
  (cond ((= (length args) 3)
         (let ((to (car args))
               (at (cadr args))
               (from (caddr args)))
           (%vector-copy! to at from 0 (vector-length from))))
        ((= (length args) 4)
         (let ((to (list-ref args 0))
               (at (list-ref args 1))
               (from (list-ref args 2))
               (start (list-ref args 3)))
           (%vector-copy! to at from start (vector-length from))))
        ((= (length args) 5)
         (let ((to (list-ref args 0))
               (at (list-ref args 1))
               (from (list-ref args 2))
               (start (list-ref args 3))
               (end (list-ref args 4)))
           (%vector-copy! to at from start end)))
        (else (error "invalid number of arguments to vector-copy!"))))

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

(define (string->vector . args)
  ;; case-lambda
  (cond ((= 1 (length args))
         (%string->vector (car args) 0 (string-length (car args))))
        ((= 2 (length args))
         (%string->vector (car args) (cadr args) (string-length (car args))))
        ((= 3 (length args))
         (%string->vector (car args) (cadr args) (caddr args)))
        (else (error "invalid number of arguments for string->vector"))))

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

(define (vector->string . args)
  ;; case-lambda
  (cond ((= 1 (length args))
         (%vector->string (car args) 0 (vector-length (car args))))
        ((= 2 (length args))
         (%vector->string (car args) (cadr args) (vector-length (car args))))
        ((= 3 (length args))
         (%vector->string (car args) (cadr args) (caddr args)))
        (else (error "invalid number of arguments for vector->string"))))

(define (%vector-fill! vec fill start end)
  (let loop ((i start))
    (if (= i end)
        vec
        (begin
          (vector-set! vec i fill)
          (loop (+ i 1))))))

(define (vector-fill! . args)
  ;; case-lambda
  (cond ((= 2 (length args))
         (%vector-fill! (car args) (cadr args) 0 (vector-length (car args))))
        ((= 3 (length args))
         (%vector-fill! (car args) (cadr args) (caddr args) (vector-length (car args))))
        ((= 4 (length args))
         (%vector-fill! (car args) (cadr args) (caddr args) (cadddr args)))
        (else (error "invalid number of arguments to vector-fill!"))))

(define (typeof x)
  (cond ((number? x) 'number)
        ((string? x) 'string)
        ((pair? x) 'pair)
        ((vector? x) 'vector)
        ((symbol? x) 'symbol)
        ((procedure? x) 'procedure?)
        ((port? x) 'port)
        ((boolean? x) 'boolean)
        ((char? x) 'char)
        ((eof-object? x) 'eof-object)
        ((error-object? x) 'error-object)
        ((wrapped? x) 'wrapped)
        (else 'other)))

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
                fields)
        record-type-meta-type-id))

(define (record-type? obj)
  (and (wrapped? obj)
       (eq? (wrapped-kind obj) record-type-meta-type-id)))

(define (record-type-type-id record-type-obj)
  (vector-ref (unwrap record-type-obj) 0))

(define (record-type-fields record-type-obj)
  (vector-ref (unwrap record-type-obj) 1))

;; helper function to get the index of a field in the underlying vector, given
;; the field's tag. "start-idx" should be initially passed 0, and "fields"
;; should be the list of field tags for the record type.
(define (record-type-field-idx field-tag start-idx fields)
  (if (eq? field-tag (car fields))
      start-idx
      (record-type-field-idx field-tag (+ start-idx 1) (cdr fields))))

(define (record-constructor record-type tags)
  (unless (record-type? record-type)
    (error "Invalid record type" record-type))
  (let ((type-id (record-type-type-id record-type))
        (fields (record-type-fields record-type)))
    (lambda args
      (unless (= (length tags) (length args))
        (error "Invalid number of arguments for record constructor"
               (length tags)))

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
    (error "Invalid record type" record-type))
  (lambda (obj)
    (and (wrapped? obj)
         (eq? (wrapped-kind obj) (record-type-type-id record-type)))))

(define (record-accessor record-type tag)
  (unless (record-type? record-type)
    (error "Invalid record type" record-type))
  (let ((idx (record-type-field-idx tag 0 (record-type-fields record-type)))
        (type-id (record-type-type-id record-type)))
    (lambda (obj)
      (unless (and (wrapped? obj)
                   (eq? (wrapped-kind obj) type-id))
        (error (format "Invalid value passed to accessor procedure (expecting: ~a)" (record-type-type-id record-type))))
      (vector-ref (unwrap obj) idx))))

(define (record-modifier record-type tag)
  (unless (record-type? record-type)
    (error "Invalid record type" record-type))
  (let ((idx (record-type-field-idx tag 0 (record-type-fields record-type)))
        (type-id (record-type-type-id record-type)))
    (lambda (obj value)
      (unless (and (wrapped? obj)
                   (eq? (wrapped-kind obj) type-id))
        (error (format "Invalid value passed to modifier procedure  (expecting: ~a)" (record-type-type-id record-type))))
      (vector-set! (unwrap obj) idx value))))

(define (record-set-print record-type print-proc)
  (wrapped-set-print (vector-ref (unwrap record-type) 0)
                     print-proc))
