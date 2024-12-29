(define *qq-list* (gensym "QQ-LIST"))
(define *qq-append* (gensym "QQ-APPEND"))
(define *qq-list-star* (gensym "QQ-LIST*"))
(define *qq-quote* (gensym "QQ-QUOTE"))
(define *qq-list->vector* (gensym "QQ-LIST->VECTOR"))

(define *qq-simplify* #t)

(define (qq-quasiquote form)
  (unless (and (list? form)
               (= (length form) 2)
               (identifier-is-special (car form) 'quasiquote))
    (error "bad quasiquote form"))
  (let ((result (qq-process (cadr form) 1)))
    (let ((result (if *qq-simplify* (qq-simplify result) result)))
      (qq-remove-tokens result))))

(define (qq-process form level)
  (cond ((vector? form) (list *qq-list->vector*
                              (qq-process-list (vector->list form) level)))
        ((atom? form) (if (= level 1)
                          (list *qq-quote* form)
                          form))
        ((qq-is-unquote form) (if (= level 1)
                                  (cadr form)
                                  (qq-process-list form (- level 1))))
        ((qq-is-unquote-splicing form) (error "unquote-splicing immediately inside quasiquote"))
        ((qq-is-quasiquote form) (qq-process-list form (+ level 1)))
        (else (qq-process-list form level))))

(define (qq-process-list form level)
  (let ((rest-n-tail (qq-split-improper-tail form)))
    (let ((rest (car rest-n-tail))
          (tail (cadr rest-n-tail)))
      (let ((append-form (list* *qq-append*
                                (map (lambda (x)
                                       (qq-process-list-item x level))
                                     rest))))
        (if (null? tail)
            append-form
            (append append-form (cons (qq-process-list-tail tail level) '())))))))

(define (qq-process-list-item form level)
  (cond ((vector? form) (list *qq-list*
                              (list *qq-list->vector*
                                    (qq-process-list (vector->list form) level))))
        ((atom? form) (list *qq-quote* (list form)))
        ((qq-is-unquote form) (if (= level 1)
                                  (list *qq-list* (cadr form))
                                  (list *qq-list* (qq-process-list form (- level 1)))))
        ((qq-is-unquote-splicing form) (if (= level 1)
                                           (cadr form)
                                           (list *qq-list* (qq-process-list form (- level 1)))))
        ((qq-is-quasiquote form) (list *qq-list* (qq-process-list form (+ level 1))))
        (else (list *qq-list* (qq-process-list form level)))))

(define (qq-process-list-tail form level)
  (cond ((vector? form) (list *qq-list->vector*
                              (qq-process-list (vector->list form) level)))
        ((atom? form) (list *qq-quote* form))
        ((qq-is-unquote form) (if (= level 1)
                                  (cadr form)
                                  (qq-process-list form (- level 1))))
        ((qq-is-unquote-splicing form) (error "unquote-splicing in dotted tail"))
        ((qq-is-quasiquote form) (qq-process-list form (+ level 1)))
        (else (qq-process-list form level))))

(define (qq-split-improper-tail ls)
  ;; () => () -- ()
  ;; (a) => (a) -- ()
  ;; (a b) => (a b) -- ()
  ;; (a . b) => (a) -- b
  ;; (a . ,b) => (a) -- ,b
  ;; (a b . ,c) => (a b) -- ,c
  ;; (a b . c) => (a b) -- c
  ;; (a . `c) => a -- `c
  ;; (a b . `c) => (a b) -- `c
  (cond ((null? ls) (list '() '()))
        ((atom? (cdr ls)) (list (list (car ls))
                                (cdr ls)))
        ((qq-is-unquote (cdr ls)) (list (list (car ls))
                                        (cdr ls)))
        ((qq-is-unquote-splicing (cdr ls)) (list (list (car ls))
                                                 (cdr ls)))
        ((qq-is-quasiquote (cdr ls)) (list (list (car ls))
                                           (cdr ls)))
        (else (let ((rest-n-tail (qq-split-improper-tail (cdr ls))))
                (list (cons (car ls) (car rest-n-tail)) (cadr rest-n-tail))))))

(define (qq-is-unquote form)
  (and (list? form)
       (= (length form) 2)
       (identifier-is-aux (car form) 'unquote)))

(define (qq-is-unquote-splicing form)
  (and (list? form)
       (= (length form) 2)
       (identifier-is-aux (car form) 'unquote-splicing)))

(define (qq-is-quasiquote form)
  (and (list? form)
       (= (length form) 2)
       (identifier-is-special (car form) 'quasiquote)))

(define (qq-is-quote-nil form)
  ;; looking for '() i.e (quote ())
  (and (list? form)
       (= (length form) 2)
       (eq? (car form) *qq-quote*)
       (null? (cadr form))))

(define (qq-is-null-or-quoted form)
  (or (null? form)
      (and (pair? form) (eq? (car form) *qq-quote*))))

(define (qq-is-frob form)
  (and (list? form)
       (= (length form) 2)
       (memq (car form) '(unquote unquote-splicing quasiquote))))

(define (qq-maptree fn x)
  (if (atom? x)
      (fn x)
      (let ((a (fn (car x)))
            (d (qq-maptree fn (cdr x))))
        (if (and (eqv? a (car x))
                 (eqv? d (cdr x)))
            x
            (cons a d)))))

(define (qq-simplify form)
  (if (atom? form)
      form
      (let ((form (if (eq? (car form) *qq-quote*)
                      form
                      (qq-maptree qq-simplify form))))
        (if (eq? (car form) *qq-append*)
            (qq-simplify-args form)
            form))))

(define (qq-simplify-args form)
  (let loop ((args (reverse (cdr form)))
             (result '()))
    (if (null? args)
        result
        (cond ((atom? (car form))
               (loop (cdr args)
                     (qq-attach-append *qq-append* (car args) result)))
              ((and (eq? (caar args) *qq-list*)
                    (not (qq-any? qq-is-unquote-splicing (cadr args))))
               (loop (cdr args)
                     (qq-attach-conses (cdar args) result)))
              ((and (eq? (caar args) *qq-list-star*)
                    (not (qq-any? qq-is-unquote-splicing (cdar args))))
               (loop (cdr args)
                     (qq-attach-conses (reverse (cdar args))
                                       (qq-attach-append *qq-append*
                                                         (car (last (car args)))
                                                         result))))
              ((and (eq? (caar args) *qq-quote*)
                    (pair? (cadar args))
                    (qq-is-frob (cadar args))
                    (null? (cddar args)))
               (loop (cdr args)
                     (qq-attach-conses (list (list *qq-quote* (caadar args)))
                                       result)))
              (else (qq-attach-append *qq-append* (car args) result))))))

(define (qq-attach-conses items result)
  (cond ((and (qq-all? qq-is-null-or-quoted items)
              (qq-is-null-or-quoted result))
         (list *qq-quote* (append (map cadr items)) (cadr result)))
        ((or (null? result)
             (qq-is-quote-nil result))
         (cons *qq-list* items))
        ((and (pair? result)
              (or (eq? (car result) *qq-list*)
                  (eq? (car result) *qq-list-star*)))
         (cons (car result) (append items (cdr result))))
        (else (cons *qq-list-star* (append items (list result))))))

(define (qq-attach-append op item result)
  (cond ((and (qq-is-null-or-quoted item)
              (qq-is-null-or-quoted result))
         (cond ((and (null? (cadr item))
                     (null? (cadr result)))
                (list *qq-quote* '()))
               ((null? (cadr item))
                (list *qq-quote* (cadr result)))
               ((null? result)
                (list *qq-quote* (cadr item)))
               (else (list *qq-quote* (append (cadr item) (cadr result))))))
        ((or (null? result)
             (qq-is-quote-nil result))
         (if (qq-is-unquote-splicing result)
             (list op item)
             item))
        ((and (pair? result)
              (eq? (car result) op))
         (list* (car result) item (cdr result)))
        (else (list op item result))))

(define (qq-remove-tokens form)
  (cond ((eq? form *qq-list*) (identifier 'global 'list 'list))
        ((eq? form *qq-append*) (identifier 'global 'append 'append))
        ((eq? form *qq-list-star*) (identifier 'global 'list* 'list*))
        ((eq? form *qq-quote*) (identifier 'special 'quote 'quote))
        ((eq? form *qq-list->vector*) (identifier 'global 'list->vector 'list->vector))
        ((atom? form) form)
        ((and (eq? (car form) *qq-list-star*)
              (pair? (cddr form))
              (null? (cdddr form)))
         (cons (identifier 'primacll 'cons 'cons)
               (qq-maptree qq-remove-tokens (cdr form))))
        (else (qq-maptree qq-remove-tokens form))))

(define (qq-any? fn ls)
  (any? (map fn ls)))

(define (qq-all? fn ls)
  (all? (map fn ls)))
