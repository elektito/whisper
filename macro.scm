(import (trick))

(define-record-type <sequence>
  (make-sequence)
  sequence?
  (items sequence-items sequence-items-set!))

(define (new-sequence)
  (let ((seq (make-sequence)))
    (sequence-items-set! seq #())
    seq))

(define (sequence-add! seq value)
  (sequence-items-set! seq (vector-append (sequence-items seq) (vector value))))

(define (sequence . items)
  (let loop ((seq (new-sequence)) (items items))
    (if (null? items)
        seq
        (begin
          (sequence-add! seq (car items))
          (loop seq (cdr items))))))

(define-record-type <store>
  (make-store)
  store?
  (vars store-vars store-vars-set!))

(define (new-store)
  (let ((store (make-store)))
    (store-vars-set! store '())
    store))

(define (store-add-value store var value)
  (print "store-add-value" var value)
  (let ((p (assq var (store-vars store))))
    (if p
        (sequence-add! (cdr p) value)
        (store-vars-set! store (cons `(,var . ,(sequence value)) (store-vars store))))))

(define (store-get-var store var)
  (let ((p (assq var (store-vars store))))
    (if p
        (cdr p)
        #f)))

(define (x-store-add-value x v)
  (if (not (symbol? x))
      (error "store-add-value variable name not a symbol"))
  (print "store-add-value" x v))

(define (store-add-seq x v)
  (if (not (symbol? x))
      (error "store-add-seq variable name not a symbol"))
  (print "store-add-seq" x v))

(define (starts-with-seq? ls ellipsis)
  (and (pair? ls)
       (pair? (cdr ls))
       (eq? ellipsis (cadr ls))))

(define (sublist ls start end)
  (when (negative? start)
    (error "sublist: start is negative"))
  (when (< end start)
    (error "sublist: start is greater than end"))
  (let loop ((result '()) (i 0) (ls ls))
    (cond ((null? ls) (if (= i end)
                          (reverse result)
                          (error "sublist: list is too short")))
          ((atom? ls) (if (= i end)
                          (reverse result)
                          (error "sublist indices can't span an improper tail")))
          (else (if (= i end)
                    (reverse result)
                    (if (>= i start)
                        (loop (cons (car ls) result) (+ i 1) (cdr ls))
                        (loop result (+ i 1) (cdr ls))))))))

;;(define (sublist ls start end)
;;  (vector->list (vector-copy (list->vector ls) start end)))

;; length, including the last cdr
;; so:
;; (improper-length '(1 2 . 3)) => 3
;; (improper-length '(1 2 3)) => 4
(define (improper-length ls)
  (cond ((atom? ls) 1)
        (else (+ 1 (improper-length (cdr ls))))))

(define (%improper-reverse ls acc)
  (if (atom? ls)
      (cons ls acc)
      (%improper-reverse (cdr ls) (cons (car ls) acc))))

(define (improper-reverse ls)
  (%improper-reverse ls '()))

(define (find-ellipsis-idx-in-vector vec ellipsis)
  (let ((veclen (vector-length vec)))
    (let loop ((i 0))
      (if (= i veclen)
          #f
          (if (eq? ellipsis (vector-ref vec i))
              (- i 1)
              (loop (+ i 1)))))))

(define (compile-all-vector-elements vec literals ellipsis)
  (map (lambda (x) (compile-pattern x literals ellipsis)) (vector->list vec)))

(define (compile-literal lit)
  (lambda (x store)
    (eq? x lit)))

(define (compile-variable var)
  (lambda (x store)
    (store-add-value store var x)
    #t))

(define (compile-seq var literals ellipsis)
  (if (symbol? var)
      (lambda (x store)
        (store-add-seq var x)
        #t)
      (let ((m (compile-pattern var literals ellipsis)))
        (lambda (x store)
          (if (vector? x)
              (all? (vector->list (vector-map (lambda (x) (m x store)) x)))
              (all? (map (lambda (x) (m x store)) x)))))))

(define (compile-pair pair literals ellipsis)
  (if (starts-with-seq? pair ellipsis)
      (compile-pair-with-seq pair literals ellipsis)
      (compile-pair-without-seq pair literals ellipsis)))

(define (compile-pair-without-seq pair literals ellipsis)
  (let ((car-matcher (compile-pattern (car pair) literals ellipsis))
        (cdr-matcher (compile-pattern (cdr pair) literals ellipsis)))
    (lambda (x store)
      (and (pair? x)
           (car-matcher (car x) store)
           (cdr-matcher (cdr x) store)))))

(define (compile-pair-with-seq pair literals ellipsis)
  (let* ((rest (cddr pair))
         (rest-len (improper-length rest))
         (rev (improper-reverse rest))
         (rev-matcher (compile-pattern rev literals ellipsis))
         (seq-matcher (compile-seq (car pair) literals ellipsis)))
    (lambda (x store)
      (let ((n-left (- (improper-length x) rest-len)))
        (and (>= n-left 0)
             (let ((x-end-rev (sublist (improper-reverse x) 0 rest-len))
                   (x-start (sublist x 0 n-left)))
               (and (rev-matcher x-end-rev store)
                    (seq-matcher x-start store))))))))

(define (compile-vector vec literals ellipsis)
  (let ((idx (find-ellipsis-idx-in-vector vec ellipsis)))
    (if idx
        (compile-vector-with-seq vec idx literals ellipsis)
        (compile-vector-without-seq vec literals ellipsis))))

(define (compile-vector-without-seq vec literals ellipsis)
  (let ((veclen (vector-length vec))
        (matchers (compile-all-vector-elements vec literals ellipsis)))
    (lambda (x store)
      (and (vector? x)
           (= (vector-length x) veclen)
           (let loop ((i 0) (matchers matchers))
             (if (null? matchers)
                 #t
                 (and ((car matchers) (vector-ref x i) store)
                      (loop (+ i 1) (cdr matchers)))))))))

(define (compile-vector-with-seq vec idx literals ellipsis)
  (let* ((left-idx idx)
         (right-idx (- (vector-length vec) idx))
         (sub-left (vector-copy vec 0 left-idx))
         (sub-right (vector-copy vec right-idx))
         (left-matcher (compile-vector-without-seq sub-left literals ellipsis))
         (right-matcher (compile-vector-without-seq sub-right literals ellipsis))
         (seq-matcher (compile-seq (vector-ref vec left-idx) literals ellipsis)))
    (lambda (x store)
      (and (vector? x)
           (let ((right-idx (- (vector-length x) (vector-length sub-right))))
             (and (>= right-idx left-idx)
                  (let ((x-left (vector-copy x 0 left-idx))
                        (x-right (vector-copy x right-idx))
                        (x-middle (vector-copy x left-idx right-idx)))
                    (and (left-matcher x-left store)
                         (right-matcher x-right store)
                         (seq-matcher x-middle store)))))))))

(define (compile-pattern pat literals ellipsis)
  (cond ((symbol? pat) (if (memq pat literals)
                           (compile-literal pat)
                           (compile-variable pat)))
        ((vector? pat) (compile-vector pat literals ellipsis))
        ((atom? pat) (compile-literal pat))
        (else (compile-pair pat literals ellipsis))))

(let ((m (compile-pattern #(a (b x ...) ... c) '(else) '...)))
  (print (m #(1 (2 3 a b) (4 5 foo bar spam) 6) (new-store))))

;;(let ((m (compile-pattern '#(a #(x ...) b) '(else) '...)))
;;  (print (m '#(1 #(2 3 4 5) 6) (new-store))))
