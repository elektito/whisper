(import (trick))

(define (deep-copy x)
  (if (pair? x)
      (cons (deep-copy (car x)) (deep-copy (cdr x)))
      x))

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

(define (sequence-length seq)
  (vector-length (sequence-items seq)))

(define (sequence->list seq)
  (vector->list (sequence-items seq)))

(define-record-type <store>
  (make-store)
  store?
  (vars store-vars store-vars-set!))

(define (new-store)
  (let ((store (make-store)))
    (store-vars-set! store '())
    store))

(define (store-copy store)
  (let ((s (new-store)))
    (store-vars-set! s (deep-copy (store-vars store)))
    s))

(define (store-add-value store var value)
  (print "store-add-value" var value)
  (let ((p (assq var (store-vars store))))
    (if p
        (error "duplicate variable")
        (store-vars-set! store (cons `(,var . ,value) (store-vars store))))))

(define (store-set-var store var value)
  (let ((p (assq var (store-vars store))))
    (if p
        (set-cdr! p value)
        (store-add-value var value))))

(define (store-get-var store var)
  (let ((p (assq var (store-vars store))))
    (if p
        (cdr p)
        #f)))

(define (store-has-var store var)
  (not (not (assq var (store-vars store)))))

(define (store-get-all-vars store)
  (map car (store-vars store)))

(define (store-update-from-alist store alist)
  (unless (null? alist)
    (store-set-var store (caar alist) (cdar alist))
    (store-update-from-alist store (cdr alist))))

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

(define (get-all-pattern-vars pattern literals ellipsis)
  (cond ((eq? ellipsis pattern) '())
        ((memq pattern literals) '())
        ((symbol? pattern) (list pattern))
        ((pair? pattern) (append (get-all-pattern-vars (car pattern) literals ellipsis)
                                 (get-all-pattern-vars (cdr pattern) literals ellipsis)))
        ((vector? pattern) (let loop ((results '()) (i 0))
                             (if (= i (vector-length pattern))
                                 results
                                 (loop (append results (get-all-pattern-vars (vector-ref pattern i) literals ellipsis))
                                       (+ i 1)))))
        (else '())))

(define (merge-stores main subs pattern literals ellipsis)
  (let* ((vars (get-all-pattern-vars pattern literals ellipsis))
         (seqs (make-list (length vars) (sequence))))
    (let lp1 ((vars vars) (seq (sequence)))
      (if (null? vars)
          (void)
          (let lp2 ((subs subs))
            (if (null? subs)
                (begin
                  (store-add-value main (car vars) seq)
                  (lp1 (cdr vars) (sequence)))
                (begin
                  (sequence-add! seq (store-get-var (car subs) (car vars)))
                  (lp2 (cdr subs)))))))))

(define (compile-seq var literals ellipsis)
  (let ((m (compile-pattern var literals ellipsis)))
    (lambda (x store)
      (let loop ((x (if (vector? x) (vector->list x) x))
                 (sub-store (new-store))
                 (stores '()))
        (if (null? x)
            (begin
              (merge-stores store (reverse stores) var literals ellipsis)
              #t)
            (begin
              (if (m (car x) sub-store)
                  (loop (cdr x) (new-store) (cons sub-store stores))
                  #f)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-sequence-vars element store)
  (cond ((symbol? element) (if (store-has-var store element)
                               (list element)
                               '()))
        ((vector? element)
         (vector->list (vector-map (lambda (x)
                                     (get-sequence-vars x store))
                                   element)))
        ((atom? element) '())
        (else (append (get-sequence-vars (car element) store)
                      (get-sequence-vars (cdr element) store)))))

(define (multiply-seqs store vars)
  (when (null? vars)
    (error "multiply-seqs cannot be called with zero variables"))

  (let ((lens (map (lambda (x) (and (sequence? (store-get-var store x))
                                    (sequence-length (store-get-var store x))))
                   vars)))
    (unless (and (all? lens)
                 (or (< (length lens) 2)
                     (apply = lens)))
      (error "not all sequences are of the same size")))

  (let* ((seqs (map (lambda (x) (store-get-var store x)) vars))
         (lists (map (lambda (x) (sequence->list x)) seqs))
         (mult (apply map (lambda args args) lists)))
    (let loop ((mult mult) (result '()))
      (if (null? mult)
          (reverse result)
          (let ((sub-store (store-copy store)))
            (store-update-from-alist sub-store
                                     (map (lambda (x y) (cons x y))
                                          vars (car mult)))
            (loop (cdr mult) (cons sub-store result)))))))

(define (expand-sequence element store ellipsis)
  (let* ((vars (get-sequence-vars element store))
         (stores (if (null? vars) (list store) (multiply-seqs store vars))))
    (if (null? vars)
        (error "no sequence variables to expand" element)
        (let loop ((stores stores) (result '()))
          (if (null? stores)
              (reverse result)
              (loop (cdr stores) (cons (expand element (car stores) ellipsis) result)))))))

(define (expand-vector vec store ellipsis)
  (let loop ((result #()) (i 0) (veclen (vector-length vec)))
    (if (= i veclen)
        result
        (if (and (< i (- veclen 1))
                 (eq? ellipsis (vector-ref vec (+ i 1))))
            (let ((expanded (expand-sequence (vector-ref vec i) store ellipsis)))
              (loop (vector-append result (list->vector expanded))
                    (+ i 1)
                    veclen))
            (let ((expanded (expand (vector-ref vec i) store ellipsis)))
              (loop (vector-append result (vector expanded)) (+ i 1) veclen))))))

(define (expand-pair pair store ellipsis)
  (let loop ((result '()) (pair pair))
    (cond ((atom? pair)
           (append result pair))
          ((eq? ellipsis (car pair))
           (if (and (pair? (cdr pair))
                    (eq? ellipsis (cadr pair)))
               (loop (append result (list ellipsis)) (cddr pair))
               (error "lone ellipsis in template")))
          ((and (pair? (cdr pair))
                (eq? ellipsis (cadr pair)))
           (let ((expanded (expand-sequence (car pair) store ellipsis)))
             (if (eq? ellipsis (caddr pair))
                 (error "multiple ellipses not supported yet")
                 (loop (append result expanded) (cddr pair)))))
          (else (let ((expanded (expand (car pair) store ellipsis)))
                  (loop (append result (list expanded)) (cdr pair)))))))

(define (expand template store ellipsis)
  (cond ((store-has-var store template) (store-get-var store template))
        ((vector? template) (expand-vector template store ellipsis))
        ((atom? template) template)
        (else (expand-pair template store ellipsis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(let ((m (compile-pattern #(a (b x ...) ... c) '(else) '...)))
;;  (print (m #(1 (2 3 a b) (4 5 foo bar spam) 6) (new-store))))

;;(let ((m (compile-pattern '#(a #(x ...) b) '(else) '...)))
;;  (print (m '#(1 #(2 3 4 5) 6) (new-store))))

(let ((m (compile-pattern '(#(a x ... b)...) '(else) '...))
      (s (new-store)))
  (print (m '(#(1 2 3 4 5 6) #(10 20 30) #(a b c d)) s))
  (print s)
  (print "xx1" (expand '(foo x ... ... bar) s '...))
  )

(print "-------")

(define st (new-store))
(store-add-value st 'a 10)
(store-add-value st 'b 20)
(store-add-value st 'c (sequence 100 200 300))
(store-add-value st 'd 40)
(store-add-value st 'e (sequence 1000 2000 3000))

(print (multiply-seqs st '(c)))
