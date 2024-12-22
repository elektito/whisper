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

(define (sequence-ref seq idx)
  (vector-ref (sequence-items seq) idx))

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
  ;; unless
  (if (not (null? alist))
      (begin
        (store-set-var store (caar alist) (cdar alist))
        (store-update-from-alist store (cdr alist)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pp-compile-all-vector-elements vec literals ellipsis)
  (map (lambda (x) (pp-compile-pattern x literals ellipsis)) (vector->list vec)))

(define (pp-compile-literal lit)
  (lambda (x store)
    (eq? x lit)))

(define (pp-compile-variable var)
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

(define (pp-compile-seq var literals ellipsis)
  (let ((m (pp-compile-pattern var literals ellipsis)))
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

(define (pp-compile-pair pair literals ellipsis)
  (if (starts-with-seq? pair ellipsis)
      (pp-compile-pair-with-seq pair literals ellipsis)
      (pp-compile-pair-without-seq pair literals ellipsis)))

(define (pp-compile-pair-without-seq pair literals ellipsis)
  (let ((car-matcher (pp-compile-pattern (car pair) literals ellipsis))
        (cdr-matcher (pp-compile-pattern (cdr pair) literals ellipsis)))
    (lambda (x store)
      (and (pair? x)
           (car-matcher (car x) store)
           (cdr-matcher (cdr x) store)))))

(define (pp-compile-pair-with-seq pair literals ellipsis)
  (let* ((rest (cddr pair))
         (rest-len (improper-length rest))
         (rev (improper-reverse rest))
         (rev-matcher (pp-compile-pattern rev literals ellipsis))
         (seq-matcher (pp-compile-seq (car pair) literals ellipsis)))
    (lambda (x store)
      (let ((n-left (- (improper-length x) rest-len)))
        (and (>= n-left 0)
             (let ((x-end-rev (sublist (improper-reverse x) 0 rest-len))
                   (x-start (sublist x 0 n-left)))
               (and (rev-matcher x-end-rev store)
                    (seq-matcher x-start store))))))))

(define (pp-compile-vector vec literals ellipsis)
  (let ((idx (find-ellipsis-idx-in-vector vec ellipsis)))
    (if idx
        (pp-compile-vector-with-seq vec idx literals ellipsis)
        (pp-compile-vector-without-seq vec literals ellipsis))))

(define (pp-compile-vector-without-seq vec literals ellipsis)
  (let ((veclen (vector-length vec))
        (matchers (pp-compile-all-vector-elements vec literals ellipsis)))
    (lambda (x store)
      (and (vector? x)
           (= (vector-length x) veclen)
           (let loop ((i 0) (matchers matchers))
             (if (null? matchers)
                 #t
                 (and ((car matchers) (vector-ref x i) store)
                      (loop (+ i 1) (cdr matchers)))))))))

(define (pp-compile-vector-with-seq vec idx literals ellipsis)
  (let* ((left-idx idx)
         (right-idx (- (vector-length vec) idx))
         (sub-left (vector-copy vec 0 left-idx))
         (sub-right (vector-copy vec right-idx))
         (left-matcher (pp-compile-vector-without-seq sub-left literals ellipsis))
         (right-matcher (pp-compile-vector-without-seq sub-right literals ellipsis))
         (seq-matcher (pp-compile-seq (vector-ref vec left-idx) literals ellipsis)))
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

(define (pp-compile-pattern pat literals ellipsis)
  (cond ((symbol? pat) (if (memq pat literals)
                           (pp-compile-literal pat)
                           (pp-compile-variable pat)))
        ((vector? pat) (pp-compile-vector pat literals ellipsis))
        ((atom? pat) (pp-compile-literal pat))
        (else (pp-compile-pair pat literals ellipsis))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (index-seqs item idx)
  (cond ((sequence? item) (sequence-ref item idx))
        ((pair? item) (cons (index-seqs (car item) idx)
                            (index-seqs (cdr item) idx)))
        ((vector? item) (vector-map (lambda (x) (index-seqs x idx)) item))
        (else item)))

(define (find-all-seqs item)
  (cond ((sequence? item) (list item))
        ((pair? item) (append (find-all-seqs (car item))
                              (find-all-seqs (cdr item))))
        ((vector? item) (apply append (vector->list (vector-map find-all-seqs item))))
        (else '())))

(define (multiply-seqs item)
  ;; find all sequences in item
  ;; error if none
  ;; error if not all have the same length
  ;; perform a cartesian multiplication: (1 [2 3] [a b]) => (1 2 a) (1 3 b)
  (let ((seqs (find-all-seqs item)))
    (when (null? seqs)
      (error "no sequences in expanded element"))
    (unless (or (< (length seqs) 2)
                (apply = (map sequence-length seqs)))
        (error "sequences do not have the same sizes"))
    (let ((len (sequence-length (car seqs))))
      (let loop ((i 0) (result '()))
        (if (= i len)
            (reverse result)
            (loop (+ i 1) (cons (index-seqs item i) result)))))))

(define (expand-vector vec store ellipsis)
  (let loop ((result #()) (i 0) (veclen (vector-length vec)))
    (if (= i veclen)
        result
        (if (and (< i (- veclen 1))
                 (eq? ellipsis (vector-ref vec (+ i 1))))
            (let ((expanded (expand (vector-ref vec i) store ellipsis)))
              (let inner ((multiplied (multiply-seqs expanded)) (next (+ i 2)))
                (if (and (< next (- veclen 1))
                         (eq? ellipsis (vector-ref vec next)))
                    (inner (apply append (map multiply-seqs multiplied))
                           (+ next 1))
                    (loop (vector-append result (list->vector multiplied))
                          next
                          veclen))))
            (let ((expanded (expand (vector-ref vec i) store ellipsis)))
              (loop (vector-append result (vector expanded)) (+ i 1) veclen))))))

(define (expand-pair pair store ellipsis)
  (let loop ((result '()) (item pair))
    (cond ((atom? item)
           (append result (expand item store ellipsis)))
          ((eq? ellipsis (car item))
           (if (and (pair? (cdr item))
                    (eq? ellipsis (cadr item)))
               (loop (append result (list ellipsis)) (cddr item))
               (error "lone ellipsis in template")))
          ((and (pair? (cdr item))
                (eq? ellipsis (cadr item)))
           (let ((expanded (expand (car item) store ellipsis)))
             (let inner ((multiplied (multiply-seqs expanded))
                         (rest (cddr item)))
               (if (and (pair? rest)
                        (eq? ellipsis (car rest)))
                   (inner (apply append (map multiply-seqs multiplied)) (cdr rest))
                   (loop (append result multiplied) rest)))))
          (else (let ((expanded (expand (car item) store ellipsis)))
                  (loop (append result (list expanded)) (cdr item)))))))

(define (expand template store ellipsis)
  (cond ((store-has-var store template) (store-get-var store template))
        ((vector? template) (expand-vector template store ellipsis))
        ((atom? template) template)
        (else (expand-pair template store ellipsis))))

(define-record-type <transformer>
  (make-transformer func)
  transformer?
  (func transformer-func))

(define (compile-syntax-rules form)
  (when (< (length form) 3)
    (error "invalid syntax-rules"))
  (let* ((ellipsis (if (symbol? (cadr form)) (cadr form) '...))
         (literals (if (symbol? (cadr form)) (caddr form) (cadr form)))
         (rules (let loop ((result '())
                           (rest (if (symbol? (cadr form)) (cdddr form) (cddr form))))
                  (if (null? rest)
                      (reverse result)
                      (if (and (pair? (car rest))
                               (= 2 (length (car rest)))
                               (pair? (caar rest)))
                          (loop (cons (cons (pp-compile-pattern (cdaar rest) literals ellipsis)
                                            (cadar rest))
                                      result)
                                (cdr rest))
                          (error "invalid rule"))))))
    (when (null? rules)
      (error "syntax-rules has no rules"))
    (make-transformer (lambda (input)
                        (let loop ((rules rules))
                          (if (null? rules)
                              (error "no rule matched input")
                              (let ((store (new-store))
                                    (pattern (caar rules))
                                    (template (cdar rules)))
                                (if (pattern (cdr input) store)
                                    (expand template store ellipsis)
                                    (loop (cdr rules))))))))))
