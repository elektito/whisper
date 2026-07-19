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
        (compile-error "internal error: duplicate variable")
        (store-vars-set! store (cons `(,var . ,value) (store-vars store))))))

(define (store-set-var store var value)
  (let ((p (assq var (store-vars store))))
    (if p
        (set-cdr! p value)
        (store-add-value store var value))))

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

(define (starts-with-seq? ls is-ellipsis?)
  (and (pair? ls)
       (pair? (cdr ls))
       (is-ellipsis? (cadr ls))))

(define (sublist ls start end)
  (when (negative? start)
    (compile-error "internal error: sublist: start is negative"))
  (when (< end start)
    (compile-error "internal error: sublist: start is greater than end"))
  (let loop ((result '()) (i 0) (ls ls))
    (cond ((null? ls) (if (= i end)
                          (reverse result)
                          (compile-error "internal error: sublist: list is too short")))
          ((atom? ls) (if (= i end)
                          (reverse result)
                          (compile-error "internal error: sublist indices can't span an improper tail")))
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

(define (find-ellipsis-idx-in-vector vec is-ellipsis?)
  (let ((veclen (vector-length vec)))
    (let loop ((i 0))
      (if (= i veclen)
          #f
          (if (is-ellipsis? (vector-ref vec i))
              (- i 1)
              (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pp-compile-all-vector-elements vec literal-bindings is-ellipsis? is-wildcard?)
  (map (lambda (x) (pp-compile-pattern x literal-bindings is-ellipsis? is-wildcard?)) (vector->list vec)))

(define (pp-compile-literal lit def-binding)
  ;; this should check if either one of the following is true:
  ;;  - x in the use-env has the same binding as lit in def-env
  ;;    (def-binding).
  ;;  - x is unbound in use-env, lit is unbound in def-env, and they
  ;;    both have the same name.
  (lambda (x store use-env)
    (let ((use-binding (cond ((identifier? x) (identifier-binding x))
                             ((symbol? x) (expand-env-lookup use-env x))
                             (else #f))))
      (if (and def-binding use-binding)
          (binding-denotes-same? def-binding use-binding)
          (cond (def-binding #f)
                (use-binding #f)
                (else (let ((lit-name (if (identifier? lit) (identifier-name lit) lit))
                            (x-name (if (identifier? x) (identifier-name x) x)))
                        (eq? lit-name x-name))))))))

(define (pp-compile-variable var)
  (lambda (x store use-env)
    (store-add-value store var x)
    #t))

(define (get-all-pattern-vars pattern literal-bindings is-ellipsis? is-wildcard?)
  (cond ((is-wildcard? pattern) '())
        ((is-ellipsis? pattern) '())
        ((assq pattern literal-bindings) '())
        ((symbol? pattern) (list pattern))
        ((identifier? pattern) (list pattern))
        ((pair? pattern) (append (get-all-pattern-vars (car pattern) literal-bindings is-ellipsis? is-wildcard?)
                                 (get-all-pattern-vars (cdr pattern) literal-bindings is-ellipsis? is-wildcard?)))
        ((vector? pattern) (let loop ((results '()) (i 0))
                             (if (= i (vector-length pattern))
                                 results
                                 (loop (append results (get-all-pattern-vars (vector-ref pattern i) literal-bindings is-ellipsis? is-wildcard?))
                                       (+ i 1)))))
        (else '())))

;; syntax-rules forbids a pattern variable from appearing more than once
;; in one pattern. return the first variable that repeats, or #f if all
;; are distinct.
(define (repeated-pattern-var pattern literal-bindings is-ellipsis? is-wildcard?)
  (let loop ((vars (get-all-pattern-vars pattern literal-bindings is-ellipsis? is-wildcard?))
             (seen '()))
    (cond ((null? vars) #f)
          ((memq (car vars) seen) (car vars))
          (else (loop (cdr vars) (cons (car vars) seen))))))

(define (merge-stores main subs pattern literal-bindings is-ellipsis? is-wildcard?)
  (let* ((vars (get-all-pattern-vars pattern literal-bindings is-ellipsis? is-wildcard?)))
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

(define (pp-compile-seq var literal-bindings is-ellipsis? is-wildcard?)
  (let ((m (pp-compile-pattern var literal-bindings is-ellipsis? is-wildcard?)))
    (lambda (x store use-env)
      (let loop ((x (if (vector? x) (vector->list x) x))
                 (sub-store (new-store))
                 (stores '()))
        (if (null? x)
            (begin
              (merge-stores store (reverse stores) var literal-bindings is-ellipsis? is-wildcard?)
              #t)
            (begin
              (if (m (car x) sub-store use-env)
                  (loop (cdr x) (new-store) (cons sub-store stores))
                  #f)))))))

(define (pp-compile-pair pair literal-bindings is-ellipsis? is-wildcard?)
  (if (starts-with-seq? pair is-ellipsis?)
      (pp-compile-pair-with-seq pair literal-bindings is-ellipsis? is-wildcard?)
      (pp-compile-pair-without-seq pair literal-bindings is-ellipsis? is-wildcard?)))

(define (pp-compile-pair-without-seq pair literal-bindings is-ellipsis? is-wildcard?)
  (let ((car-matcher (pp-compile-pattern (car pair) literal-bindings is-ellipsis? is-wildcard?))
        (cdr-matcher (pp-compile-pattern (cdr pair) literal-bindings is-ellipsis? is-wildcard?)))
    (lambda (x store use-env)
      (and (pair? x)
           (car-matcher (car x) store use-env)
           (cdr-matcher (cdr x) store use-env)))))

(define (pp-compile-pair-with-seq pair literal-bindings is-ellipsis? is-wildcard?)
  (let* ((rest (cddr pair))
         (rest-len (improper-length rest))
         (rev (improper-reverse rest))
         (rev-matcher (pp-compile-pattern rev literal-bindings is-ellipsis? is-wildcard?))
         (seq-matcher (pp-compile-seq (car pair) literal-bindings is-ellipsis? is-wildcard?)))
    (lambda (x store use-env)
      (let ((n-left (- (improper-length x) rest-len)))
        (and (>= n-left 0)
             (let ((x-end-rev (sublist (improper-reverse x) 0 rest-len))
                   (x-start (sublist x 0 n-left)))
               (and (rev-matcher x-end-rev store use-env)
                    (seq-matcher x-start store use-env))))))))

(define (pp-compile-vector vec literal-bindings is-ellipsis? is-wildcard?)
  (let ((idx (find-ellipsis-idx-in-vector vec is-ellipsis?)))
    (if idx
        (pp-compile-vector-with-seq vec idx literal-bindings is-ellipsis? is-wildcard?)
        (pp-compile-vector-without-seq vec literal-bindings is-ellipsis? is-wildcard?))))

(define (pp-compile-vector-without-seq vec literal-bindings is-ellipsis? is-wildcard?)
  (let ((veclen (vector-length vec))
        (matchers (pp-compile-all-vector-elements vec literal-bindings is-ellipsis? is-wildcard?)))
    (lambda (x store use-env)
      (and (vector? x)
           (= (vector-length x) veclen)
           (let loop ((i 0) (matchers matchers))
             (if (null? matchers)
                 #t
                 (and ((car matchers) (vector-ref x i) store use-env)
                      (loop (+ i 1) (cdr matchers)))))))))

(define (pp-compile-vector-with-seq vec idx literal-bindings is-ellipsis? is-wildcard?)
  (let* ((left-idx idx)
         (right-idx (+ idx 2)) ;; 2: pattern variable plus ellipsis
         (sub-left (vector-copy vec 0 left-idx))
         (sub-right (vector-copy vec right-idx))
         (left-matcher (pp-compile-vector-without-seq sub-left literal-bindings is-ellipsis? is-wildcard?))
         (right-matcher (pp-compile-vector-without-seq sub-right literal-bindings is-ellipsis? is-wildcard?))
         (seq-matcher (pp-compile-seq (vector-ref vec left-idx) literal-bindings is-ellipsis? is-wildcard?)))
    (lambda (x store use-env)
      (and (vector? x)
           (let ((right-idx (- (vector-length x) (vector-length sub-right))))
             (and (>= right-idx left-idx)
                  (let ((x-left (vector-copy x 0 left-idx))
                        (x-right (vector-copy x right-idx))
                        (x-middle (vector-copy x left-idx right-idx)))
                    (and (left-matcher x-left store use-env)
                         (right-matcher x-right store use-env)
                         (seq-matcher x-middle store use-env)))))))))

(define (pp-compile-pattern pat literal-bindings is-ellipsis? is-wildcard?)
  (cond ((is-wildcard? pat) (lambda (x store use-env) #t))
        ((is-ellipsis? pat) (compile-error "ellipsis in invalid pattern position"))
        ;; literal-bindings is a list of (symbol . binding) pairs
        ((symbol? pat) (let ((pair (assq pat literal-bindings)))
                         (if pair
                             (pp-compile-literal (car pair) (cdr pair))
                             (pp-compile-variable pat))))
        ((identifier? pat)
         (let ((b (identifier-binding pat)))
           (let loop ((lbs literal-bindings))
             (cond ((null? lbs) (pp-compile-variable pat))
                   ((or (and b (eq? b (cdar lbs)))
                        (and (not b)
                             (not (cdar lbs))
                             (eq? pat (caar lbs))))
                    (pp-compile-literal (caar lbs) (cdar lbs)))
                   (else (loop (cdr lbs)))))))
        ((vector? pat) (pp-compile-vector pat literal-bindings is-ellipsis? is-wildcard?))
        ((atom? pat) (lambda (x store use-env)
                       (equal? x pat)))
        (else (pp-compile-pair pat literal-bindings is-ellipsis? is-wildcard?))))

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

(define (zip-seqs item)
  ;; find all sequences in item
  ;; error if none
  ;; error if not all have the same length
  ;; zip the sequences: (1 [2 3] [a b]) => (1 2 a) (1 3 b)
  (let ((seqs (find-all-seqs item)))
    (when (null? seqs)
      (compile-error "no sequences in expanded element"))
    (unless (or (< (length seqs) 2)
                (apply = (map sequence-length seqs)))
      (compile-error "sequences do not have the same sizes"))
    (let ((len (sequence-length (car seqs))))
      (let loop ((i 0) (result '()))
        (if (= i len)
            (reverse result)
            (loop (+ i 1) (cons (index-seqs item i) result)))))))

;; returns the pattern variables of `template` that are bound to a
;; <sequence> in `store`, by walking `template` and collecting any
;; symbol or identifier found in `store` whose value is a <sequence>.
(define (template-seq-vars template store)
  (cond ((and (symbol-or-identifier? template) (store-has-var store template))
         (if (sequence? (store-get-var store template)) (list template) '()))
        ((pair? template)
         (append (template-seq-vars (car template) store)
                 (template-seq-vars (cdr template) store)))
        ((vector? template)
         (apply append (map (lambda (x) (template-seq-vars x store))
                             (vector->list template))))
        (else '())))

;; expand `template` once per repetition of the ellipsis that follows
;; it. For each repetition i, build a copy of `store` where every
;; variable this ellipsis iterates over is rebound to its i-th slice,
;; then recursively expand the (unmodified) template against that sliced
;; store. slicing before expanding, rather than expanding then searching
;; the result for sequences to multiply, is what keeps a nested ellipsis
;; inside `template` from seeing (and consuming) a dimension that
;; belongs to this outer ellipsis instead.
(define (expand-ellipsis template store is-ellipsis? rename)
  (let ((vars (template-seq-vars template store)))
    (when (null? vars)
      (compile-error "no sequences in expanded element"))
    (let ((lengths (map (lambda (v) (sequence-length (store-get-var store v))) vars)))
      (unless (apply = lengths)
        (compile-error "sequences do not have the same sizes"))
      (let ((len (car lengths)))
        (let loop ((i 0) (result '()))
          (if (= i len)
              (reverse result)
              (let ((sliced (store-copy store)))
                (for-each (lambda (v)
                            (store-set-var sliced v (sequence-ref (store-get-var store v) i)))
                          vars)
                (loop (+ i 1)
                      (cons (expand template sliced is-ellipsis? rename) result)))))))))

(define (expand-vector vec store is-ellipsis? rename)
  (let loop ((result #()) (i 0) (veclen (vector-length vec)))
    (if (= i veclen)
        result
        (if (and (< i (- veclen 1))
                 (is-ellipsis? (vector-ref vec (+ i 1))))
            (let inner ((multiplied (expand-ellipsis (vector-ref vec i) store is-ellipsis? rename))
                        (next (+ i 2)))
              (if (and (< next (- veclen 1))
                       (is-ellipsis? (vector-ref vec next)))
                  (inner (apply append (map zip-seqs multiplied))
                         (+ next 1))
                  (loop (vector-append result (list->vector multiplied))
                        next
                        veclen)))
            (let ((expanded (expand (vector-ref vec i) store is-ellipsis? rename)))
              (loop (vector-append result (vector expanded)) (+ i 1) veclen))))))

(define (expand-pair pair store is-ellipsis? rename)
  (let loop ((result '()) (item pair))
    (cond ((atom? item)
           (append result (expand item store is-ellipsis? rename)))
          ((is-ellipsis? (car item))
           (if (and (pair? (cdr item))
                    (is-ellipsis? (cadr item)))
               (loop (append result (list (rename (car item)))) (cddr item))
               (compile-error "lone ellipsis in template")))
          ((and (pair? (cdr item))
                (is-ellipsis? (cadr item)))
           (let inner ((multiplied (expand-ellipsis (car item) store is-ellipsis? rename))
                       (rest (cddr item)))
             (if (and (pair? rest)
                      (is-ellipsis? (car rest)))
                 (inner (apply append (map zip-seqs multiplied)) (cdr rest))
                 (loop (append result multiplied) rest))))
          (else (let ((expanded (expand (car item) store is-ellipsis? rename)))
                  (loop (append result (list expanded)) (cdr item)))))))

;; detect (<ellipsis> <template>) escape (which is supposed to emit
;; <template> but with ellipses inside it treated as ordinary
;; identifiers (renamed, never as repetition). the canonical use is
;; (... ...) which yields a single literal ellipsis.
(define (is-ellipsis-escape? template is-ellipsis?)
  (and (pair? template)
       (is-ellipsis? (car template))
       (pair? (cdr template))
       (null? (cddr template))))

(define (expand-escaped template store rename)
  ;; expand with the is-ellipsis? function always returning false
  (expand template store (lambda (x) #f) rename))

(define (expand template store is-ellipsis? rename)
  (cond ((store-has-var store template) (store-get-var store template))
        ((vector? template) (expand-vector template store is-ellipsis? rename))
        ((symbol? template) (rename template))
        ((atom? template) template)
        ((is-ellipsis-escape? template is-ellipsis?)
         (expand-escaped (cadr template) store rename))
        (else (expand-pair template store is-ellipsis? rename))))

(define (compile-syntax-rules form def-env)
  (when (< (length form) 3)
    (compile-error "invalid syntax-rules"))

  ;; a custom ellipsis is bound to a fresh aux binding in a private frame,
  ;; keyed by its resolution key: the symbol when written directly, or its
  ;; per-expansion rename when this form came from an enclosing macro. every
  ;; occurrence shares that key, so is-ellipsis?/is-wildcard? resolve them
  ;; through the env like any other identifier.
  (let* ((custom-ellipsis (and (symbol-or-identifier? (cadr form)) (cadr form)))
         ;; gensym meaning, not '... because if we used '... under
         ;; denotation comparison (used for non-lexicals), a '... would
         ;; mean the same thing as the custom ellipsis, instead of a
         ;; literal ....
         (ellipsis-aux (new-binding 'aux (gensym "ellipsis")))
         (def-env (if custom-ellipsis
                      (make-expand-env
                       (list (make-identifier (binder-key custom-ellipsis) ellipsis-aux))
                       def-env)
                      def-env))
         (ellipsis-binding (if custom-ellipsis
                               ellipsis-aux
                               (expand-env-lookup def-env '...)))
         (wildcard-binding (expand-env-lookup def-env '_))
         (_ (unless ellipsis-binding
              (compile-error "internal error: no ellipsis (...) in definition environment")))
         (_ (unless wildcard-binding
              (compile-error "internal error: no wildcard (_) in definition environment")))
         (is-ellipsis? (lambda (x)
                         (let ((b (resolve-head x def-env)))
                           (and b (binding-denotes-same? b ellipsis-binding)))))
         (is-wildcard? (lambda (x)
                         (let ((b (resolve-head x def-env)))
                           (and b (binding-denotes-same? b wildcard-binding)))))
         (literals (if custom-ellipsis (caddr form) (cadr form)))
         (literal-bindings (map (lambda (x) ;; build an alist (symbol . binding-or-#f)
                                  (if (identifier? x)
                                      (cons x (identifier-binding x))
                                      (cons x (expand-env-lookup def-env x))))
                                literals))
         (rules (let loop ((result '())
                           (rest (if custom-ellipsis (cdddr form) (cddr form))))
                  (if (null? rest)
                      (reverse result)
                      (if (and (pair? (car rest))
                               (= 2 (length (car rest)))
                               (pair? (caar rest)))
                          (let* ((pattern (cdaar rest))
                                 (dup (repeated-pattern-var pattern literal-bindings is-ellipsis? is-wildcard?)))
                            (when dup
                              (compile-error "pattern variable ~a used more than once in syntax-rules pattern"
                                             (if (identifier? dup) (identifier-name dup) dup)))
                            (loop (cons (cons (pp-compile-pattern pattern literal-bindings is-ellipsis? is-wildcard?)
                                              (cadar rest))
                                        result)
                                  (cdr rest)))
                          (compile-error "invalid rule"))))))
    (when (null? rules)
      (compile-error "syntax-rules has no rules"))
    (make-transformer def-env
                      (lambda (input use-env)
                        (let* ((rename-map '())
                               (rename (lambda (x)
                                         (if (identifier? x)
                                             x
                                             (let ((mapping (assq x rename-map)))
                                               (if mapping
                                                   (cdr mapping)
                                                   (let* ((binding (expand-env-lookup def-env x))
                                                          (id (make-identifier x (if binding
                                                                                     binding
                                                                                     (new-binding 'global x)))))
                                                     (identifier-rename-set! id (gensym))
                                                     (set! rename-map (cons (cons x id) rename-map))
                                                     id)))))))
                          (let loop ((rules rules))
                            (if (null? rules)
                                (compile-error "no rule matched input: ~s" input)
                                (let ((store (new-store))
                                      (pattern (caar rules))
                                      (template (cdar rules)))
                                  (if (pattern (cdr input) store use-env)
                                      (expand template store is-ellipsis? rename)
                                      (loop (cdr rules)))))))))))
