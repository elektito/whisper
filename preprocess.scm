(define-record-type <identifier>
  (identifier kind name meaning)
  identifier?
  (kind identifier-kind)
  (name identifier-name)
  (meaning identifier-meaning))

(record-set-print <identifier>
                  (lambda (id port)
                    (if (symbol? (identifier-meaning id))
                        (format port "[~s]" (identifier-meaning id))
                        (format port "[~s]" (identifier-name id)))))

(define (free-identifier=? id1 id2)
  (and (identifier? id1)
       (identifier? id2)
       (eq? (identifier-name id1) (identifier-name id2))))

(define (bound-identifier=? id1 id2)
  (and (identifier? id1)
       (identifier? id2)
       (eq? (identifier-kind id1) (identifier-kind id2))
       (identifier-meaning id1)
       (identifier-meaning id2)
       (eq? (identifier-meaning id1) (identifier-meaning id2))))

(define (identifier-is-special id . rest)
  (when (and (pair? rest)
             (> (length rest) 1))
    (error "bad identifier-is-special call"))
  (if (identifier? id)
      (if (null? rest)
          (eq? 'special (identifier-kind id))
          (and (eq? 'special (identifier-kind id))
               (eq? (car rest) (identifier-meaning id))))
      #f))

(define (identifier-is-primcall id . rest)
  (when (and (pair? rest)
             (> (length rest) 1))
    (error "bad identifier-is-primcall call"))
  (if (identifier? id)
      (if (null? rest)
          (eq? 'primcall (identifier-kind id))
          (and (eq? 'primcall (identifier-kind id))
               (eq? (car rest) (identifier-meaning id))))
      #f))

(define (identifier-is-aux id . rest)
  (when (and (pair? rest)
             (> (length rest) 1))
    (error "bad identifier-is-aux call"))
  (if (identifier? id)
      (if (null? rest)
          (eq? 'aux (identifier-kind id))
          (and (eq? 'aux (identifier-kind id))
               (eq? (car rest) (identifier-meaning id))))
      #f))

(define-record-type <pp-environment>
  (make-pp-environment parent)
  pp-environment?
  (parent pp-environment-parent)
  (bindings pp-environment-bindings pp-environment-bindings-set!)
  (counter pp-environment-counter pp-environment-counter-set!))

(define (pp-environment-next-counter env)
  (let loop ((root env))
    (if (pp-environment-parent root)
        (loop (pp-environment-parent root))
        (let ((counter (+ 1 (pp-environment-counter root))))
          (pp-environment-counter-set! root counter)
          counter))))

(define (pp-environment-lookup env sym)
  (let loop ((rest (pp-environment-bindings env)))
    (if (null? rest)
        (if (pp-environment-parent env)
            (pp-environment-lookup (pp-environment-parent env) sym)
            #f)
        (if (eq? sym (identifier-name (car rest)))
            (car rest)
            (loop (cdr rest))))))

(define (pp-environment-add-var env id)
  (pp-environment-bindings-set! env (cons id (pp-environment-bindings env))))

(define (pp-environment parent)
  (let ((env (make-pp-environment parent)))
    (pp-environment-counter-set! env 0)
    (pp-environment-bindings-set! env '())
    env))

(define (preprocess-create-environment root-identifiers)
  (let ((env (pp-environment #f)))
    (let loop ((root-identifiers root-identifiers))
      (unless (null? root-identifiers)
        (pp-environment-add-var env (car root-identifiers))
        (loop (cdr root-identifiers))))
    env))

;; you can call "preprocess" with root-identifiers, or create an
;; environment using preprocess-create-environment and then call
;; "preprocess-form"
(define (preprocess form root-identifiers)
  (let ((env (pp-environment #f)))
    (let loop ((root-identifiers root-identifiers))
      (unless (null? root-identifiers)
        (pp-environment-add-var env (car root-identifiers))
        (loop (cdr root-identifiers))))
    (preprocess-form env form)))

(define (preprocess-form env form)
  (cond ((symbol? form) (let ((m (pp-environment-lookup env form)))
                          (if m m (identifier 'global form form))))
        ((null? form) '())
        ((pair? form) (preprocess-list env form))
        ((vector? form) (preprocess-vector env form))
        (else form)))

(define (preprocess-vector env form)
  (let loop ((i 0) (veclen (vector-length form)))
    (if (= i veclen)
        form
        (begin
          (vector-set! form i (preprocess-form env (vector-ref form i)))
          (loop (+ i 1) veclen)))))

(define (preprocess-list env form)
  (let ((m (pp-environment-lookup env (car form))))
    ;; process forms that create bindings differently
    (cond ((identifier-is-special m 'define) (preprocess-define env form))
          ((identifier-is-special m 'define-syntax) (preprocess-define-syntax env form))
          ((identifier-is-special m 'lambda) (preprocess-lambda env form))
          ((identifier-is-special m 'let) (preprocess-let env form))
          (else (preprocess-list-items env form)))))

(define (preprocess-let env form)
  (set-car! form (identifier 'special (car form) 'let))
  (if (or (< (length form) 3)
          (and (symbol? (cadr form))
               (< (length form) 4)))
      (preprocess-list-items env form)
      (begin
        (when (symbol? (cadr form))
          (let ((id (identifier 'local (cadr form) (pp-unique-name (cadr form) env))))
            (set-car! (cdr form) id)))
        (let ((bindings (if (identifier? (cadr form))
                            (caddr form)
                            (cadr form)))
              (body (if (identifier? (cadr form))
                        (cdddr form)
                        (cddr form))))
          (let ((env (preprocess-let-bindings env bindings (if (identifier? (cadr form)) (cadr form) #f))))
            (preprocess-list-items env body))
          form))))

(define (pp-unique-name name env)
  (let ((name (format "#~s_~a" name (pp-environment-next-counter env))))
    (string->symbol name)))

(define (preprocess-let-bindings env bindings self-name)
  (let loop ((bindings bindings) (new-env (pp-environment env)))
    (when self-name
      (pp-environment-add-var new-env self-name))
    (if (null? bindings)
        new-env
        (let ((binding (car bindings)))
          (when (!= 2 (length binding))
            (compile-error "bad let binding: ~s" binding))
          (when (not (symbol? (car binding)))
            (compile-error "bad let variable: ~s" (car binding)))
          (let ((id (identifier 'local (car binding) (pp-unique-name (car binding) env))))
            (pp-environment-add-var new-env id)
            (set-car! binding id)
            (set-car! (cdr binding) (preprocess-form env (cadr binding)))
            (loop (cdr bindings) new-env))))))

(define (preprocess-lambda env form)
  (set-car! form (identifier 'special (car form) 'lambda))
  (if (< (length form) 3)
      (preprocess-list-items env form)
      (let ((params (cadr form))
            (body (cddr form)))
        (let ((env (preprocess-lambda-params env params (lambda (id)
                                                          (set-car! (cdr form) id)))))
          (preprocess-list-items env body))
        form)))

(define (preprocess-lambda-params env params replace-whole)
  (cond ((symbol? params) (let ((env (pp-environment env)))
                            (let ((id (identifier 'local params (pp-unique-name params env))))
                              (pp-environment-add-var env id)
                              (replace-whole id)
                              env)))
        ((null? params) env)
        ((pair? params) (let loop ((params params) (env (pp-environment env)))
                          (unless (symbol? (car params))
                            (compile-error "bad lambda parameter: ~s" (car params)))
                          (let ((id (identifier 'local (car params) (pp-unique-name (car params) env))))
                            (pp-environment-add-var env id)
                            (set-car! params id))
                          (cond ((symbol? (cdr params)) (let ((id (identifier 'local (cdr params) (pp-unique-name (cdr params) env))))
                                                          (pp-environment-add-var env id)
                                                          (set-cdr! params id)
                                                          env))
                                ((null? (cdr params)) env)
                                ((pair? (cdr params)) (loop (cdr params) env))
                                (else (compile-error "bad lambda parameter: ~s" (cdr params))))))
        (else (compile-error "bad parameter list for lambda: ~s" params))))

(define (preprocess-define env form)
  (set-car! form (identifier 'special (car form) 'define))
  (cond ((< (length form) 2)
         (preprocess-list-items env form))
        ((not (or (symbol? (cadr form))
                  (pair? (cadr form))))
         (preprocess-list-items env form))
        ((and (pair? (cadr form))
              (< (length form) 3))
         (preprocess-list-items env form))
        ((and (symbol? (cadr form))
              (and (!= (length form) 3) (!= (length form) 2)))
         (preprocess-list-items env form))
        (else
         (if (symbol? (cadr form))
             (begin
               (set-car! (cdr form) (identifier 'global (cadr form) (cadr form)))
               (unless (null? (cddr form))
                 (set-car! (cddr form) (preprocess-form env (caddr form))))
               form)
             (begin
               (if (symbol? (caadr form))
                   (begin
                     (set-car! (cadr form) (identifier 'global (caadr form) (caadr form)))
                     (let ((env (preprocess-lambda-params env (cdadr form) (lambda (id)
                                                                             (set-cdr! (cadr form) id)))))
                       (preprocess-list-items env (cddr form))
                       form))
                   (compile-error "bad define form: ~s" form)))))))

(define (preprocess-define-syntax env form)
  (cond ((!= (length form) 3)
         (preprocess-list-items env form))
        ((not (symbol? (cadr form)))
         (preprocess-list-items env form))
        (else
         (set-car! form (identifier 'special (car form) 'define-syntax))
         (set-car! (cdr form) (identifier 'global (cadr form) (cadr form)))
         (preprocess-form env (caddr form))
         form)))

(define (preprocess-list-items env form)
  (let loop ((p form))
    (set-car! p (preprocess-form env (car p)))
    (if (pair? (cdr p))
        (loop (cdr p))
        (begin
          (set-cdr! p (preprocess-form env (cdr p)))
          form))))
