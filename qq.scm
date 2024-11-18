;; This code is based on the backquote implementation by Guy L. Steele
;; Jr., in Appendix C of "Common Lisp the Language, 2nd Edition". You
;; can find a copy of the code here.
;;
;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node367.html
;;
;; The original code is in the public domain, as attested by the comment
;; at the beginning of the code.

(define *comma* 'unquote)
(define *comma-atsign* 'unquote-splicing)

(define *qq-list* (gensym "QQ-LIST"))
(define *qq-append* (gensym "QQ-APPEND"))
(define *qq-list** (gensym "QQ-LIST*"))
(define *qq-quote* (gensym "QQ-QUOTE"))
(define *qq-quote-nil* (list *qq-quote* '()))

(define *qq-simplify* #t)

;;; receives the argument to quasiquote and returns the processed form
(define (qq-quasiquote x)
  (qq-completely-process x))

(define (qq-completely-process x)
  (let ((raw-result (qq-process x)))
    (qq-remove-tokens (if *qq-simplify*
                          (qq-simplify raw-result)
                          raw-result))))

(define (qq-process x)
  (cond ((atom? x)
         (list *qq-quote* x))
        ((eq? (cl-car x) 'quasiquote)
         (qq-process (qq-completely-process (cl-cadr x))))
        ((eq? (cl-car x) *comma*) (cl-cadr x))
        ((eq? (cl-car x) *comma-atsign*)
         (error (format ",@~S after `" (cl-cadr x))))
        (else (let loop ((p x) (q '()))
                (if (atom? p)
                    (cons *qq-append*
                          (nreconc q (list (list *qq-quote* p))))
                    (if (eq? (cl-car p) *comma*)
                        (if (not (null? (cl-cddr p)))
                            (error (format "Malformed ,~S" p))
                            (cons *qq-append*
                                  (nreconc q (list (cl-cadr p)))))
                        (if (eq? (cl-car p) *comma-atsign*)
                            (error (format "Dotted ,@~S" p))
                            (loop (cl-cdr p) (cons (bracket (cl-car p)) q)))))))))

(define (bracket x)
  (cond ((atom? x)
         (list *qq-list* (qq-process x)))
        ((eq? (cl-car x) *comma*)
         (list *qq-list* (cl-cadr x)))
        ((eq? (cl-car x) *comma-atsign*)
         (cl-cadr x))
        (else (list *qq-list* (qq-process x)))))

;;; This auxiliary function is like MAPCAR but has two extra purposes:
;;; (1) it handles dotted lists; (2) it tries to make the result share
;;; with the argument x as much as possible.

(define (maptree fn x)
  (if (atom? x)
      (fn x)
      (let ((a (fn (cl-car x)))
            (d (maptree fn (cl-cdr x))))
        (if (and (eqv? a (cl-car x)) (eqv? d (cl-cdr x)))
            x
            (cons a d)))))

;;; This predicate is true of a form that when read looked like ,@foo

(define (qq-splicing-frob? x)
  (and (pair? x)
       (eq? (cl-car x) *comma-atsign*)))


;;; This predicate is true of a form that when read looked like ,@foo or
;;; just plain ,foo.

(define (qq-frob? x)
  (and (pair? x)
       (or (eq? (cl-car x) *comma*)
           (eq? (cl-car x) *comma-atsign*))))

;;; The simplifier essentially looks for calls to #:QQ-APPEND and tries
;;; to simplify them. The arguments to #:QQ-APPEND are processed from
;;; right to left, building up a replacement form. At each step a number
;;; of special cases are handled that, loosely speaking, look like this:
;;;
;;;  (APPEND (LIST a b c) foo) => (LIST* a b c foo)
;;;       provided a, b, c are not splicing frobs
;;;  (APPEND (LIST* a b c) foo) => (LIST* a b (APPEND c foo))
;;;       provided a, b, c are not splicing frobs
;;;  (APPEND (QUOTE (x)) foo) => (LIST* (QUOTE x) foo)

(define (qq-simplify x)
  (if (atom? x)
      x
      (let ((x (if (eq? (cl-car x) *qq-quote*)
                   x
                   (maptree qq-simplify x))))
        (if (not (eq? (cl-car x) *qq-append*))
            x
            (qq-simplify-args x)))))

(define (qq-simplify-args x)
  (let loop ((args (reverse (cl-cdr x)))
             (result '()))
    (if (null? args)
        result
        (loop (cl-cdr args)
              (cond ((atom? (cl-car args))
                     (qq-attach-append *qq-append* (cl-car args) result))
                    ((and (eq? (cl-caar args) *qq-list*)
                          (not-any? qq-splicing-frob? (cl-cdar args)))
                     (qq-attach-conses (cl-cdar args) result))
                    ((and (eq? (cl-caar args) *qq-list**)
                          (not-any? qq-splicing-frob? (cl-cdar args)))
                     (qq-attach-conses
                      (reverse (cl-cdr (reverse (cl-cdar args))))
                      (qq-attach-append *qq-append*
                                        (cl-car (cl-last (cl-car args)))
                                        result)))
                    ((and (eq? (cl-caar args) *qq-quote*)
                          (pair? (cl-cadar args))
                          (not (qq-frob? (cl-cadar args)))
                          (null? (cl-cddar args)))
                     (qq-attach-conses (list (list *qq-quote*
                                                   (cl-caadar args)))
                                       result))
                    (else (qq-attach-append *qq-append*
                                            (cl-car args)
                                            result)))))))
(define (null-or-quoted? x)
  (or (null? x)
      (and (pair? x)
           (eq? (cl-car x) *qq-quote*))))

;;; When QQ-ATTACH-APPEND is called, the OP should be #:QQ-APPEND. This
;;; produces a form (op item result) but some simplifications are done
;;; on the fly:
;;;
;;;  (op '(a b c) '(d e f g)) => '(a b c d e f g)
;;;  (op item 'nil) => item, provided item is not a splicable frob
;;;  (op item 'nil) => (op item), if item is a splicable frob
;;;  (op item (op a b c)) => (op item a b c)

(define (qq-attach-append op item result)
  (cond ((and (null-or-quoted? item) (null-or-quoted? result))
         (list *qq-quote* (append (cl-cadr item) (cl-cadr result))))
        ((or (null? result) (equal? result *qq-quote-nil*))
         (if (qq-splicing-frob? item) (list op item) item))
        ((and (pair? result) (eq? (cl-car result) op))
         (list* (cl-car result) item (cl-cdr result)))
        (else (list op item result))))

;;; The effect of QQ-ATTACH-CONSES is to produce a form as if by
;;; `(LIST* ,@items ,result) but some simplifications are done
;;; on the fly.
;;;
;;;  (LIST* 'a 'b 'c 'd) => '(a b c . d)
;;;  (LIST* a b c 'nil) => (LIST a b c)
;;;  (LIST* a b c (LIST* d e f g)) => (LIST* a b c d e f g)
;;;  (LIST* a b c (LIST d e f g)) => (LIST a b c d e f g)

(define (qq-attach-conses items result)
  (cond ((and (every? null-or-quoted? items)
              (null-or-quoted? result))
         (list *qq-quote*
               (append (mapcar cadr items) (cl-cadr result))))
        ((or (null? result) (equal? result *qq-quote-nil*))
         (cons *qq-list* items))
        ((and (pair? result)
              (or (eq? (cl-car result) *qq-list*)
                  (eq? (cl-car result) *qq-list**)))
         (cons (cl-car result) (append items (cl-cdr result))))
        (else (cons *qq-list** (append items (list result))))))

;;; Removes funny tokens and changes (#:QQ-LIST* a b) into
;;; (CONS a b) instead of (LIST* a b), purely for readability.

(define (qq-remove-tokens x)
  (cond ((eq? x *qq-list*) 'list)
        ((eq? x *qq-append*) 'append)
        ((eq? x *qq-list**) 'list*)
        ((eq? x *qq-quote*) 'quote)
        ((atom? x) x)
        ((and (eq? (cl-car x) *qq-list**)
              (pair? (cl-cddr x))
              (null? (cl-cdddr x)))
         (cons 'cons (maptree qq-remove-tokens (cl-cdr x))))
        (else (maptree qq-remove-tokens x))))

;;; define CL variants of car, cdr, and co

(define (cl-car x)
  (if (null? x) x (car x)))

(define (cl-cdr x)
  (if (null? x) x (cdr x)))

(define (cl-caar x)
  (cl-car (cl-car x)))

(define (cl-cadr x)
  (cl-car (cl-cdr x)))

(define (cl-cdar x)
  (cl-cdr (cl-car x)))

(define (cl-cddr x)
  (cl-cdr (cl-cdr x)))

(define (cl-cadar x)
  (cl-car (cl-cdr (cl-car x))))

(define (cl-cdddr x)
  (cl-cdr (cl-cdr (cl-cdr x))))

(define (cl-cddar x)
  (cl-cdr (cl-cdr (cl-car x))))

(define (cl-caadar x)
  (cl-car (cl-car (cl-cdr (cl-car x)))))

;;; define other utility functions

(define (nreconc list1 list2)
  (if (null? list1)
      list2
      ;; here we could (should?) use a destructive variant of reverse
      ;; (reverse!) to be like nreconc
      (let ((rev-list1 (reverse list1)))
        (set-cdr! (last-pair rev-list1) list2)
        rev-list1)))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(define (not-any? pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          #f
          (not-any? pred (cdr lst)))))

(define (every? pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          (every? pred (cdr lst))
          #f)))

;; very similar to last-pair above, but works exactly like in CL
(define (cl-last x)
  (if (null? x)
      x
      (if (null? (cdr x))
          x
          (last (cdr x)))))
