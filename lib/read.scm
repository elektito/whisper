;; just a unique value to represent a single dot which could be used to
;; specify pairs/dotted lists.
(define *dot* (gensym "dot"))

(define (read-error fmt . args)
  (error (apply format fmt args)))

(define read
  (case-lambda
   (() (read (current-input-port)))
   ((port)
    (skip-whitespace-and-comments port)
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) ch)
            ((char=? #\( ch) (read-list port))
            ((char=? #\" ch) (read-string-literal port))
            ((char=? #\# ch) (read-sharp-thing port))
            ((char=? #\' ch) (read-quoted-form port))
            ((char=? #\` ch) (read-quasiquoted-form port))
            ((char=? #\, ch) (read-unquoted-form port))
            ((char=? #\| ch) (read-piped-symbol port))
            ((char=? #\. ch) (read-dot-or-identifier port))
            ((char=? #\) ch) (read-error "extra closing parenthesis"))
            (else (read-char port) ; read-identifier-or-number expects first character already read and passed to it
                  (read-identifier-or-number port ch)))))))

(define (skip-whitespace-and-comments port)
  (let loop ((ch (peek-char port)))
    (cond ((char-whitespace? ch) (read-char port) (loop (peek-char port)))
          ((char=? #\; ch) (skip-line-comment port) (loop (peek-char port)))
          ((char=? #\# ch) (read-char port)
                           (let ((next-char (peek-char port)))
                             (case next-char
                               ((#\;) (read-char port)
                                      (read port)
                                      (loop (peek-char port))) ; read and ignore one datum
                               ((#\|) (read-char port)
                                      (skip-block-comment port)
                                      (loop (peek-char port)))
                               ;; read the last character so we can
                               ;; unread the two in the right order.
                               ;; notice that two "unread" operations is
                               ;; not guaranteed to be supported on all
                               ;; platforms.
                               (else (read-char port)
                                     (unread-char next-char port)
                                     (unread-char ch port)))))
          (else (void)))))

(define (skip-line-comment port)
  (read-char port) ; skip the semicolon character
  (let loop ((ch (read-char port)))
    (cond ((eof-object? ch) ch)
          ((eq? #\newline ch) ch)
          (else (loop (read-char port))))))

(define (skip-block-comment port)
  (let loop ((depth 1))
    (if (zero? depth)
        (void)
        (let ((ch (read-char port)))
          (cond ((eof-object? ch) (read-error "unterminated block comment"))
                ((and (char=? ch #\#) (char=? (peek-char port) #\|))
                 (read-char port) (loop (+ depth 1)))
                ((and (char=? ch #\|) (char=? (peek-char port) #\#))
                 (read-char port) (loop (- depth 1)))
                (else (loop depth)))))))

(define (read-quoted-form port)
  (read-char port) ; skip the quote character
  (let ((form (read port)))
    (cons 'quote (cons form '()))))

(define (read-quasiquoted-form port)
  (read-char port) ; skip the quasiquote character
  (let ((form (read port)))
    (cons 'quasiquote (cons form '()))))

(define (read-unquoted-form port)
  (read-char port) ; skip the unquote (comma) character
  (let ((unquote (if (char=? #\@ (peek-char port))
                     (begin
                       (read-char port)
                       'unquote-splicing)
                     'unquote)))
    (let ((form (read port)))
      (cons unquote (cons form '())))))

(define (check-for-stray-dot ls)
  (let loop ((l ls))
    (if (not (pair? l))
        ls
        (if (eq? *dot* (car l))
            (read-error "unexpected dot (.)")
            (loop (cdr l))))))

(define (postprocess-list ls)
  ;; receives a list that has just been read, and does two things on it:
  ;; 1) if there is a dot in it in the correct position, converts it to
  ;; a dotted list. 2) reverses the list (since it's initially read in
  ;; reverse order).
  (if (or (null? ls) (null? (cdr ls)))
      (check-for-stray-dot ls)
      (let ((tail (if (eq? *dot* (cadr ls))
                      (car ls)
                      '()))
            (rest (if (eq? *dot* (cadr ls))
                      (cddr ls)
                      ls)))
        (check-for-stray-dot (append (reverse rest) tail)))))

(define (read-list port)
  (read-char port) ; get rid of the open parenthesis
  (skip-whitespace-and-comments port)
  (let loop ((ch (peek-char port))
             (ls '()))
    (cond ((eof-object? ch) (read-error "eof inside list"))
          ((char=? #\) ch) (read-char port) (postprocess-list ls))
          (else (let ((item (read port)))
                  (if (eof-object? item)
                      (read-error "eof inside list")
                      (begin
                        (skip-whitespace-and-comments port)
                        (loop (peek-char port) (cons item ls)))))))))

(define (read-string-literal port)
  (read-char port) ; get rid of open quotation
  (let loop ((ch (peek-char port))
             (s ""))
    (read-char port)
    (cond ((eof-object? ch) (read-error "eof in string"))
          ((char=? #\" ch) s)
          ((char=? #\\ ch) (let ((escaped-char (read-escaped-char port)))
                             (loop (peek-char port) (string-append-char s escaped-char))))
          (else (let ((s (string-append-char s ch)))
                  (loop (peek-char port) s))))))

(define (read-piped-symbol port)
  (read-char port) ; get rid of initial pipe
  (let loop ((ch (peek-char port))
             (s ""))
    (read-char port)
    (cond ((eof-object? ch) (read-error "eof in piped symbol"))
          ((char=? #\| ch) (string->symbol s))
          ((char=? #\\ ch) (let ((escaped-char (read-escaped-char port)))
                             (loop (peek-char port) (string-append-char s escaped-char))))
          (else (let ((s (string-append s (make-string 1 ch))))
                  (loop (peek-char port) s))))))

(define (read-escaped-char port)
  ;; note: the backslash is already read
  (let ((ch (peek-char port)))
    (read-char port)
    (case ch
      ((#\a) #\alarm)
      ((#\b) #\backspace)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\t) #\tab)
      ((#\") #\")
      ((#\|) #\|)
      ((#\\) #\\)
      ((#\x) (read-escaped-hex-char port))
      (else (read-error "bad escape sequence")))))

(define (char-is-hex-digit? ch)
  (or (and (char>=? ch #\0) (char<=? ch #\9))
      (and (char>=? ch #\a) (char<=? ch #\f))
      (and (char>=? ch #\A) (char<=? ch #\F))))

(define (read-escaped-hex-char port)
  ;; reads a character literal like \x22;
  ;; assumes \x is already read
  (let loop ((ch (read-char port)) (s ""))
    (cond ((eof-object? ch) (read-error "eof inside escape sequence"))
          ((char-is-hex-digit? ch)
           (loop (read-char port) (string-append-char s ch)))
          ((char=? #\; ch) (let ((n (string->number s 16)))
                             (if n
                                 (integer->char n)
                                 (read-error "bad hex code: ~a" s))))
          (else (read-error "bad character in hex escape code: ~a" ch)))))

(define (sym-or-num s)
  (let ((n (string->number s)))
    (if (not n)
        (string->symbol s)
        n)))

(define (char-is-separator? ch)
  (or (char-whitespace? ch)
      (char=? #\' ch)
      (char=? #\( ch)
      (char=? #\) ch)))

(define (read-dot-or-identifier port)
  (read-char port) ; skip the dot
  (let ((ch (peek-char port)))
    (if (or (eof-object? ch)
            (char-is-separator? ch))
        *dot*
        (read-identifier-or-number port #\.))))

(define (read-identifier-or-number port first-char)
  (let loop ((first-iter #t) (ch first-char) (s ""))
    (cond ((char-is-separator? ch) (sym-or-num s))
          ((eq? #\\ ch) (unless first-iter (read-char port))
                        (let ((escaped-char (read-escaped-char port)))
                          (loop #f (peek-char port) (string-append-char s escaped-char))))
          (else (unless first-iter
                  (read-char port))
                (loop #f (peek-char port) (string-append-char s ch))))))

(define (read-sharp-thing port)
  (read-char port) ; skip the sharp
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) (read-error "unexpected eof after sharp"))
          ((char=? #\\ ch) (read-char-literal port))
          ((char=? #\( ch) (let ((ls (read-list port)))
                             (if (not (list? ls))
                                 (read-error "bad vector literal: #~s" ls)
                                 (list->vector ls))))
          (else (read-sharp-identifier port)))))

(define (read-sharp-identifier port)
  (let loop ((ch (peek-char port))
             (s "#"))
    (cond ((or (eof-object? ch)
               (char-whitespace? ch)
               (char=? #\( ch)
               (char=? #\) ch)
               (char=? #\' ch))
           (cond ((string=? "#f" s) #f)
                 ((string=? "#t" s) #t)
                 (else (string->symbol s))))
          (else (read-char port)
                (loop (peek-char port) (string-append s (make-string 1 ch)))))))

(define (str->char s)
  (cond ((string=? "alarm" s) #\alarm)
        ((string=? "backspace" s) #\backspace)
        ((string=? "delete" s) #\delete)
        ((string=? "escape" s) #\escape)
        ((string=? "newline" s) #\newline)
        ((string=? "null" s) #\null)
        ((string=? "return" s) #\return)
        ((string=? "space" s) #\space)
        ((string=? "tab" s) #\tab)
        ((= 1 (string-length s)) (string-ref s 0))
        ((eq? (string-ref s 0) #\x) (integer->char (string->number (substring s 1 (string-length s)) 16)))
        (else (read-error "invalid character literal"))))

(define (read-char-literal port)
  (read-char port) ; skip the backslash
  (when (eof-object? (peek-char port))
    (read-error "eof inside character literal"))
  (let ((s (make-string 1 (read-char port))))
    (let loop ((ch (peek-char port))
               (s s))
      (if (or (eof-object? ch)
              (char-whitespace? ch)
              (char=? #\( ch)
              (char=? #\) ch)
              (char=? #\' ch))
          (str->char s)
          (begin
            (read-char port)
            (loop (peek-char port) (string-append s (make-string 1 ch))))))))
