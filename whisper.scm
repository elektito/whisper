(include "utils.scm")
(include "format.scm")

(define *indent-size* 4)

;;;;;; reader ;;;;;;

(define (read port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) ch)
          ((char-whitespace? ch) (read-char port) (read port))
          ((char=? #\( ch) (read-list port))
          ((char=? #\" ch) (read-string-literal port))
          ((char=? #\# ch) (read-sharp-thing port))
          ((char=? #\' ch) (read-quoted-form port))
          ((char=? #\; ch) (skip-line-comment port) (read port))
          (else (read-identifier-or-number port)))))

(define (skip-line-comment port)
  (read-char port) ; skip the semicolon character
  (let loop ((ch (read-char port)))
    (cond ((eof-object? ch) ch)
          ((eq? #\newline ch) ch)
          (else (loop (read-char port))))))

(define (read-quoted-form port)
  (read-char port) ; skip the quote character
  (let ((form (read port)))
    (cons 'quote (cons form '()))))

(define (read-list port)
  (read-char port) ; get rid of the open parenthesis
  (let loop ((ch (peek-char port))
             (ls '()))
    (cond ((eof-object? ch) (compile-error "eof inside list"))
          ((char-whitespace? ch) (read-char port) (loop (peek-char port) ls))
          ((char=? #\) ch) (read-char port) (reverse ls))
          (else (let ((item (read port)))
                  (if (eof-object? item)
                      (compile-error "eof inside list")
                      (loop (peek-char port) (cons item ls))))))))

(define (read-string-literal port)
  (read-char port) ; get rid of open quotation
  (let loop ((ch (peek-char port))
             (s ""))
    (read-char port)
    (cond ((eof-object? ch) (compile-error "eof in string"))
          ((char=? #\" ch) s)
          (else (let ((s (string-append s (make-string 1 ch))))
                  (loop (peek-char port) s))))))

(define (sym-or-num s)
  (let ((n (string->number s)))
    (if (char=? n #f)
        (string->symbol s)
        n)))

(define (read-identifier-or-number port)
  (let loop ((ch (peek-char port))
             (s ""))
    (if (or (char-whitespace? ch)
            (char=? #\' ch)
            (char=? #\( ch)
            (char=? #\) ch))
        (sym-or-num s)
        (let ((s (string-append s (make-string 1 ch))))
          (read-char port)
          (loop (peek-char port) s)))))

(define (read-sharp-thing port)
  (read-char port) ; skip the sharp
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) (compile-error "unexpected eof after sharp"))
          ((char=? #\\ ch) (read-char-literal port))
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
        (else (compile-error "invalid character literal"))))

(define (read-char-literal port)
  (read-char port) ; skip the backslash
  (if (eof-object? (peek-char port))
      (compile-error "eof inside character literal"))
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

;;;;;; compiler ;;;;;;

(define (create-program port)
  (list port
        '() ; funcs
        0   ; funcnum (function counter)
        ))

(define (program-port program)
  (car program))

(define (program-funcs program)
  (cadr program))

(define (program-next-funcnum func)
  (let ((n (caddr func)))
    (list-set! func 2 (+ n 1))
    n))

(define (gen-func-prototypes program output)
  (let loop ((funcs (program-funcs program)))
    (if (not (null? funcs))
        (begin
          (format output "static value ~a(environment env, int nargs, ...);\n" (func-name (car funcs)))
          (loop (cdr funcs))))))

(define (gen-func-bodies program output)
  (let loop ((funcs (program-funcs program)))
    (if (not (null? funcs))
        (begin
          (format output "static value ~a(environment env, int nargs, ...) {\n" (func-name (car funcs)))
          (display (get-output-string (func-port (car funcs))) output)
          (format output "}\n")
          (format output "\n")
          (loop (cdr funcs))))))

(define (output-program-code program)
  (let ((port (open-output-file "b.c")))
    (display "#include \"core.h\"\n\n" port)
    (gen-func-prototypes program port)
    (newline port)
    (gen-func-bodies program port)
    (newline port)
    (display "int main(int argc, char *argv[]) {\n" port)
    (display "    f0(NULL, 0);\n" port)
    (display "}\n" port)
    (void)))

(define (add-function program parent nargs has-rest)
  (let ((func (list (open-output-string) ; port
                    0                    ; varnum
                    (format "f~a" (program-next-funcnum program)) ; name
                    program
                    parent
                    nargs
                    has-rest)))
    (list-set! program 1 (cons func (program-funcs program)))
    func))

(define (func-port func)
  (car func))

(define (func-next-varnum func)
  (let ((n (cadr func)))
    (list-set! func 1 (+ n 1))
    n))

(define (func-name func)
  (list-ref func 2))

(define (gen-code func indent fmt . args)
  (let ((port (func-port func)))
    (display (make-string (* indent *indent-size*) #\space) port)
    (%format port fmt args)))

(define (compile-number func indent form)
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = FIXNUM(~a);\n" varnum form)))

(define (compile-string func indent form)
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = make_string(~s, ~a);\n" varnum form (string-length form))))

(define (compile-bool func indent form)
  (let ((varnum (func-next-varnum func)))
    (if form
        (gen-code func indent "value x~a = TRUE;\n" varnum form)
        (gen-code func indent "value x~a = FALSE;\n" varnum form))))

(define (c-char ch)
  ;; output the c literal form of a character (minus the single quotes).
  ;; #\a => "a"
  ;; #\newline => "\\n"
  ;; #\delete => "\\x7f"
  (cond ((eq? ch #\alarm) "\\a")
        ((eq? ch #\backspace) "\\b")
        ((eq? ch #\newline) "\\n")
        ((eq? ch #\return) "\\r")
        ((eq? ch #\tab) "\\t")
        ((eq? ch #\\) "\\\\")
        ((eq? ch #\x00) "\\0")
        ((eq? ch #\') "\\'")
        ((and (>= (char->integer ch) 32)
              (< (char->integer ch) 127))
         (make-string 1 ch))
        (else (if (< (char->integer ch) 10)
                  (format "\\x0~a" (char->integer ch))
                  (format "\\x~x" (char->integer ch))))))

(define (compile-char func indent form)
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = CHAR('~a');\n" varnum (c-char form))))

(define (compile-form func indent form)
  (cond ((number? form) (compile-number func indent form))
        ((string? form) (compile-string func indent form))
        ((boolean? form) (compile-bool func indent form))
        ((char? form) (compile-char func indent form))
        (else (compile-error "don't know how to compile form: ~s" form))))

(define (compile-error fmt . args)
  (%format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (compile-program program)
  (let ((func (add-function program #f 0 0))
        (port (program-port program)))
    (let loop ((form (read port)))
      (if (eof-object? form)
          func
          (begin
            (compile-form func 1 form)
            (loop (read port)))))))

;;;;;; main ;;;;;;

(if (not (= 2 (length (command-line))))
    (compile-error "you need to pass the input file as the only argument"))

(let ((port (open-input-file (cadr (command-line)))))
  (let ((program (create-program port)))
    (compile-program program)
    (output-program-code program)))
