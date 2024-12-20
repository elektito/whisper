(include "utils.scm")
(include "format.scm")
(include "qq.scm")

(define *indent-size* 4)

;; just a unique value to represent a single dot which could be used to
;; specify pairs/dotted lists.
(define *dot* (gensym "dot"))

;;;;;; reader ;;;;;;

(define (read port)
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
          (else (read-char port) ; read-identifier-or-number expects first character already read and passed to it
                (read-identifier-or-number port ch)))))

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
  (compile-error "block comments not yet supported"))

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
            (compile-error "unexpected dot (.)")
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
    (cond ((eof-object? ch) (compile-error "eof inside list"))
          ((char=? #\) ch) (read-char port) (postprocess-list ls))
          (else (let ((item (read port)))
                  (if (eof-object? item)
                      (compile-error "eof inside list")
                      (begin
                        (skip-whitespace-and-comments port)
                        (loop (peek-char port) (cons item ls)))))))))

(define (read-string-literal port)
  (read-char port) ; get rid of open quotation
  (let loop ((ch (peek-char port))
             (s ""))
    (read-char port)
    (cond ((eof-object? ch) (compile-error "eof in string"))
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
    (cond ((eof-object? ch) (compile-error "eof in piped symbol"))
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
      (else (compile-error "bad escape sequence")))))

(define (char-is-hex-digit? ch)
  (or (and (char>=? ch #\0) (char<=? ch #\9))
      (and (char>=? ch #\a) (char<=? ch #\f))
      (and (char>=? ch #\A) (char<=? ch #\F))))

(define (read-escaped-hex-char port)
  ;; reads a character literal like \x22;
  ;; assumes \x is already read
  (let loop ((ch (read-char port)) (s ""))
    (cond ((eof-object? ch) (compile-error "eof inside escape sequence"))
          ((char-is-hex-digit? ch)
           (loop (read-char port) (string-append-char s ch)))
          ((char=? #\; ch) (let ((n (string->number s 16)))
                             (if n
                                 (integer->char n)
                                 (compile-error "bad hex code: ~a" s))))
          (else (compile-error "bad character in hex escape code: ~a" ch)))))

(define (sym-or-num s)
  (let ((n (string->number s)))
    (if (char=? n #f)
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
          ((eq? #\\ ch) (if (not first-iter) (read-char port))
                        (let ((escaped-char (read-escaped-char port)))
                          (loop #f (peek-char port) (string-append-char s escaped-char))))
          (else (if (not first-iter)
                    (read-char port))
                (loop #f (peek-char port) (string-append-char s ch))))))

(define (read-sharp-thing port)
  (read-char port) ; skip the sharp
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) (compile-error "unexpected eof after sharp"))
          ((char=? #\\ ch) (read-char-literal port))
          ((char=? #\( ch) (let ((ls (read-list port)))
                             (if (not (list? ls))
                                 (compile-error "bad vector literal: #~s" ls)
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

(define-record-type <program>
  (make-program ports funcs funcnum interned-symbols init-func referenced-vars is-test-suite test-counter debug)
  program?
  (ports program-ports program-ports-set!)
  (funcs program-funcs program-funcs-set!)
  (funcnum program-funcnum program-funcnum-set!)
  (interned-symbols program-symbols program-symbols-set!)
  (init-func program-init-func program-init-func-set!)
  (referenced-vars program-referenced-vars program-referenced-vars-set!)
  (is-test-suite program-is-test-suite program-is-test-suite-set!)
  (test-counter program-test-counter program-test-counter-set!)
  (debug program-debug program-debug-set!))

(define (create-program port)
  (make-program (list port)
                '() ; funcs
                0   ; funcnum (function counter)
                '() ; interned symbols
                #f  ; init func, to be set later
                '() ; referenced variables
                #f  ; is test suite?
                0   ; test counter
                #f  ; debug instrumentation
                ))

(define (program-port program)
  (car (program-ports program)))

(define (program-is-main-file program)
  ;; returns true if we are not inside an included file.
  ;; we check if there's only one input port.
  (null? (cdr (program-ports program))))

(define (program-push-port program port)
  (let ((ports (program-ports program)))
    (program-ports-set! program (cons port ports))))

(define (program-pop-port program)
  (let ((ports (program-ports program)))
    (program-ports-set! program (cdr ports))))

(define (program-add-function program func)
  (program-funcs-set! program (cons func (program-funcs program))))

(define (program-next-funcnum program)
  (let ((n (program-funcnum program)))
    (program-funcnum-set! program (+ n 1))
    n))

(define (program-test-counter-inc! program)
  (let ((c (+ 1 (program-test-counter program))))
    (program-test-counter-set! program c)
    c))

(define (program-add-referenced-var program var)
  (let loop ((vars (program-referenced-vars program)))
    (if (null? vars)
        (program-referenced-vars-set! program (cons var (program-referenced-vars program)))
        (if (not (eq? var (car vars)))
            (loop (cdr vars))))))

(define (gen-func-prototypes program output)
  (let loop ((funcs (program-funcs program)))
    (if (not (null? funcs))
        (begin
          (format output "static value ~a(environment env, enum call_flags flags, int nargs, ...);\n" (func-name (car funcs)))
          (loop (cdr funcs))))))

(define (gen-func-bodies program output)
  (let loop ((funcs (program-funcs program)))
    (if (not (null? funcs))
        (begin
          (format output "static value ~a(environment env, enum call_flags flags, int nargs, ...) {\n" (func-name (car funcs)))
          (if (program-debug program)
              (format output "    enter_proc(~a);\n" (func-name (car funcs))))
          (display (get-output-string (func-port (car funcs))) output)
          (format output "}\n")
          (format output "\n")
          (loop (cdr funcs))))))

(define (gen-symbol-defines program output)
  (let loop ((i 0) (symbols (program-symbols program)))
    (if (not (null? symbols))
        (begin
          (format output "#define sym~a SYMBOL(~a)\n" (mangle-name (car symbols)) i)
          (format output "#define symidx~a ~a\n" (mangle-name (car symbols)) i)
          (loop (+ i 1) (cdr symbols))))))

(define (gen-symbol-table-init program output)
  (let ((n-symbols (length (program-symbols program))))
    (format output "    n_symbols = ~a;\n" n-symbols)
    (format output "    symbols = malloc(sizeof(struct symbol) * ~a);\n" n-symbols)
    (let loop ((i 0) (symbols (program-symbols program)))
      (if (not (null? symbols))
          (begin
            (format output "    symbols[~a].name = \"~a\";\n" i (symbol->string (car symbols)))
            (format output "    symbols[~a].name_len = ~a;\n" i (string-length (symbol->string (car symbols))))
            (format output "    symbols[~a].value = VOID;\n" i)
            (loop (+ i 1) (cdr symbols)))))))

(define (output-program-code program filename)
  (let ((port (open-output-file filename)))
    (display "#include \"core.h\"\n\n" port)
    (gen-symbol-defines program port)
    (newline port)
    (gen-func-prototypes program port)
    (newline port)
    (gen-func-bodies program port)
    (display "int main(int argc, const char *argv[]) {\n" port)
    (display "    uint64_t ss;\n" port)
    (display "    stack_start = &ss;\n" port)
    (display "    init_memory();\n" port)
    (newline port)
    (gen-symbol-table-init program port)
    (newline port)
    (display "    current_input_port.type = OBJ_PORT;\n" port)
    (display "    current_input_port.port.direction = PORT_DIR_READ;\n" port)
    (display "    current_input_port.port.fp = stdin;\n" port)
    (display "    current_input_port.port.read_char = file_read_char;\n" port)
    (display "    current_input_port.port.peek_char = file_peek_char;\n" port)
    (display "    current_input_port.port.read_line = file_read_line;\n" port)
    (newline port)
    (display "    current_output_port.type = OBJ_PORT;\n" port)
    (display "    current_output_port.port.direction = PORT_DIR_WRITE;\n" port)
    (display "    current_output_port.port.fp = stdout;\n" port)
    (display "    current_output_port.port.printf = file_printf;\n" port)
    (display "    current_output_port.port.write_char = file_write_char;\n" port)
    (newline port)
    (display "    current_error_port.type = OBJ_PORT;\n" port)
    (display "    current_error_port.port.direction = PORT_DIR_WRITE;\n" port)
    (display "    current_error_port.port.fp = stderr;\n" port)
    (display "    current_error_port.port.printf = file_printf;\n" port)
    (display "    current_error_port.port.write_char = file_write_char;\n" port)
    (newline port)
    (display "    cmdline_argc = argc;\n" port)
    (display "    cmdline_argv = argv;\n" port)
    (newline port)
    (format port "    ~a(NULL, NO_CALL_FLAGS, 0);\n" (func-name (program-init-func program)))
    (display "}\n" port)
    (close-port port)))

(define-record-type <function>
  (make-function port varnum name program bindings parent freevars)
  function?
  (port func-port)
  (varnum func-varnum func-varnum-set!)
  (name func-name)
  (program func-program)
  (bindings func-bindings func-bindings-set!)
  (parent func-parent)
  (freevars func-freevars func-freevars-set!))

(define (add-function program parent params rest-param)
  (let ((func (make-function (open-output-string) ; port
                             0  ; varnum
                             (format "f~a" (program-next-funcnum program)) ; name
                             program
                             (let ((params (if rest-param (append params (list rest-param)) params)))
                               (map (lambda (p) (cons p (make-meaning (if parent 'local 'global) #f))) params))
                             parent
                             '() ; freevars
                             )))
    (program-add-function program func)
    func))

(define (func-next-varnum func)
  (let ((n (func-varnum func)))
    (func-varnum-set! func (+ n 1))
    n))

(define (func-add-freevar func var)
  (let ((new-freevars (append (func-freevars func) (list var))))
    (func-freevars-set! func new-freevars)

    ;; return the index of the new freevar
    (- (length new-freevars) 1)))

(define (func-add-binding func var meaning)
  (let ((new-bindings (cons (cons var meaning) (func-bindings func))))
    (func-bindings-set! func new-bindings)))

(define (func-lookup-binding func identifier)
  (let ((r (assq identifier (func-bindings func))))
    (if r (cdr r) #f)))

(define (func-has-param func name)
  (let ((m (func-lookup-binding func name)))
    (if m
        (if (eq? 'local (meaning-kind m)) #t #f)
        #f)))

(define (func-find-freevar func var)
  (let loop ((i 0) (freevars (func-freevars func)))
    (if (null? freevars)
        #f
        (if (eq? var (car freevars))
            i
            (loop (+ i 1) (cdr freevars))))))

(define (gen-code func indent fmt . args)
  (let ((port (func-port func)))
    (display (make-string (* indent *indent-size*) #\space) port)
    (apply format port fmt args)))

(define (compile-number func indent form)
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = FIXNUM(~a);\n" varnum form)
    varnum))

(define (compile-string func indent form)
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = make_string(~s, ~a);\n" varnum form (string-length form))
    varnum))

(define (compile-bool func indent form)
  (let ((varnum (func-next-varnum func)))
    (if form
        (gen-code func indent "value x~a = TRUE;\n" varnum)
        (gen-code func indent "value x~a = FALSE;\n" varnum))
    varnum))

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
    (gen-code func indent "value x~a = CHAR('~a');\n" varnum (c-char form))
    varnum))

(define *primcalls* '((apply "apply" 1 -1)
                      (car "car" 1 1)
                      (cdr "cdr" 1 1)
                      (boolean? "boolean_q" 1 1)
                      (car "car" 1 1)
                      (cdr "cdr" 1 1)
                      (char-downcase "char_downcase" 1 1)
                      (char-upcase "char_upcase" 1 1)
                      (char->integer "char_to_integer" 1 1)
                      (char? "char_q" 1 1)
                      (close-port "close_port" 1 1)
                      (command-line "command_line" 0 0)
                      (cons "cons" 2 2)
                      (current-input-port "current_input_port" 0 0)
                      (current-output-port "current_output_port" 0 0)
                      (current-error-port "current_error_port" 0 0)
                      (delete-file "delete_file" 1 1)
                      (display "display" 1 2)
                      (eof-object? "eof_object_q" 1 1)
                      (eq? "eq_q" 2 2)
                      (error "error" 1 1)
                      (error-object? "error_object_q" 1 1)
                      (exit "exit" 0 1)
                      (file-error? "file_error_q" 1 1)
                      (gensym "gensym" 0 1)
                      (get-output-string "get_output_string" 1 1)
                      (get-environment-variable "get_environment_variable" 1 1)
                      (input-port? "input_port_q" 1 1)
                      (integer->char "integer_to_char" 1 1)
                      (make-string "make_string" 1 2)
                      (make-vector "make_vector" 1 2)
                      (newline "newline" 0 1)
                      (number? "number_q" 1 1)
                      (number->string "number_to_string" 1 2)
                      (open-input-file "open_input_file" 1 1)
                      (open-output-file "open_output_file" 1 1)
                      (open-output-string "open_output_string" 0 0)
                      (pair? "pair_q" 1 1)
                      (peek-char "peek_char" 0 1)
                      (port? "port_q" 1 1)
                      (procedure? "procedure_q" 1 1)
                      (read-char "read_char" 0 1)
                      (read-line "read_line" 0 1)
                      (set-car! "set_car_b" 2 2)
                      (set-cdr! "set_cdr_b" 2 2)
                      (string-copy "string_copy" 1 3)
                      (string->number "string_to_number" 1 2)
                      (string->symbol "string_to_symbol" 1 1)
                      (symbol->string "symbol_to_string" 1 1)
                      (string-append "string_append" 0 -1)
                      (string-length "string_length" 1 1)
                      (string-ref "string_ref" 2 2)
                      (string-set! "string_set_b" 3 3)
                      (string=? "string_eq_q" 1 -1)
                      (string? "string_q" 1 1)
                      (substring "substring" 3 3)
                      (symbol? "symbol_q" 1 1)
                      (system "system" 1 1)
                      (uninterned-symbol? "uninterned_symbol_q" 1 1)
                      (unread-char "unread_char" 1 2)
                      (urandom "urandom" 1 1)
                      (unwrap "unwrap" 1 1)
                      (vector-length "vector_length" 1 1)
                      (vector-ref "vector_ref" 2 2)
                      (vector-set! "vector_set_b" 3 3)
                      (vector? "vector_q" 1 1)
                      (void "void" 0 0)
                      (wrap "wrap" 2 2)
                      (wrapped? "wrapped_q" 1 1)
                      (wrapped-kind "wrapped_kind" 1 1)
                      (write "write" 1 2)
                      (write-char "write_char" 1 2)
                      (+ "add" 0 -1)
                      (- "sub" 1 -1)
                      (* "mul" 0 -1)
                      (/ "div" 1 -1)
                      (= "num_eq" 1 -1)
                      (< "num_lt" 1 -1)
                      (> "num_gt" 1 -1)
                      (<= "num_le" 1 -1)
                      (>= "num_ge" 1 -1)))

(define *specials* '(and begin case cond define define-record-type if include lambda let or quasiquote quote))

;;; compiles a list of forms and returns their varnums as a list
(define (compile-list-of-forms func indent forms)
  (let loop ((varnums '()) (forms forms))
    (if (null? forms)
        (reverse varnums)
        (loop (cons (compile-form func indent (car forms)) varnums)
              (cdr forms)))))

;;; like compile-list-of-forms, but calls compile-quoted-item instead of
;;; compile-form for each item
(define (compile-list-of-quoted-forms func indent forms)
  (let loop ((varnums '()) (forms forms))
    (if (null? forms)
        (reverse varnums)
        (loop (cons (compile-quoted-item func indent (car forms)) varnums)
              (cdr forms)))))

(define (compile-primcall func indent form primcall-info)
  (let ((c-name (list-ref primcall-info 1))
        (min-args (list-ref primcall-info 2))
        (max-args (list-ref primcall-info 3))
        (arg-varnums (compile-list-of-forms func indent (cdr form)))
        (varnum (func-next-varnum func)))
    (gen-code func
              indent
              "value x~a = primcall_~a(NULL, NO_CALL_FLAGS, ~a~a~a);\n"
              varnum
              c-name
              (length arg-varnums)
              (if (null? arg-varnums) "" ", ")
              (string-join (map (lambda (n) (format "x~a" n)) arg-varnums) ", "))
    varnum))

(define (mangle-name name)
  (let ((name (if (symbol? name)
                  (symbol->string name)
                  (if (string? name)
                      name
                      (error "mangle-name argument not a string or symbol")))))
    (let loop ((i 0)
               (mangled "_"))
      (if (= i (string-length name))
          mangled
          (let ((ch (string-ref name i)))
            (cond ((= i (string-length name)) mangled)
                  ((or (char-alphabetic? ch) (char-numeric? ch))
                   (loop (+ i 1) (string-append-char mangled ch)))
                  ((char=? #\- ch) (loop (+ i 1) (string-append mangled "_")))
                  ((char=? #\_ ch) (loop (+ i 1) (string-append mangled "__")))
                  (else (loop (+ i 1) (format "~a_~a" mangled (char->integer ch))))))))))

(define (intern program sym)
  (let loop ((i 0) (symbols (program-symbols program)))
    (if (null? symbols)
        (program-symbols-set! program (cons sym (program-symbols program)))
        (if (eq? sym (car symbols))
            i
            (loop (+ i 1) (cdr symbols))))))

(define (compile-quoted-item func indent form)
  (cond ((boolean? form) (compile-form func indent form))
        ((number? form) (compile-form func indent form))
        ((char? form) (compile-form func indent form))
        ((string? form) (compile-form func indent form))
        ((vector? form) (compile-form func indent form))
        ((symbol? form) (let ((varnum (func-next-varnum func)))
                          (intern (func-program func) form)
                          (gen-code func indent "value x~a = sym~a;\n" varnum (mangle-name form))
                          varnum))
        ((null? form) (let ((varnum (func-next-varnum func)))
                        (gen-code func indent "value x~a = NIL;\n" varnum)
                        varnum))
        ((pair? form) (let ((varnum (func-next-varnum func)))
                        (gen-code func indent "value x~a = VOID;\n" varnum)
                        (let loop ((f form))
                          (if (atom? (cdr f))
                              (begin
                                (let ((cdr-varnum (compile-quoted-item func indent (cdr f))))
                                  (let ((car-varnum (compile-quoted-item func indent (car f))))
                                    (gen-code func indent "x~a = make_pair(x~a, x~a);\n" varnum car-varnum cdr-varnum))))
                              (begin
                                (loop (cdr f))
                                (let ((car-varnum (compile-quoted-item func indent (car f))))
                                  (gen-code func indent "x~a = make_pair(x~a, x~a);\n" varnum car-varnum varnum)))))
                        varnum))
        (else (compile-error "unknown quoted value: ~s" form))))

(define (compile-quote func indent form)
  (if (!= (length form) 2)
      (compile-error "quote expects a single argument")
      (compile-quoted-item func indent (cadr form))))

(define (compile-quasiquote func indent form)
  (compile-form func indent (qq-quasiquote form)))

(define (parse-lambda-params form)
  ;; given a lambda form, parse its arguments and return a list with two
  ;; elements: (params rest-param). params is the list of arguments
  ;; without rest parameter. rest-param is either the name of the reset
  ;; parameter, or #f otherwise.
  ;;
  ;; some examples and their return values:
  ;; (lambda () 1) => (() #f)
  ;; (lambda (x y) x) => ((x y) #f)
  ;; (lambda (x y . z) x) => ((x y) z)
  ;; (lambda x x) => (() x)
  (let ((params (cadr form)))
    (if (symbol? params)
        (list '() params)
        (let loop ((proper-params '()) (params params))
          (cond ((null? params) (list (reverse proper-params) #f))
                ((atom? params) (list (reverse proper-params) params))
                (else (loop (cons (car params) proper-params) (cdr params))))))))

(define (compile-lambda func indent form known-freevars)
  (if (< (length form) 3)
      (compile-error "malformed lambda"))
  (let ((params-and-rest (parse-lambda-params form)))
    (let ((params (car params-and-rest))
          (rest-param (cadr params-and-rest)))
      (let ((new-func (add-function (func-program func)
                                    func
                                    params
                                    rest-param)))
        ;; add known free vars to the beginning of the list of free vars
        (let loop ((known-freevars known-freevars))
          (if (not (null? known-freevars))
              (begin
                (func-add-freevar new-func (car known-freevars))
                (loop (cdr known-freevars)))))

        (if (eq? rest-param #f)
            (gen-code new-func 1 "if (nargs != ~a) RAISE(\"argument count mismatch: %s\", find_func_name(~a));\n" (length params) (func-name new-func))
            (gen-code new-func 1 "if (nargs < ~a) RAISE(\"too few arguments for function: %s\", find_func_name(~a));\n" (length params) (func-name new-func)))

        ;; generate code for reading arguments
        (gen-code new-func 1 "init_args();\n")
        (let loop ((params params))
          (if (not (null? params))
              (begin
                (if (not (symbol? (car params)))
                    (compile-error "parameter not an identifier: ~a" (car params)))
                (gen-code new-func 1 "value ~a = next_arg();\n" (mangle-name (car params)))
                (loop (cdr params)))))

        (if (not (eq? rest-param #f))
            (begin
              (gen-code new-func 1 "value ~a = NIL;\n" (mangle-name rest-param))
              (gen-code new-func 1 "for (int i = 0; i < nargs - ~a; ++i) { value v = next_arg(); ~a = make_pair(v, ~a); }\n" (length params) (mangle-name rest-param) (mangle-name rest-param))
              (gen-code new-func 1 "~a = reverse_list(~a, NIL);\n" (mangle-name rest-param) (mangle-name rest-param))))

        (gen-code new-func 1 "\n")

        ;; compile function body
        (let loop ((body (cddr form)) (varnum -1))
          (if (null? body)
              (begin
                (gen-code new-func 1 "free_args();\n")
                (if (program-debug (func-program func))
                    (gen-code new-func 1 "leave_proc();\n"))
                (gen-code new-func 1 "return x~a;\n" varnum))
              (let ((form (car body)))
                (loop (cdr body) (compile-form new-func 1 form)))))

        ;; generate the code for referencing the function
        (let ((varnum (func-next-varnum func)))
          (gen-code func indent "value x~a = make_closure(~a, ~a, ~a"
                    varnum
                    (func-name new-func)
                    (length params)
                    (length (func-freevars new-func)))
          (let loop ((freevars (func-freevars new-func)))
            (if (not (null? freevars))
                (begin
                  (if (func-has-param func (car freevars))
                      (gen-code func 0 ", ~a" (mangle-name (car freevars))) ;; generate mangled param name
                      (if (memq (car freevars) known-freevars)
                          (gen-code func 0 ", (value) 0")
                          (let ((freevar (func-find-freevar func (car freevars))))
                            (if freevar
                                (gen-code func 0 ", envget(env, ~a)" freevar)
                                (gen-code func 0 ", envget(env, ~a)" (func-add-freevar func (car freevars)))))))
                  (loop (cdr freevars)))))
          (gen-code func 0 ");\n")

          ;; return function varnum
          varnum)))))

(define (compile-let func indent form)
  (if (< (length form) 3)
      (compile-error "malformed let"))
  (let ((name (if (symbol? (cadr form))
                  (cadr form)
                  #f))
        (body (if (symbol? (cadr form))
                  (cdddr form)
                  (cddr form))))
    (let loop ((bindings (if (symbol? (cadr form))
                             (caddr form)
                             (cadr form)))
               (params '())
               (init-forms '()))
      (if (null? bindings)
          (let ((func-varnum (compile-lambda func indent (append (list 'lambda (reverse params)) body) (if name (list name) '()))))
            (if name
                (gen-code func indent "GET_CLOSURE(x~a)->freevars[0] = x~a;\n" func-varnum func-varnum))
            (let ((arg-varnums (compile-list-of-forms func indent (reverse init-forms))))
              (let ((ret-varnum (func-next-varnum func)))
                (gen-code func indent "value x~a = GET_CLOSURE(x~a)->func(GET_CLOSURE(x~a)->freevars, NO_CALL_FLAGS, ~a~a~a);\n"
                          ret-varnum
                          func-varnum
                          func-varnum
                          (length arg-varnums)
                          (if (eq? '() arg-varnums) "" ", ")
                          (string-join (map (lambda (n) (format "x~a" n)) arg-varnums) ", "))
                ret-varnum)))
          (let ((binding (car bindings)))
            (if (!= (length binding) 2)
                (compile-error "bad let binding: ~s" binding)
                (loop (cdr bindings) (cons (car binding) params) (cons (cadr binding) init-forms))))))))

(define (compile-define func indent form)
  (if (< (length form) 2)
      (compile-error "malformed define: ~s" form))
  (if (and (symbol? (cadr form))
           (> (length form) 3))
      (compile-error "malformed define: ~s" form))
  (if (and (list? (cadr form))
           (< (length form) 3))
      (compile-error "malformed define: ~s" form))

  ;; we could maybe allow this, and then further on, declare the
  ;; variable in-place as opposed to setting the global variable.
  ;; something like this:
  ;;
  ;; (if (null? (func-parent func))
  ;;     (gen-code func indent "~a = x~a;\n" (mangle-name name) init-varnum)
  ;;     (gen-code func indent "value ~a = x~a;\n" (mangle-name name) init-varnum))
  ;;
  ;; this is technically incorrect though, since it can't shadow current
  ;; function parameters while it should.
  (if (func-parent func)
      (compile-error "define currently only supported at the top-level"))

  ;; convert the function form to normal form
  (let ((form (if (pair? (cadr form))
                  (list 'define
                        (caadr form)
                        (append (list 'lambda (cdadr form)) (cddr form)))
                  form)))
    (let ((name (cadr form))
          (init-form (if (< (length form) 3)
                         #f
                         (caddr form))))
      ;; check for re-definition
      (let ((meaning (lookup-identifier func name)))
        (if (and meaning (eq? 'global (meaning-kind meaning)))
            (compile-error "re-defining: ~a" name)))

      ;; top-level variable names are all interned, since the values of
      ;; global variables are stored in the symbol table.
      (intern (func-program func) name)

      ;; add the name to the list of current (top-level) function bindings
      (func-add-binding func name (make-meaning 'global #f))

      ;; copmile init form and either set it as a global variable (if in
      ;; the top-level), or declare it as a variable.
      (let ((init-varnum (compile-form func indent init-form)))
        ;; if not init value, we won't initialize here. all global
        ;; variables are initialized with VOID at the top-level.
        (if init-form
            (gen-code func indent "symbols[symidx~a].value = x~a;\n" (mangle-name name) init-varnum))
        init-varnum))))

(define (compile-if func indent form)
  (if (or (< (length form) 3) (> (length form) 4))
      (compile-error "malformed if"))
  (let ((cond-varnum (compile-form func indent (cadr form)))
        (one-legged (= (length form) 3)))
    (let ((ret-varnum (func-next-varnum func)))
          (gen-code func indent "value x~a = VOID;\n" ret-varnum)
          (gen-code func indent "if (x~a != FALSE) {\n" cond-varnum)
          (let ((then-varnum (compile-form func (+ indent 1) (caddr form))))
            (gen-code func (+ indent 1) "x~a = x~a;\n" ret-varnum then-varnum)
            (if one-legged
                (gen-code func indent "}\n")
                (begin
                  (gen-code func indent "} else {\n")
                  (let ((else-varnum (compile-form func (+ indent 1) (cadddr form))))
                    (gen-code func (+ indent 1) "x~a = x~a;\n" ret-varnum else-varnum)
                    (gen-code func indent "}\n")))))
          ret-varnum)))

(define (compile-cond func indent form)
  (let ((ret-varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = VOID;\n" ret-varnum)
    (let loop ((clauses (cdr form)) (i 0))
      (if (null? clauses)
          ret-varnum
          (let ((clause (car clauses)))
            (if (not (pair? clause))
                (compile-error "bad cond clause: ~s" clause))
            (if (eq? (car clause) 'else)
                (cond ((null? (cdr clause))
                       (compile-error "bad cond else clause: ~s" clause))
                      ((!= 1 (length clauses))
                       (compile-error "else is not the last cond clause"))
                      (else (let ((arg-varnums (compile-list-of-forms func (+ indent i) (cdr clause))))
                              (gen-code func (+ indent i) "x~a = x~a;\n" ret-varnum (last arg-varnums)))))
                (let ((cond-varnum (compile-form func (+ indent i) (car clause))))
                  (gen-code func (+ indent i) "if (x~a != FALSE) {\n" cond-varnum)
                  (let ((arg-varnums (compile-list-of-forms func (+ indent i 1) (cdr clause))))
                    (if (null? arg-varnums)
                        (gen-code func (+ indent i 1) "x~a = x~a;\n" ret-varnum cond-varnum)
                        (gen-code func (+ indent i 1) "x~a = x~a;\n" ret-varnum (last arg-varnums)))
                    (gen-code func (+ indent i) "} else {\n")
                    (loop (cdr clauses) (+ i 1))))))))
    (let ((have-else (eq? 'else (car (last form)))))
      (let loop ((i 0))
        (if (< i (- (length form) 1 (if have-else 1 0)))
            (begin
              (gen-code func (+ indent (length form) (- i) (if have-else -1 0) -2) "}\n")
              (loop (+ i 1))))))
    ret-varnum))

(define (compile-case func indent form)
  (if (< (length form) 2)
      (compile-error "bad case syntax: ~s" form))
  (let ((key-varnum (compile-form func indent (cadr form))))
    (let ((ret-varnum (func-next-varnum func)))
      (gen-code func indent "value x~a = VOID;\n" ret-varnum)
      (let loop ((clauses (cddr form)) (i 0))
        (if (null? clauses)
            ret-varnum
            (let ((clause (car clauses)))
              (if (not (pair? clause))
                  (compile-error "bad case clause: ~s" clause))
              (if (eq? (car clause) 'else)
                  (cond ((null? (cdr clause))
                         (compile-error "bad case else clause: ~s" clause))
                        ((!= 1 (length clauses))
                         (compile-error "else is not the last case clause"))
                        (else (let ((arg-varnums (compile-list-of-forms func (+ indent i) (cdr clause))))
                                (gen-code func (+ indent i) "x~a = x~a;\n" ret-varnum (last arg-varnums)))))
                  (if (not (pair? (car clause)))
                      (compile-error "bad case clause: ~s" clause)
                      (let ((data-varnums (compile-list-of-quoted-forms func (+ indent i) (car clause))))
                        (gen-code func (+ indent i) "if (~a) {\n" (string-join (map (lambda (x) (format "x~a == x~a" key-varnum x)) data-varnums) " || "))
                        (let ((arg-varnums (compile-list-of-forms func (+ indent i 1) (cdr clause))))
                          (gen-code func (+ indent i 1) "x~a = x~a;\n" ret-varnum (last arg-varnums))
                          (gen-code func (+ indent i) "} else {\n")
                          (loop (cdr clauses) (+ i 1)))))))))
      (let ((have-else (eq? 'else (car (last form)))))
        (let loop ((i (+ (length form) -3 (if have-else 0 1))))
          (if (positive? i)
              (begin
                (gen-code func (+ indent i -1) "}\n")
                (loop (- i 1))))))
      ret-varnum)))

(define (compile-include func indent form)
  (if (or (!= (length form) 2)
          (not (string? (cadr form))))
      (compile-error "bad include form: ~s" form))
  (let ((port (open-input-file (cadr form))))
    (program-push-port (func-program func) port))
  -1)

(define (compile-and func indent form)
  (let ((ret-varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = TRUE;\n" ret-varnum)
    (let loop ((values (cdr form)) (i 0))
      (if (not (null? values))
          (let ((arg-varnum (compile-form func (+ indent i) (car values))))
            (gen-code func (+ indent i) "if (x~a == FALSE) {\n" arg-varnum)
            (gen-code func (+ indent i 1) "x~a = FALSE;\n" ret-varnum)
            (gen-code func (+ indent i) "} else {\n")
            (gen-code func (+ indent i 1) "x~a = x~a;\n" ret-varnum arg-varnum)
            (loop (cdr values) (+ i 1)))))
    (let loop ((values (cdr form)))
      (if (not (null? values))
          (begin
            (gen-code func (length values) "}\n")
            (loop (cdr values)))))
    ret-varnum))

(define (compile-or func indent form)
  (let ((ret-varnum (func-next-varnum func)))
    (gen-code func indent "value x~a;\n" ret-varnum)
    (let loop ((values (cdr form)) (i 0))
      (if (not (null? values))
          (let ((arg-varnum (compile-form func (+ indent i) (car values))))
            (gen-code func (+ indent i) "if (x~a != FALSE) {\n" arg-varnum)
            (gen-code func (+ indent i 1) "x~a = x~a;\n" ret-varnum arg-varnum)
            (gen-code func (+ indent i) "} else {\n")
            (loop (cdr values) (+ i 1)))))
    (gen-code func (+ indent (length form) -1) "x~a = FALSE;\n" ret-varnum)
    (let loop ((values (cdr form)))
      (if (not (null? values))
          (begin
            (gen-code func (length values) "}\n")
            (loop (cdr values)))))
    ret-varnum))

(define (compile-begin func indent form)
  (let ((ret-varnum (func-next-varnum func)))
    (if (= 1 (length form))
        (if (eq? (func-parent func) #f)
            (gen-code func indent "value x~a = VOID;\n" ret-varnum)
            (compile-error "empty begin expression is not allowed"))
        (let loop ((exprs (cdr form)))
          (let ((expr-varnum (compile-form func indent (car exprs))))
            (if (null? (cdr exprs))
                (gen-code func indent "value x~a = x~a;\n" ret-varnum expr-varnum)
                (loop (cdr exprs))))))
    ret-varnum))

(define (compile-define-record-type func indent form)
  (if (func-parent func)
      (error "define-record-type only supported at the top-level at the moment"))
  (if (or (< (length form) 4)
          (not (symbol? (cadr form)))
          (not (list? (caddr form)))
          (not (symbol? (cadddr form))))
      (error "bad define-record-type form"))
  (let ((type-name (cadr form))
        (ctor (caddr form))
        (pred (cadddr form))
        (fields (cdr (cdddr form))))
    ;; validate fields
    (let loop ((fields fields) (names '()))
      (if (pair? fields)
          (begin
            (if (memq (caar fields) names)
                (compile-error "duplicate field name: ~s" (caar fields)))
            (if (not (list? (car fields)))
                (compile-error "bad field spec: ~s" (car fields)))
            (if (or (< (length (car fields)) 2)
                    (> (length (car fields)) 3))
                (compile-error "bad field spec: ~s" (car fields)))
            (if (not (all? (map symbol? (car fields))))
                (compile-error "bad field spec: ~s" (car fields)))
            (loop (cdr fields) (cons (caar fields) names)))))

    (let ((field-names (map car fields)))
      ;; make sure constructor arguments are in the fields list
      (let loop ((ctor-args (cdr ctor)))
        (if (pair? ctor-args)
            (begin
              (if (not (memq (car ctor-args) field-names))
                  (compile-error "constructor argument ~a not in fields list" (car ctor-args)))
              (loop (cdr ctor-args)))))

      (compile-form func indent `(define ,type-name (gensym (symbol->string ',type-name))))
      (compile-form func indent `(define (,(car ctor) ,@(cdr ctor))
                                   (wrap (vector ,@(map (lambda (x)
                                                          (if (memq x (cdr ctor))
                                                              x
                                                              '(void)))
                                                        field-names))
                                         ,type-name)))
      (compile-form func indent `(define (,pred x)
                                   (eq? ,type-name (wrapped-kind x))))
      (let loop ((fields fields) (idx 0))
        (if (pair? fields)
            (let ((field-name (caar fields))
                  (accessor-name (cadar fields))
                  (mutator-name (if (> (length (car fields)) 2) (caddar fields) #f)))
              (compile-form func indent `(define (,accessor-name x)
                                           (vector-ref (unwrap x) ,idx)))
              (if mutator-name
                  (compile-form func indent `(define (,mutator-name x value)
                                               (vector-set! (unwrap x) ,idx value))))
              (loop (cdr fields) (+ idx 1))))))))

(define (compile-special func indent form kind)
  (case kind
    ((and) (compile-and func indent form))
    ((begin) (compile-begin func indent form))
    ((case) (compile-case func indent form))
    ((cond) (compile-cond func indent form))
    ((define) (compile-define func indent form))
    ((define-record-type) (compile-define-record-type func indent form))
    ((include) (compile-include func indent form))
    ((if) (compile-if func indent form))
    ((let) (compile-let func indent form))
    ((lambda) (compile-lambda func indent form '()))
    ((or) (compile-or func indent form))
    ((quasiquote) (compile-quasiquote func indent form))
    ((quote) (compile-quote func indent form))
    (else -1)))

(define (compile-call func indent form)
  (let ((func-varnum (compile-form func indent (car form))))
    (gen-code func indent "if (!IS_CLOSURE(x~a)) { RAISE(\"called object not a procedure\"); }\n" func-varnum)
    (let ((arg-varnums (compile-list-of-forms func indent (cdr form))))
      (let ((ret-varnum (func-next-varnum func)))
        (gen-code func indent "value x~a = GET_CLOSURE(x~a)->func(GET_CLOSURE(x~a)->freevars, NO_CALL_FLAGS, ~a~a~a);\n"
                  ret-varnum
                  func-varnum
                  func-varnum
                  (length arg-varnums)
                  (if (eq? '() arg-varnums) "" ", ")
                  (string-join (map (lambda (n) (format "x~a" n)) arg-varnums) ", "))
        ret-varnum))))

(define (lookup-primcall identifier)
  (let loop ((primcalls *primcalls*))
    (if (null? primcalls)
        #f
        (if (eq? identifier (caar primcalls))
            (car primcalls)
            (loop (cdr primcalls))))))

(define (lookup-special identifier)
  (let loop ((specials *specials*))
    (if (null? specials)
        #f
        (if (eq? identifier (car specials))
            (car specials)
            (loop (cdr specials))))))

(define-record-type <meaning>
  (make-meaning kind info)
  meaning?
  (kind meaning-kind)
  (info meaning-info))

(define (lookup-identifier func identifier)
  ;; returned types
  ;;  - local: it's a local variable in current function
  ;;
  ;;  - free: it's a free variable in current function but a local in
  ;;    one of its parents (but not top-level). this is also used for
  ;;    variables that are not local to the parent, but set by the
  ;;    caller in the environment (like referring to the name of a named
  ;;    let block).
  ;;
  ;;  - global: it's defined in top-level
  ;;
  ;;  - primcall: it's a primcall
  ;;
  ;;  - special: it's a special form
  ;;
  ;;  - unknown: neither of the previous ones

  (let ((meaning (func-lookup-binding func identifier)))
    (if meaning
        meaning
        (let loop ((func func))
          (if (not func)
              (let ((primcall-info (lookup-primcall identifier)))
                (if primcall-info
                    (make-meaning 'primcall primcall-info)
                    (let ((special-info (lookup-special identifier)))
                      (if special-info
                          (make-meaning 'special special-info)
                          (make-meaning 'unknown #f)))))
              (let ((meaning (func-lookup-binding func identifier)))
                (if meaning
                    (if (eq? (meaning-kind meaning) 'local)
                        (make-meaning 'free #f)
                        meaning)
                    (if (func-find-freevar func identifier)
                        (make-meaning 'free #f)
                        (loop (func-parent func))))))))))

(define (compile-list func indent form)
  (if (not (symbol? (car form)))
      (compile-call func indent form)
      (let ((meaning (lookup-identifier func (car form))))
        (case (meaning-kind meaning)
          ((local global free) (compile-call func indent form))
          ((primcall) (compile-primcall func indent form (meaning-info meaning)))
          ((special) (compile-special func indent form (meaning-info meaning)))

          ;; allow use not-yet defined variables only in non-top-level
          ;; functions, i.e. inside a lambda or let, but not directly.
          ;; for example:
          ;;     (define foo bar)
          ;; is not allowed where bar is undefined, but:
          ;;     (define foo (lambda () bar))
          ;; is allowed.
          ((unknown) (if (func-parent func)
                         (compile-call func indent form)
                         (compile-error "unbound identifier: ~a" (car form))))
          (else (error (format "unhandled identifier kind: ~a" (meaning-kind meaning))))))))

(define (compile-identifier func indent form)
  (let ((meaning (lookup-identifier func form))
        (varnum (func-next-varnum func)))
    (case (meaning-kind meaning)
      ((local)
       (gen-code func indent "value x~a = ~a;\n" varnum (mangle-name form)))
      ((global)
       (gen-code func indent "value x~a = symbols[symidx~a].value;\n" varnum (mangle-name form)))
      ((free)
       (let ((freevar-idx (func-find-freevar func form)))
         (let ((freevar-idx (if freevar-idx
                                freevar-idx
                                (func-add-freevar func form))))
           (gen-code func indent "value x~a = envget(env, ~a);\n" varnum freevar-idx))))
      ((primcall)
       (gen-code func indent "value x~a = make_closure(primcall_~a, 0, 0);\n" varnum (list-ref (meaning-info meaning) 1)))
      ((special) (compile-error "invalid use of special: ~a" form))

      ;; allow use of not-yet defined variables only in non-top-level
      ;; functions, i.e. inside a lambda or let, but not directly.
      ;; for example:
      ;;     (define foo bar)
      ;; is not allowed where bar is undefined, but:
      ;;     (define foo (lambda () bar))
      ;; is allowed.
      ((unknown) (if (func-parent func)
                     (begin
                       (program-add-referenced-var (func-program func) form)
                       (gen-code func indent "value x~a = symbols[symidx~a].value;\n" varnum (mangle-name form)))
                     (compile-error "unbound identifier: ~a" form)))

      (else (error (format "unknown meaning kind: ~a" (meaning-kind meaning)))))
    varnum))

(define (compile-vector func indent form)
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = make_vector(~a, VOID);\n" varnum (vector-length form))
    (let loop ((i 0))
      (if (< i (vector-length form))
          (let ((value-varnum (compile-quoted-item func indent (vector-ref form i))))
            (gen-code func indent "GET_OBJECT(x~a)->vector.data[~a] = x~a;\n" varnum i value-varnum)
            (loop (+ i 1)))))
    varnum))

(define (compile-form func indent form)
  (cond ((symbol? form) (compile-identifier func indent form))
        ((number? form) (compile-number func indent form))
        ((string? form) (compile-string func indent form))
        ((boolean? form) (compile-bool func indent form))
        ((char? form) (compile-char func indent form))
        ((pair? form) (compile-list func indent form))
        ((vector? form) (compile-vector func indent form))
        ((eq? form *dot*) (compile-error "unexpected dot (.)"))
        (else (compile-error "don't know how to compile form: ~s" form))))

(define (compile-error fmt . args)
  (apply format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (compile-program program)
  (let ((func (add-function program #f '() #f)))
    (program-init-func-set! program func)
    (let loop ((form (read (program-port program))))
      (if (eof-object? form)
          (if (= (length (program-ports program)) 1)
              (begin
                (if (program-is-test-suite program)
                    (gen-code func 1 "printf(\"\\n\");\n"))
                (gen-code func 1 "return VOID;\n"))
              (begin
                (program-pop-port program)
                (loop (read (program-port program)))))
          (let ((varnum (compile-form func 1 form)))
            (if (and (program-is-test-suite program)
                     (program-is-main-file program))
                (gen-code func 1 "if (x~a == TRUE) { printf(\".\"); } else { printf(\"F(~a)\"); }\n" varnum (program-test-counter-inc! program)))
            (loop (read (program-port program))))))
    ;; check for undefined referenced variables
    (let loop ((vars (program-referenced-vars program)))
      (if (not (null? vars))
          (let ((meaning (lookup-identifier func (car vars))))
            (if (not (and meaning (eq? 'global (meaning-kind meaning))))
                (compile-error "undefined variable: ~a" (car vars))))))))

;;;;;; command-line parsing ;;;;;;

(define-record-type <cmdline>
  (make-cmdline just-compile run output-file input-file test c-file executable-file delete-executable debug cflags)
  cmdline?
  (just-compile cmdline-just-compile cmdline-just-compile-set!)
  (run cmdline-run cmdline-run-set!)
  (output-file cmdline-output-file cmdline-output-file-set!)
  (input-file cmdline-input-file cmdline-input-file-set!)
  (test cmdline-test cmdline-test-set!)
  (c-file cmdline-c-file cmdline-c-file-set!)
  (executable-file cmdline-executable-file cmdline-executable-file-set!)
  (delete-executable cmdline-delete-executable cmdline-delete-executable-set!)
  (debug cmdline-debug cmdline-debug-set!)
  (cflags cmdline-cflags cmdline-cflags-set!))

(define (create-cmdline-args)
  (make-cmdline #f ; just compile
                #f ; run
                #f ; output file
                #f ; input file
                #f ; test
                #f ; c output file
                #f ; executable output file
                #f ; delete executable
                #f ; debug
                "" ; cflags
                ))

(define (command-line-error fmt . args)
  (apply format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (parse-command-line-args)
  (let ((args (create-cmdline-args)))
    (let loop ((cl (cdr (command-line))))
      (cond ((null? cl) args)
            ((string=? (car cl) "-h")
             (print-usage))
            ((string=? (car cl) "-?")
             (print-usage))
            ((string=? (car cl) "-c")
             (cmdline-just-compile-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-r")
             (cmdline-run-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-t")
             (cmdline-test-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-g")
             (cmdline-debug-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-f")
             (if (null? (cdr cl))
                 (command-line-error "missing argument to -f")
                 (begin
                   (cmdline-cflags-set! args (cadr cl))
                   (loop (cddr cl)))))
            ((string=? (car cl) "-o")
             (if (null? (cdr cl))
                 (command-line-error "missing argument to -o")
                 (begin
                   (cmdline-output-file-set! args (cadr cl))
                   (loop (cddr cl)))))
            (else (if (cmdline-input-file args)
                      (begin
                        (print "ERR")
                       (command-line-error "unexpected argument: ~a" (car cl))
                       )
                      (begin
                        (cmdline-input-file-set! args (car cl))
                        (loop (cdr cl)))))))))

(define (print-usage)
  (format (current-error-port) "usage: ~a input-file [-r] [-c] [-o output-file] [-f cflags] [-g]

 -r\tcompile and run the program
 -c\tonly compile a c file
 -o\tthe name of the output file. defaults to b.c or b.out depending on
\twhether -c is passed or not.
 -f\tuse the given options when invoking the C compiler
 -t\tcompile the program as a test suite
 -g\tadd debug instrumentation
" (car (command-line)))
  (exit 0)
  )

(define (hex-encode s)
  (let loop ((i 0) (x ""))
    (if (= i (string-length s))
        x
        (let ((c (char->integer (string-ref s i))))
          (if (< c 16)
              (loop (+ i 1) (format "~a0~x" x c))
              (loop (+ i 1) (format "~a~x" x c)))))))

(define (temp-filename)
  (format "/tmp/~a" (hex-encode (urandom 6))))

(define (postprocess-cmdline args)
  (if (not (cmdline-input-file args))
      (command-line-error "missing input file"))
  (if (cmdline-output-file args)
      (if (cmdline-just-compile args)
          (cmdline-c-file-set! args (cmdline-output-file args))
          (begin
            (cmdline-c-file-set! args (string-append (temp-filename) ".c"))
            (cmdline-executable-file-set! args (cmdline-output-file args))))
      (begin
        (cmdline-delete-executable-set! args #t)
        (if (cmdline-just-compile args)
            (cmdline-output-file-set! args "b.c")
            (if (cmdline-run args)
                (begin
                  (cmdline-output-file-set! args (temp-filename))
                  (cmdline-delete-executable-set! args #t))
                (cmdline-output-file-set! args "b.out")))))
  (if (cmdline-just-compile args)
      (cmdline-c-file-set! args (cmdline-output-file args))
      (begin
        (cmdline-c-file-set! args (string-append (temp-filename) ".c"))
        (cmdline-executable-file-set! args (cmdline-output-file args)))))

;;;;;; main ;;;;;;

(let ((args (parse-command-line-args)))
  (postprocess-cmdline args)
  (let ((port (open-input-file (cmdline-input-file args))))
    (let ((program (create-program port)))
      (if (cmdline-test args)
          (program-is-test-suite-set! program #t))
      (if (cmdline-debug args)
          (program-debug-set! program #t))
      (compile-program program)
      (output-program-code program (cmdline-c-file args))
      (if (not (cmdline-just-compile args))
          (let ((cc (get-environment-variable "CC")))
            (let ((cc (if cc cc "gcc"))
                  (own-cflags (if (program-debug program) " -DDEBUG" "")))
              (let ((cmd (format "~a -I.~a -o ~a ~a ~a" cc own-cflags (cmdline-executable-file args) (cmdline-cflags args) (cmdline-c-file args))))
                (let ((ret (system cmd)))
                  (delete-file (cmdline-c-file args))
                  (if (not (zero? ret))
                      (begin
                        (format (current-error-port) "program exited with error code: ~a\n" ret)
                        (exit ret))))))))
      (if (cmdline-run args)
          (let ((cmd (format "$(realpath ~a)" (cmdline-executable-file args))))
            (let ((ret (system cmd)))
              (if (cmdline-delete-executable args)
                  (delete-file (cmdline-executable-file args)))
              (exit ret)))))))
