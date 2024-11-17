(include "utils.scm")
(include "format.scm")
(include "qq.scm")

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
          ((char=? #\` ch) (read-quasiquoted-form port))
          ((char=? #\, ch) (read-unquoted-form port))
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
        '() ; interned symbols
        #f  ; init func, to be set later
        ))

(define (program-port program)
  (car program))

(define (program-funcs program)
  (cadr program))

(define (program-next-funcnum func)
  (let ((n (caddr func)))
    (list-set! func 2 (+ n 1))
    n))

(define (program-symbols program)
  (list-ref program 3))

(define (program-set-symbols program symbols)
  (list-set! program 3 symbols))

(define (program-init-func program)
  (list-ref program 4))

(define (program-init-func-set! program func)
  (list-set! program 4 func))

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

(define (gen-symbol-defines program output)
  (let loop ((i 0) (symbols (program-symbols program)))
    (if (not (null? symbols))
        (begin
          (format output "#define sym~a ~a\n" (mangle-name (car symbols)) i)
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
            (loop (+ i 1) (cdr symbols)))))))

(define (gen-global-vars program output)
  (let ((func (program-init-func program)))
    (let loop ((params (func-params func)))
      (if (not (null? params))
          (begin
            (format output "static value ~a = VOID;\n" (mangle-name (car params)))
            (loop (cdr params)))))))

(define (output-program-code program)
  (let ((port (open-output-file "b.c")))
    (display "#include \"core.h\"\n\n" port)
    (gen-symbol-defines program port)
    (newline port)
    (gen-global-vars program port)
    (newline port)
    (gen-func-prototypes program port)
    (newline port)
    (gen-func-bodies program port)
    (display "int main(int argc, char *argv[]) {\n" port)
    (gen-symbol-table-init program port)
    (newline port)
    (display "    f0(NULL, 0);\n" port)
    (display "}\n" port)))

(define (add-function program parent params rest-param)
  (let ((func (list (open-output-string) ; port
                    0                    ; varnum
                    (format "f~a" (program-next-funcnum program)) ; name
                    program
                    params
                    parent
                    rest-param
                    '() ; freevars
                    )))
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

(define (func-program func)
  (list-ref func 3))

(define (func-params func)
  (list-ref func 4))

(define (func-params-set! func params)
  (list-set! func 4 params))

(define (func-parent func)
  (list-ref func 5))

(define (func-rest-param func)
  (list-ref func 6))

(define (func-freevars func)
  (list-ref func 7))

(define (func-freevars-set! func freevars)
  (list-set! func 7 freevars))

(define (func-add-freevar func var)
  (let ((new-freevars (cons var (func-freevars func))))
    (func-freevars-set! new-freevars)

    ;; return the index of the new freevar
    (- (length new-freevars) 1)))

(define (func-add-param func var)
  (let ((new-params (cons var (func-params func))))
    (func-params-set! func new-params)))

(define (func-find-freevar func var)
  (let loop ((i 0) (freevars (func-freevars func)))
    (if (null? freevars)
        #f
        (if (eq? var (car freevars))
            i
            (loop (+ i 1) (cdr freevars))))))

(define (func-has-param func param)
  (let loop ((params (func-params)))
    (if (null? params)
        #f
        (if (eq? param (car params))
            #t
            (loop (cdr params))))))

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
        (gen-code func indent "value x~a = TRUE;\n" varnum form)
        (gen-code func indent "value x~a = FALSE;\n" varnum form))
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
    (gen-code func indent "value x~a = CHAR('~a');\n" varnum (c-char form))))

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
                      (display "display" 1 2)
                      (eof-object? "eof_object_q" 1 1)
                      (eq? "eq_q" 2 2)
                      (error "error" 1 1)
                      (exit "exit" 0 1)
                      (gensym "gensym" 0 1)
                      (get-output-string "get_output_string" 1 1)
                      (input-port? "input_port_q" 1 1)
                      (integer->char "integer_to_char" 1 1)
                      (make-string "make_string" 1 2)
                      (newline "newline" 0 1)
                      (number? "number_q" 1 1)
                      (number->string "number_to_string" 1 2)
                      (open-input-file "open_input_file" 1 1)
                      (open-output-file "open_output_file" 1 1)
                      (open-output-string "open_output_string" 0 0)
                      (pair? "pair_q" 1 1)
                      (peek-char "peek_char" 0 1)
                      (port? "port_q" 1 1)
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
                      (string=? "string_eq_q" 1 -1)
                      (string? "string_q" 1 1)
                      (substring "substring" 3 3)
                      (symbol? "symbol_q" 1 1)
                      (uninterned-symbol? "uninterned_symbol_q" 1 1)
                      (void "void" 0 0)
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

;;; compiles a list of forms and returns their varnums as a list
(define (compile-list-of-forms func indent forms)
  (let loop ((varnums '()) (forms forms))
    (if (null? forms)
        (reverse varnums)
        (loop (cons (compile-form func indent (car forms)) varnums)
              (cdr forms)))))

(define (compile-primcall func indent form)
  (let loop ((primcalls *primcalls*))
    (if (null? primcalls)
        -1
        (if (and (symbol? (car form))
                 (eq? (car form) (caar primcalls)))
            (let ((c-name (list-ref (car primcalls) 1))
                  (min-args (list-ref (car primcalls) 2))
                  (max-args (list-ref (car primcalls) 3))
                  (arg-varnums (compile-list-of-forms func indent (cdr form)))
                  (varnum (func-next-varnum func)))
              (gen-code func
                        indent
                        "value x~a = primcall_~a(NULL, NO_CALL_FLAGS, ~a, ~a);\n"
                        varnum
                        c-name
                        (length arg-varnums)
                        (string-join (map (lambda (n) (format "x~a" n)) arg-varnums) ", "))
              varnum)
            (loop (cdr primcalls))))))

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
        (program-set-symbols program (cons sym (program-symbols program)))
        (if (eq? sym (car symbols))
            i
            (loop (+ i 1) (cdr symbols))))))

(define (compile-quoted-item func indent form)
  (cond ((boolean? form) (compile-form func indent form))
        ((number? form) (compile-form func indent form))
        ((char? form) (compile-form func indent form))
        ((string? form) (compile-form func indent form))
        ((symbol? form) (let ((varnum (func-next-varnum func)))
                          (intern (func-program func) form)
                          (gen-code func indent "value x~a = sym~a;\n" varnum (mangle-name form))
                          varnum))
        ((list? form) (let ((varnum (func-next-varnum func)))
                        (gen-code func indent "value x~a = NIL;\n" varnum)
                        (let loop ((form form))
                          (if (null? form)
                              varnum
                              (let ((car-varnum (compile-quoted-item func indent (car form))))
                                (gen-code func indent "x~a = make_pair(x~a, x~a);\n" varnum car-varnum varnum)
                                (loop (cdr form)))))))
        (else (compile-error "unknown quoted value: ~s" form))))

(define (compile-quote func indent form)
  (if (!= (length form) 2)
      (compile-error "quote expects a single argument")
      (compile-quoted-item func indent (cadr form))))

(define (compile-quasiquote func indent form)
  (if (!= (length form) 2)
      (compile-error "quasiquote expects a single argument")
      (compile-form func indent (qq-quasiquote (cadr form)))))

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

(define (compile-lambda func indent form)
  (if (< (length form) 3)
      (compile-error "malformed lambda"))
  (let ((params-and-rest (parse-lambda-params form)))
    (let ((params (car params-and-rest))
          (rest-param (cadr params-and-rest)))
      (let ((new-func (add-function (func-program func)
                                    func
                                    params
                                    rest-param)))
        (if (eq? rest-param #f)
            (gen-code new-func 1 "if (nargs != ~a) RAISE(\"argument count mismatch\");\n" (length params))
            (gen-code new-func 1 "if (nargs < ~a) RAISE(\"too few arguments for function\");\n" (length params)))

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
              (gen-code new-func 1 "for (int i = 0; i < nargs - ~a; ++i) { value v = next_arg(); ~a = make_pair(v, ~a); }\n" (length params) (mangle-name rest-param) (mangle-name rest-param))))

        (gen-code new-func 1 "\n")

        ;; compile function body
        (let loop ((body (cddr form)) (varnum -1))
          (if (null? body)
              (begin
                (gen-code new-func 1 "free_args();\n")
                (gen-code new-func 1 "return x~a;\n" varnum))
              (let ((form (car body)))
                (loop (cdr body) (compile-form new-func 1 form)))))

        ;; generate the code for referencing the function
        (let ((varnum (func-next-varnum func)))
          (gen-code func indent "value x~a = make_closure(~a, ~a, ~a" varnum (func-name new-func) (length params) (length (func-freevars new-func)))
          (let loop ((freevars (reverse (func-freevars new-func))))
            (if (not (null? freevars))
                (begin
                  (if (func-has-param (car freevars))
                      (gen-code func 0 (mangle-name (car freevars))) ;; generate mangled param name
                      (let ((freevar (func-find-freevar (car freevars))))
                        (if freevar
                            (gen-code func 0 "envget(env, ~a)" freevar)
                            (gen-code func 0 "envget(env, ~a)" (func-add-freevar (car freevars))))))
                  (loop (cdr freevars)))))
          (gen-code func 0 ");\n")

          ;; return function varnum
          varnum)))))

(define (compile-define func indent form)
  (if (< (length form) 2)
      (compile-error "malformed define: ~s" form))
  (if (and (symbol? (cadr form))
           (> (length form) 3))
      (compile-error "malformed define: ~s" form))

  ;; we could technically allow this, and then further on, declare the
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
  (let ((form (if (list? (cadr form))
                  (list 'define
                        (caadr form)
                        (append (list 'lambda (cdadr form)) (cddr form)))
                  form)))
    (let ((name (cadr form))
          (init-form (if (< (length form) 3)
                         #f
                         (caddr form))))
      ;; check for re-definition
      (let loop ((params (func-params func)))
        (if (not (null? params))
            (if (eq? (car params) name)
                (compile-error "re-defining: ~a" name)
                (loop (cdr params)))))

      ;; add the name to the list of current function parameters
      (func-add-param func name)

      ;; copmile init form and either set it as a global variable (if in
      ;; the top-level), or declare it as a variable.
      (let ((init-varnum (compile-form func indent init-form)))
        ;; if not init value, we won't initialize here. all global
        ;; variables are initialized with VOID at the top-level.
        (if init-form
            (gen-code func indent "~a = x~a;\n" (mangle-name name) init-varnum))
        init-varnum))))

(define (compile-special-form func indent form)
  (case (car form)
    ((quote) (compile-quote func indent form))
    ((quasiquote) (compile-quasiquote func indent form))
    ((lambda) (compile-lambda func indent form))
    ((define) (compile-define func indent form))
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

(define (compile-list func indent form)
  ;; TODO fixme in the future. this function, strictly speaking, does
  ;; not work correctly. it won't allow binding or defining primcalls
  ;; and special forms.

  (let ((varnum (compile-primcall func indent form)))
    (if (negative? varnum)
          (let ((varnum (compile-special-form func indent form)))
            (if (negative? varnum)
                (compile-call func indent form)
                varnum))
          varnum)))

(define (compile-form func indent form)
  (cond ((number? form) (compile-number func indent form))
        ((string? form) (compile-string func indent form))
        ((boolean? form) (compile-bool func indent form))
        ((char? form) (compile-char func indent form))
        ((pair? form) (compile-list func indent form))
        (else (compile-error "don't know how to compile form: ~s" form))))

(define (compile-error fmt . args)
  (apply format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (compile-program program)
  (let ((func (add-function program #f '() #f))
        (port (program-port program)))
    (program-init-func-set! program func)
    (let loop ((form (read port)))
      (if (eof-object? form)
          (gen-code func 1 "return VOID;\n")
          (begin
            (compile-form func 1 form)
            (loop (read port)))))))

;;;;;; main ;;;;;;

(if (not (= 2 (length (command-line))))
    (compile-error "usage: ~a input-file" (car (command-line))))

(let ((port (open-input-file (cadr (command-line)))))
  (let ((program (create-program port)))
    (compile-program program)
    (output-program-code program)))
