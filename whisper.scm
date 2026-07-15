(include "utils.scm")
(include "format.scm")
(include "qq.scm")
(include "expand.scm")
(include "syntax-rules.scm")

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
          ((char=? #\) ch) (compile-error "extra closing parenthesis"))
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
  (when (eof-object? (peek-char port))
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
  (make-program env ports funcs funcnum interned-symbols init-func is-test-suite test-counter debug library-mode libraries)
  program?
  (env program-env program-env-set!)
  (ports program-ports program-ports-set!)
  (funcs program-funcs program-funcs-set!)
  (funcnum program-funcnum program-funcnum-set!)
  (interned-symbols program-symbols program-symbols-set!)
  (init-func program-init-func program-init-func-set!)
  (is-test-suite program-is-test-suite program-is-test-suite-set!)
  (test-counter program-test-counter program-test-counter-set!)
  (debug program-debug program-debug-set!)
  (library-mode program-library-mode program-library-mode-set!)

  ;; in library mode (-l), one <library> record per define-library form
  ;; compiled so far, in reverse source order.
  (libraries program-libraries program-libraries-set!))

(define (find-library lib-name)
  (if (equal? lib-name '(whisper core))
      (make-library lib-name
                    '()
                    (map (lambda (id)
                           ;; we know root identifiers all have
                           ;; bindings, so we won't check for #f.
                           (let* ((b (identifier-binding id))
                                  (kind (binding-kind b))
                                  (meaning (binding-meaning b)))
                             (case kind
                               ((special aux primcall)
                                (list (identifier-name id) kind meaning))
                               (else
                                (compile-error "internal error: unhandled root identifier kind: ~a" kind)))))
                         *root-identifiers*)
                    '())
      #f))

(define (init-find-library)
  (set! *find-library* find-library))

(define (create-program port env program-mode?)
  (make-program (new-expand-root-env env program-mode?)
                (list port)
                '() ; funcs
                0   ; funcnum (function counter)
                '() ; interned symbols
                #f  ; init func, to be set later
                #f  ; is test suite?
                0   ; test counter
                #f  ; debug instrumentation
                #f  ; library mode
                '() ; libraries
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

(define (gen-func-prototypes program output)
  (let loop ((funcs (program-funcs program)))
    (unless (null? funcs)
      (format output "static value ~a(environment env, enum call_flags flags, int nargs, ...);\n" (func-name (car funcs)))
      (loop (cdr funcs)))))

(define (gen-func-bodies program output)
  (let loop ((funcs (program-funcs program)))
    (unless (null? funcs)
      (format output "static value ~a(environment env, enum call_flags flags, int nargs, ...) {\n" (func-name (car funcs)))
      (when (program-debug program)
        (format output "    enter_proc(~a);\n" (func-name (car funcs))))
      (display (get-output-string (func-port (car funcs))) output)
      (format output "}\n")
      (format output "\n")
      (loop (cdr funcs)))))

(define (intern-primcalls program)
  (for-each (lambda (p) (intern program (car p))) *primcalls*))

(define (gen-symbol-defines program output)
  (let loop ((symbols (program-symbols program)))
    (unless (null? symbols)
      (format output "static value symb~a;\n" (mangle-name (car symbols)))
      (loop (cdr symbols)))))

(define (gen-register-globals program output)
  (display "static void register_globals() {\n" output)
  (let loop ((symbols (program-symbols program)))
    (unless (null? symbols)
      (let* ((sym (car symbols))
             (primcall-info (hash-table-ref/default *primcalls-table* sym #f))
             (sym-name (symbol->string sym))
             (sym-len (string-length sym-name)))
        (format output "    symb~a = extend_global_env(\"~a\", ~a, ~a);\n"
                (mangle-name sym) sym sym-len
                (if primcall-info "sym_value" "sym_unbound"))
        (when primcall-info
          (let* ((c-name (cadr primcall-info))
                 (min-args (caddr primcall-info))
                 (max-args (cadddr primcall-info))
                 (c-max-args (if (= max-args -1) "MAX_ARGS" max-args)))
            (format output "    GET_SYMBOL(symb~a)->value = make_closure(primcall_~a, ~a, ~a, 0);\n"
                    (mangle-name sym) c-name min-args c-max-args)))
        (loop (cdr symbols)))))
  (display "}\n" output))

(define (output-program-code program filename)
  (intern-primcalls program)
  (let ((port (open-output-file filename)))
    (display "#include \"core.h\"\n\n" port)
    (gen-symbol-defines program port)
    (display "\nstatic value global_env;\n" port)
    (newline port)
    (gen-func-prototypes program port)
    (newline port)
    (gen-register-globals program port)
    (newline port)
    (gen-func-bodies program port)
    (if (program-library-mode program)
        (begin
          (display "#ifdef SO_MODE\n" port)
          (display "value whisper_main(value env) {\n" port)
          (display "    global_env = env;\n" port)
          (display "    register_globals();\n" port)
          (format port "    return ~a(NULL, NO_CALL_FLAGS, 0);\n" (func-name (program-init-func program)))
          (display "}\n" port)
          (display "#else\n" port)
          (display "static value _lib_init(value env) {\n" port)
          (display "    global_env = env;\n" port)
          (display "    register_globals();\n" port)
          (format port "    return ~a(NULL, NO_CALL_FLAGS, 0);\n" (func-name (program-init-func program)))
          (display "}\n\n" port)
          (display "static struct static_lib _lib_node = { _lib_init, NULL };\n\n" port)
          (display "STATIC_LIB_CONSTRUCTOR(_lib_ctor) {\n" port)
          (display "    register_static_lib(&_lib_node);\n" port)
          (display "}\n" port)
          (display "#endif\n" port))
        (begin
          (display "__attribute__((no_sanitize(\"address\")))\n" port)
          (display "int main(int argc, const char *argv[]) {\n" port)
          (display "    uint64_t ss;\n" port)
          (display "    stack_start = &ss;\n" port)
          (display "    init_memory();\n" port)
          (display "    init_symbols();\n" port)
          (display "    global_env = make_global_env();\n" port)
          (display "    register_globals();\n" port)
          (display "    init_ports();\n" port)
          (display "    run_static_libs(global_env);\n" port)
          (display "    cmdline_argc = argc;\n" port)
          (display "    cmdline_argv = argv;\n" port)
          (format port "    ~a(NULL, NO_CALL_FLAGS, 0);\n" (func-name (program-init-func program)))
          (display "}\n" port)))
    (close-port port)))

(define-record-type <function>
  (make-function port varnum name program parent freevars)
  function?
  (port func-port)
  (varnum func-varnum func-varnum-set!)
  (name func-name)
  (program func-program)
  (parent func-parent)
  (freevars func-freevars func-freevars-set!))

(define (add-function program parent)
  (let ((func (make-function (open-output-string) ; port
                             0  ; varnum
                             (format "f~a" (program-next-funcnum program)) ; name
                             program
                             parent
                             '() ; freevars
                             )))
    (program-add-function program func)
    func))

(define (func-next-varnum func)
  (let ((n (func-varnum func)))
    (func-varnum-set! func (+ n 1))
    n))

(define (func-add-freevar func binding)
  (let ((new-freevars (append (func-freevars func) (list binding))))
    (func-freevars-set! func new-freevars)

    ;; return the index of the new freevar
    (- (length new-freevars) 1)))

(define (func-find-freevar func binding)
  (let loop ((i 0) (freevars (func-freevars func)))
    (cond ((null? freevars) #f)
          ((eq? binding (car freevars)) i)
          (else (loop (+ i 1) (cdr freevars))))))

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

(define *primcalls* '((%make-hash-table "percent_make_hash_table" 2 2)
                      (apply "apply" 1 -1)
                      (boolean? "boolean_q" 1 1)
                      (box "box" 1 1)
                      (box? "box_q" 1 1)
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
                      (environment? "environment_q" 1 1)
                      (environment-bind! "environment_bind_b" 4 4)
                      (environment-lookup "environment_lookup" 2 2)
                      (eof-object? "eof_object_q" 1 1)
                      (eq? "eq_q" 2 2)
                      (error "error" 1 1)
                      (error-object? "error_object_q" 1 1)
                      (exit "exit" 0 1)
                      (file-error? "file_error_q" 1 1)
                      (gensym "gensym" 0 1)
                      (get-output-string "get_output_string" 1 1)
                      (get-environment-variable "get_environment_variable" 1 1)
                      (hash "hash" 1 2)
                      (hash-by-identity "hash_by_identity" 1 2)
                      (hash-table? "hash_table_q" 1 1)
                      (hash-table->alist "hash_table_to_alist" 1 1)
                      (hash-table-copy "hash_table_copy" 1 1)
                      (hash-table-delete! "hash_table_delete_b" 1 1)
                      (hash-table-equivalence-function "hash_table_equivalence_function" 1 1)
                      (hash-table-exists? "hash_table_exists_q" 2 2)
                      (hash-table-fold "hash_table_fold" 3 3)
                      (hash-table-hash-function "hash_table_hash_function" 1 1)
                      (hash-table-keys "hash_table_keys" 1 1)
                      (hash-table-merge! "hash_table_merge_b" 1 1)
                      (hash-table-ref "hash_table_ref" 2 3)
                      (hash-table-ref/default "hash_table_ref_default" 3 3)
                      (hash-table-set! "hash_table_set_b" 3 3)
                      (hash-table-size "hash_table_size" 1 1)
                      (hash-table-update! "hash_table_update_b" 3 4)
                      (hash-table-update! "hash_table_update_b" 3 4)
                      (hash-table-update!/default "hash_table_update_b_default" 4 4)
                      (hash-table-values "hash_table_values" 1 1)
                      (hash-table-walk "hash_table_walk" 2 2)
                      (input-port? "input_port_q" 1 1)
                      (integer->char "integer_to_char" 1 1)
                      (list-directory "list_directory" 1 1)
                      (make-empty-environment "make_empty_environment" 0 0)
                      (make-environment "make_environment" 0 0)
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
                      (run-so "run_so" 2 2)
                      (set-box! "set_box_b" 2 2)
                      (set-car! "set_car_b" 2 2)
                      (set-cdr! "set_cdr_b" 2 2)
                      (string-ci-hash "string_ci_hash" 1 2)
                      (string-ci=? "string_ci_eq_q" 2 -1)
                      (string-copy "string_copy" 1 3)
                      (string-hash "string_hash" 1 2)
                      (string->number "string_to_number" 1 2)
                      (string->symbol "string_to_symbol" 1 1)
                      (symbol->string "symbol_to_string" 1 1)
                      (string-append "string_append" 0 -1)
                      (string-length "string_length" 1 1)
                      (string-ref "string_ref" 2 2)
                      (string-set! "string_set_b" 3 3)
                      (string=? "string_eq_q" 2 -1)
                      (string? "string_q" 1 1)
                      (substring "substring" 3 3)
                      (symbol? "symbol_q" 1 1)
                      (system "system" 1 1)
                      (unread-char "unread_char" 1 2)
                      (urandom "urandom" 1 1)
                      (unbox "unbox" 1 1)
                      (unwrap "unwrap" 1 1)
                      (vector-length "vector_length" 1 1)
                      (vector-ref "vector_ref" 2 2)
                      (vector-set! "vector_set_b" 3 3)
                      (vector? "vector_q" 1 1)
                      (void "void" 0 0)
                      (void? "void_q" 1 1)
                      (wrap "wrap" 2 2)
                      (wrapped? "wrapped_q" 1 1)
                      (wrapped-kind "wrapped_kind" 1 1)
                      (wrapped-set-print "wrapped_set_print" 2 2)
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

(define *primcalls-table*
  (let ((ht (make-eq-hash-table)))
    (let loop ((lst *primcalls*))
      (unless (null? lst)
        (hash-table-set! ht (caar lst) (car lst))
        (loop (cdr lst))))
    ht))

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
  (let ((name (cond ((identifier? name)
                     (symbol->string (or (and (identifier-binding name)
                                              (binding-meaning (identifier-binding name)))
                                         (identifier-name name))))
                    ((symbol? name) (symbol->string name))
                    ((string? name) name)
                    (else (error "mangle-name argument not a string or symbol")))))
    (let loop ((i 0)
               (mangled "_"))
      (if (= i (string-length name))
          mangled
          (let ((ch (string-ref name i)))
            (cond ((or (char-alphabetic? ch) (char-numeric? ch))
                   (loop (+ i 1) (string-append-char mangled ch)))
                  ((char=? #\- ch) (loop (+ i 1) (string-append mangled "_")))
                  ((char=? #\_ ch) (loop (+ i 1) (string-append mangled "__")))
                  (else (loop (+ i 1) (format "~a_~a" mangled (char->integer ch))))))))))

(define (mangle-unique-name id)
  (mangle-name (binding-meaning (identifier-binding id))))

(define (mangle-unique-name-for-binding b)
  (mangle-name (binding-meaning b)))

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
        ((identifier? form) (let ((varnum (func-next-varnum func)))
                              (intern (func-program func) (identifier-name form))
                              (gen-code func indent "value x~a = symb~a;\n" varnum (mangle-name (identifier-name form)))
                              varnum))
        ((symbol? form) (let ((varnum (func-next-varnum func)))
                          (intern (func-program func) form)
                          (gen-code func indent "value x~a = symb~a;\n" varnum (mangle-name form))
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
    (if (identifier? params)
        (list '() params)
        (let loop ((proper-params '()) (params params))
          (cond ((null? params) (list (reverse proper-params) #f))
                ((atom? params) (list (reverse proper-params) params))
                (else (loop (cons (car params) proper-params) (cdr params))))))))

(define (compile-lambda func indent form)
  (let ((params-and-rest (parse-lambda-params form)))
    (let ((params (car params-and-rest))
          (rest-param (cadr params-and-rest)))
      (let ((new-func (add-function (func-program func) func)))
        ;; add binding owners
        (let loop ((params params))
          (unless (null? params)
            (binding-owner-set! (identifier-binding (car params)) new-func)
            (loop (cdr params))))
        (when rest-param
          (binding-owner-set! (identifier-binding rest-param) new-func))

        (if (eq? rest-param #f)
            (gen-code new-func 1 "if (nargs != ~a) RAISE(\"argument count mismatch: %s\", find_func_name(~a));\n" (length params) (func-name new-func))
            (gen-code new-func 1 "if (nargs < ~a) RAISE(\"too few arguments for function: %s\", find_func_name(~a));\n" (length params) (func-name new-func)))

        ;; generate code for reading arguments
        (gen-code new-func 1 "init_args();\n")
        (let loop ((params params))
          (unless (null? params)
            (gen-code new-func 1 "value ~a = next_arg();\n" (mangle-unique-name (car params)))
            (when (var-is-modified? (car params))
              (gen-code new-func 1 "~a = primcall_box(NULL, NO_CALL_FLAGS, 1, ~a);\n" (mangle-unique-name (car params)) (mangle-unique-name (car params))))
            (loop (cdr params))))

        (unless (eq? rest-param #f)
          (gen-code new-func 1 "value ~a = NIL;\n" (mangle-unique-name rest-param))
          (gen-code new-func 1 "for (int i = 0; i < nargs - ~a; ++i) { value v = next_arg(); ~a = make_pair(v, ~a); }\n" (length params) (mangle-name rest-param) (mangle-name rest-param))
          (gen-code new-func 1 "~a = reverse_list(~a, NIL);\n" (mangle-name rest-param) (mangle-name rest-param)))

        (gen-code new-func 1 "\n")

        ;; compile function body
        (let loop ((body (cddr form)) (varnum -1))
          (if (null? body)
              (begin
                (gen-code new-func 1 "free_args();\n")
                (when (program-debug (func-program func))
                  (gen-code new-func 1 "leave_proc();\n"))
                (gen-code new-func 1 "return x~a;\n" varnum))
              (let ((form (car body)))
                (loop (cdr body) (compile-form new-func 1 form)))))

        ;; generate the code for referencing the function
        (let ((varnum (func-next-varnum func)))
          (gen-code func indent "value x~a = make_closure(~a, ~a, ~a, ~a"
                    varnum
                    (func-name new-func)
                    (length params)
                    (if rest-param "MAX_ARGS" (length params))
                    (length (func-freevars new-func)))
          (let loop ((freevars (func-freevars new-func)))
            (unless (null? freevars)
              (let ((b (car freevars)))
                (if (eq? (binding-owner b) func)
                    (gen-code func 0 ", ~a" (mangle-unique-name-for-binding b))
                    (let ((idx (or (func-find-freevar func b)
                                   (func-add-freevar func b))))
                      (gen-code func 0 ", envget(env, ~a)" idx))))
              (loop (cdr freevars))))
          (gen-code func 0 ");\n")

          ;; return function varnum
          varnum)))))

(define (compile-let func indent form)
  (let ((let-varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = VOID;\n" let-varnum)
    (gen-code func indent "{\n")
    (let loop ((bindings (cadr form)))
      (unless (null? bindings)
        (binding-owner-set! (identifier-binding (caar bindings)) func)
        (let ((varnum (compile-form func (+ 1 indent) (cadar bindings))))
          (if (var-is-modified? (caar bindings))
              (gen-code func (+ 1 indent) "value ~a = primcall_box(NULL, NO_CALL_FLAGS, 1, x~a);\n" (mangle-unique-name (caar bindings)) varnum)
              (gen-code func (+ 1 indent) "value ~a = x~a;\n" (mangle-unique-name (caar bindings)) varnum))
          (loop (cdr bindings)))))
    (let loop ((body (cddr form))
               (last-varnum -1))
      (if (null? body)
          (gen-code func (+ 1 indent) "x~a = x~a;\n" let-varnum last-varnum)
          (loop (cdr body)
                (compile-form func (+ 1 indent) (car body)))))
    (gen-code func indent "}\n")
    let-varnum))

(define (compile-letrec func indent form)
  (let ((letrec-varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = VOID;\n" letrec-varnum)
    (gen-code func indent "{\n")

    ;; unconditionally mark all letrec vars as "modified vars" so
    ;; they're boxed and accessed by reference. without this, the
    ;; variables can be captured as VOID by lambdas and then changing
    ;; them wouldn't make any difference.
    (let ((prog (func-program func)))
      (let loop ((bindings (cadr form)))
        (unless (null? bindings)
          (binding-owner-set! (identifier-binding (caar bindings)) func)
          (mark-var-as-modified (caar bindings))
          (loop (cdr bindings)))))

    ;; declare all variables as boxed(void) (since they are all marked
    ;; as modified) and bind them all into scope so later init code can
    ;; use them
    (let loop ((bindings (cadr form)))
      (unless (null? bindings)
        (gen-code func (+ 1 indent) "value ~a = primcall_box(NULL, NO_CALL_FLAGS, 1, VOID);\n" (mangle-unique-name (caar bindings)))
        (loop (cdr bindings))))

    ;; evaluate all inits
    (let ((init-varnums
           (let loop ((bindings (cadr form)) (acc '()))
             (if (null? bindings)
                 (reverse acc)
                 (loop (cdr bindings)
                       (cons (compile-form func (+ 1 indent) (cadar bindings)) acc))))))
      ;; assign results
      (let loop ((bindings (cadr form)) (varnums init-varnums))
        (unless (null? bindings)
          (gen-code func (+ 1 indent) "primcall_set_box_b(NULL, NO_CALL_FLAGS, 2, ~a, x~a);\n"
                    (mangle-name (caar bindings)) (car varnums))
          (loop (cdr bindings) (cdr varnums))))

      ;; compile the body
      (let loop ((body (cddr form)) (last-varnum -1))
        (if (null? body)
            (gen-code func (+ 1 indent) "x~a = x~a;\n" letrec-varnum last-varnum)
            (loop (cdr body)
                  (compile-form func (+ 1 indent) (car body)))))

      (gen-code func indent "}\n")
      letrec-varnum)))

(define (compile-letrec* func indent form)
  (let ((letrec-varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = VOID;\n" letrec-varnum)
    (gen-code func indent "{\n")

    ;; unconditionally mark all letrec* vars as "modified vars" so
    ;; they're boxed and accessed by reference. without this, the
    ;; variables can be captured as VOID by lambdas and then changing
    ;; them wouldn't make any difference.
    (let ((prog (func-program func)))
      (let loop ((bindings (cadr form)))
        (unless (null? bindings)
          (binding-owner-set! (identifier-binding (caar bindings)) func)
          (mark-var-as-modified (caar bindings))
          (loop (cdr bindings)))))

    ;; declare all variables as boxed(void) and bring them all into
    ;; scope
    (let loop ((bindings (cadr form)))
      (unless (null? bindings)
        (gen-code func (+ 1 indent) "value ~a = primcall_box(NULL, NO_CALL_FLAGS, 1, VOID);\n" (mangle-unique-name (caar bindings)))
        (loop (cdr bindings))))

    ;; evaluate each initializer and assign immediately before moving to
    ;; the next
    (let loop ((bindings (cadr form)))
      (unless (null? bindings)
        (let ((varnum (compile-form func (+ 1 indent) (cadar bindings))))
          (gen-code func (+ 1 indent) "primcall_set_box_b(NULL, NO_CALL_FLAGS, 2, ~a, x~a);\n" (mangle-unique-name (caar bindings)) varnum))
        (loop (cdr bindings))))

    ;; compile the body
    (let loop ((body (cddr form)) (last-varnum -1))
      (if (null? body)
          (gen-code func (+ 1 indent) "x~a = x~a;\n" letrec-varnum last-varnum)
          (loop (cdr body)
                (compile-form func (+ 1 indent) (car body)))))

    (gen-code func indent "}\n")
    letrec-varnum))

(define (compile-define func indent form)
  (let ((name (cadr form))
        (init-form (caddr form)))
    ;; top-level variable names are all interned, since the values of
    ;; global variables are stored in the symbol table. intern the
    ;; binding's meaning (not the source name), since a macro-introduced
    ;; global's meaning is a hygienic gensym distinct from its source
    ;; name, and that is the symbol mangle-unique-name below actually
    ;; emits.
    (intern (func-program func) (binding-meaning (identifier-binding name)))

    ;; compile init form and either set it as a global variable (if in
    ;; the top-level), or declare it as a variable.
    (let ((init-varnum (compile-form func indent init-form)))
      (gen-code func indent "env_define(global_env, symb~a, x~a, sym_value);\n" (mangle-unique-name name) init-varnum)

      ;; define returns no meaningful value (unspecified in Scheme)
      -1)))

(define (compile-if func indent form)
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

(define (compile-include func indent form)
  (when (or (!= (length form) 2)
            (not (string? (cadr form))))
    (compile-error "bad include form: ~s" form))
  (let ((port (open-input-file (cadr form))))
    (program-push-port (func-program func) port))
  -1)

(define (compile-begin func indent form)
  (when (= 1 (length form))
    (compile-error "empty begin expression is not allowed"))
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

(define (compile-set! func indent form)
  (let ((value-varnum (compile-form func indent (caddr form)))
        (b (identifier-binding (cadr form))))
    (case (binding-kind b)
      ((global)
       (intern (func-program func) (binding-meaning b))
       (gen-code func indent "env_define(global_env, symb~a, x~a, sym_value);\n" (mangle-unique-name (cadr form)) value-varnum))
      ((lexical)
       (if (eq? (binding-owner b) func)
           (gen-code func indent "primcall_set_box_b(NULL, NO_CALL_FLAGS, 2, ~a, x~a);\n" (mangle-unique-name (cadr form)) value-varnum)
           (let ((idx (or (func-find-freevar func b)
                          (func-add-freevar func b))))
             (gen-code func indent "primcall_set_box_b(NULL, NO_CALL_FLAGS, 2, envget(env, ~a), x~a);\n" idx value-varnum))))
      (else
       (compile-error "set! target is not a variable: ~a" (cadr form)))))
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = VOID;\n" varnum)
    varnum))

(define (compile-special func indent form kind)
  (case kind
    ((begin) (compile-begin func indent form))
    ((define) (compile-define func indent form))
    ((include) (compile-include func indent form))
    ((if) (compile-if func indent form))
    ((let) (compile-let func indent form))
    ((letrec) (compile-letrec func indent form))
    ((letrec*) (compile-letrec* func indent form))
    ((lambda) (compile-lambda func indent form))
    ((quote) (compile-quote func indent form))
    ((set!) (compile-set! func indent form))
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

(define (lookup-primcall meaning)
  (hash-table-ref/default *primcalls-table* meaning #f))

(define (compile-list func indent form)
  (if (not (identifier? (car form)))
      (compile-call func indent form)
      (let* ((binding (identifier-binding (car form)))
             (meaning (binding-meaning binding)))
        (case (binding-kind binding)
          ((lexical global) (compile-call func indent form))
          ((primcall) (compile-primcall func indent form (lookup-primcall meaning)))
          ((special) (compile-special func indent form meaning))
          (else (error (format "unhandled identifier kind: ~a" meaning)))))))

(define (var-is-modified? var)
  (let ((b (identifier-binding var)))
    (and b (binding-mutated? b))))

(define (mark-var-as-modified var)
  (binding-mutated?-set! (identifier-binding var) #t))

(define (compile-identifier func indent form)
  (let ((varnum (func-next-varnum func))
        (b (identifier-binding form)))
    (case (binding-kind b)
      ((lexical)
       (if (eq? (binding-owner b) func)
           (if (binding-mutated? b)
               (gen-code func indent "value x~a = primcall_unbox(NULL, NO_CALL_FLAGS, 1, ~a);\n" varnum (mangle-unique-name form))
               (gen-code func indent "value x~a = ~a;\n" varnum (mangle-unique-name form)))
           (let ((idx (or (func-find-freevar func b)
                          (func-add-freevar func b))))
             (if (binding-mutated? b)
                 (gen-code func indent "value x~a = primcall_unbox(NULL, NO_CALL_FLAGS, 1, envget(env, ~a));\n" varnum idx)
                 (gen-code func indent "value x~a = envget(env, ~a);\n" varnum idx)))))
      ((global)
       (intern (func-program func) (binding-meaning b))
       (gen-code func indent "value x~a = env_ref(global_env, symb~a);\n" varnum (mangle-name form)))
      ((primcall)
       (intern (func-program func) (identifier-name form))
       (gen-code func indent "value x~a = GET_SYMBOL(symb~a)->value;\n"
                 varnum (mangle-name form)))
      ((special) (compile-error "invalid use of special: ~a" (identifier-name form)))

      ((macro) (compile-error "invalid use of macro name: ~a" (identifier-name form)))

      ((aux) (compile-error "invalid use of aux keyword: ~a" (identifier-name form)))

      (else (error (format "unknown identifier kind: ~a" (binding-kind b)))))
    varnum))

(define (compile-vector func indent form)
  (let ((varnum (func-next-varnum func)))
    (gen-code func indent "value x~a = make_vector(~a, VOID);\n" varnum (vector-length form))
    (let loop ((i 0))
      (when (< i (vector-length form))
        (let ((value-varnum (compile-quoted-item func indent (vector-ref form i))))
          (gen-code func indent "GET_OBJECT(x~a)->vector.data[~a] = x~a;\n" varnum i value-varnum)
          (loop (+ i 1)))))
    varnum))

(define (compile-form func indent form)
  (cond ((identifier? form) (compile-identifier func indent form))
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

(define *root-identifiers* (list (identifier 'special 'begin 'begin)
                                 (identifier 'special 'declare 'declare)
                                 (identifier 'special 'define 'define)
                                 (identifier 'special 'define-syntax 'define-syntax)
                                 (identifier 'special 'if 'if)
                                 (identifier 'special 'include 'include)
                                 (identifier 'special 'lambda 'lambda)
                                 (identifier 'special 'let 'let)
                                 (identifier 'special 'letrec 'letrec)
                                 (identifier 'special 'letrec* 'letrec*)
                                 (identifier 'special 'let-syntax 'let-syntax)
                                 (identifier 'special 'letrec-syntax 'letrec-syntax)
                                 (identifier 'special 'quasiquote 'quasiquote)
                                 (identifier 'special 'quote 'quote)
                                 (identifier 'special 'set! 'set!)
                                 (identifier 'special 'syntax-rules 'syntax-rules)
                                 (identifier 'aux 'else 'else)
                                 (identifier 'aux '=> '=>)
                                 (identifier 'aux '... '...)
                                 (identifier 'aux '_ '_)
                                 (identifier 'aux 'unquote 'unquote)
                                 (identifier 'aux 'unquote-splicing 'unquote-splicing)
                                 (identifier 'primcall '%make-hash-table '%make-hash-table)
                                 (identifier 'primcall 'apply 'apply)
                                 (identifier 'primcall 'boolean? 'boolean?)
                                 (identifier 'primcall 'box? 'box?)
                                 (identifier 'primcall 'box 'box)
                                 (identifier 'primcall 'car 'car)
                                 (identifier 'primcall 'cdr 'cdr)
                                 (identifier 'primcall 'char-downcase 'char-downcase)
                                 (identifier 'primcall 'char-upcase 'char-upcase)
                                 (identifier 'primcall 'char->integer 'char->integer)
                                 (identifier 'primcall 'char? 'char?)
                                 (identifier 'primcall 'close-port 'close-port)
                                 (identifier 'primcall 'command-line 'command-line)
                                 (identifier 'primcall 'cons 'cons)
                                 (identifier 'primcall 'current-input-port 'current-input-port)
                                 (identifier 'primcall 'current-output-port 'current-output-port)
                                 (identifier 'primcall 'current-error-port 'current-error-port)
                                 (identifier 'primcall 'delete-file 'delete-file)
                                 (identifier 'primcall 'display 'display)
                                 (identifier 'primcall 'environment? 'environment?)
                                 (identifier 'primcall 'environment-bind! 'environment-bind!)
                                 (identifier 'primcall 'environment-lookup 'environment-lookup)
                                 (identifier 'primcall 'eof-object? 'eof-object?)
                                 (identifier 'primcall 'eq? 'eq?)
                                 (identifier 'primcall 'error 'error)
                                 (identifier 'primcall 'error-object? 'error-object?)
                                 (identifier 'primcall 'exit 'exit)
                                 (identifier 'primcall 'file-error? 'file-error?)
                                 (identifier 'primcall 'gensym 'gensym)
                                 (identifier 'primcall 'get-output-string 'get-output-string)
                                 (identifier 'primcall 'get-environment-variable 'get-environment-variable)
                                 (identifier 'primcall 'hash 'hash)
                                 (identifier 'primcall 'hash-by-identity 'hash-by-identity)
                                 (identifier 'primcall 'hash-table? 'hash-table?)
                                 (identifier 'primcall 'hash-table->alist 'hash-table->alist)
                                 (identifier 'primcall 'hash-table-copy 'hash-table-copy)
                                 (identifier 'primcall 'hash-table-delete! 'hash-table-delete!)
                                 (identifier 'primcall 'hash-table-equivalence-function 'hash-table-equivalence-function)
                                 (identifier 'primcall 'hash-table-exists? 'hash-table-exists?)
                                 (identifier 'primcall 'hash-table-fold 'hash-table-fold)
                                 (identifier 'primcall 'hash-table-hash-function 'hash-table-hash-function)
                                 (identifier 'primcall 'hash-table-keys 'hash-table-keys)
                                 (identifier 'primcall 'hash-table-merge! 'hash-table-merge!)
                                 (identifier 'primcall 'hash-table-ref 'hash-table-ref)
                                 (identifier 'primcall 'hash-table-ref/default 'hash-table-ref/default)
                                 (identifier 'primcall 'hash-table-set! 'hash-table-set!)
                                 (identifier 'primcall 'hash-table-size 'hash-table-size)
                                 (identifier 'primcall 'hash-table-update! 'hash-table-update!)
                                 (identifier 'primcall 'hash-table-update!/default 'hash-table-update!/default)
                                 (identifier 'primcall 'hash-table-values 'hash-table-values)
                                 (identifier 'primcall 'hash-table-walk 'hash-table-walk)
                                 (identifier 'primcall 'input-port? 'input-port?)
                                 (identifier 'primcall 'integer->char 'integer->char)
                                 (identifier 'primcall 'list-directory 'list-directory)
                                 (identifier 'primcall 'make-empty-environment 'make-empty-environment)
                                 (identifier 'primcall 'make-environment 'make-environment)
                                 (identifier 'primcall 'make-string 'make-string)
                                 (identifier 'primcall 'make-vector 'make-vector)
                                 (identifier 'primcall 'newline 'newline)
                                 (identifier 'primcall 'number? 'number?)
                                 (identifier 'primcall 'number->string 'number->string)
                                 (identifier 'primcall 'open-input-file 'open-input-file)
                                 (identifier 'primcall 'open-output-file 'open-output-file)
                                 (identifier 'primcall 'open-output-string 'open-output-string)
                                 (identifier 'primcall 'pair? 'pair?)
                                 (identifier 'primcall 'peek-char 'peek-char)
                                 (identifier 'primcall 'port? 'port?)
                                 (identifier 'primcall 'procedure? 'procedure?)
                                 (identifier 'primcall 'read-char 'read-char)
                                 (identifier 'primcall 'read-line 'read-line)
                                 (identifier 'primcall 'run-so 'run-so)
                                 (identifier 'primcall 'set-box! 'set-box!)
                                 (identifier 'primcall 'set-car! 'set-car!)
                                 (identifier 'primcall 'set-cdr! 'set-cdr!)
                                 (identifier 'primcall 'string-ci-hash 'string-ci-hash)
                                 (identifier 'primcall 'string-ci=? 'string-ci=?)
                                 (identifier 'primcall 'string-copy 'string-copy)
                                 (identifier 'primcall 'string-hash 'string-hash)
                                 (identifier 'primcall 'string->number 'string->number)
                                 (identifier 'primcall 'string->symbol 'string->symbol)
                                 (identifier 'primcall 'symbol->string 'symbol->string)
                                 (identifier 'primcall 'string-append 'string-append)
                                 (identifier 'primcall 'string-length 'string-length)
                                 (identifier 'primcall 'string-ref 'string-ref)
                                 (identifier 'primcall 'string-set! 'string-set!)
                                 (identifier 'primcall 'string=? 'string=?)
                                 (identifier 'primcall 'string? 'string?)
                                 (identifier 'primcall 'substring 'substring)
                                 (identifier 'primcall 'symbol? 'symbol?)
                                 (identifier 'primcall 'system 'system)
                                 (identifier 'primcall 'unread-char 'unread-char)
                                 (identifier 'primcall 'urandom 'urandom)
                                 (identifier 'primcall 'unbox 'unbox)
                                 (identifier 'primcall 'unwrap 'unwrap)
                                 (identifier 'primcall 'vector-length 'vector-length)
                                 (identifier 'primcall 'vector-ref 'vector-ref)
                                 (identifier 'primcall 'vector-set! 'vector-set!)
                                 (identifier 'primcall 'vector? 'vector?)
                                 (identifier 'primcall 'void 'void)
                                 (identifier 'primcall 'void? 'void?)
                                 (identifier 'primcall 'wrap 'wrap)
                                 (identifier 'primcall 'wrapped? 'wrapped?)
                                 (identifier 'primcall 'wrapped-kind 'wrapped-kind)
                                 (identifier 'primcall 'wrapped-set-print 'wrapped-set-print)
                                 (identifier 'primcall 'write 'write)
                                 (identifier 'primcall 'write-char 'write-char)
                                 (identifier 'primcall '+ '+)
                                 (identifier 'primcall '- '-)
                                 (identifier 'primcall '* '*)
                                 (identifier 'primcall '/ '/)
                                 (identifier 'primcall '= '=)
                                 (identifier 'primcall '< '<)
                                 (identifier 'primcall '> '>)
                                 (identifier 'primcall '<= '<=)
                                 (identifier 'primcall '>= '>=)))

(define (compile-top-level-form func env form)
  (let ((expanded (expand-top-level-form form env)))
    (let loop ((forms expanded) (varnum -1))
      (if (null? forms)
          varnum
          (loop (cdr forms) (compile-form func 1 (car forms)))))))

(define (compile-program program)
  (let ((func (add-function program #f)))
    (program-init-func-set! program func)
    (if (eq? 'library (program-library-mode program))
        (compile-library program)
        (compile-executable program))
    (check-for-undefined-vars program)
    (commit-defines! (program-env program))))

(define (compile-executable program)
  (let ((func (program-init-func program))
        (env (program-env program)))
    (let loop ((form (read (program-port program))))
      (if (eof-object? form)
          (if (= (length (program-ports program)) 1)
              (begin
                (when (program-is-test-suite program)
                  (gen-code func 1 "printf(\"\\n\");\n"))
                (gen-code func 1 "return VOID;\n"))
              (begin
                (program-pop-port program)
                (loop (read (program-port program)))))
          (let ((varnum (compile-top-level-form func env form)))
            (when (and (!= varnum -1)
                       (program-is-test-suite program)
                       (program-is-main-file program))
              (gen-code func 1 "if (x~a == TRUE) { printf(\".\"); } else { printf(\"F(~a)\"); }\n" varnum (program-test-counter-inc! program)))
            (loop (read (program-port program))))))))

(define (compile-library program)
  (let loop ((form (read (program-port program))))
    (unless (eof-object? form)
      (unless (and (pair? form) (eq? (car form) 'define-library))
        (compile-error "a library file must contain only define-library forms"))
      (compile-library-definition form program)
      (loop (read (program-port program))))))

(define (read-all-forms path)
  (let ((port (open-input-file path)))
    (let loop ((form (read port)) (forms '()))
      (if (eof-object? form)
          (begin (close-port port) (reverse forms))
          (loop (read port) (cons form forms))))))

;; compiles each top-level form in lib-env; returns macros with any
;; define-syntax form's raw source appended, since the manifest needs
;; it.
(define (compile-forms-and-gather-macros func lib-env forms)
  (let loop ((macros '()) (forms forms))
    (if (null? forms)
        macros
        (let* ((form (car forms))
               (head (and (pair? form) (resolve-head (car form) lib-env))))
          (compile-top-level-form func lib-env form)
          (loop (if (and head (binding-is-special head 'define-syntax))
                    (cons form macros)
                    macros)
                (cdr forms))))))

;; parses one export spec into (local-name . export-name); a bare symbol
;; exports itself, (rename local export) exports under a different name.
(define (parse-export-spec spec)
  (cond ((symbol? spec) (cons spec spec))
        ((and (pair? spec) (eq? (car spec) 'rename) (= (length spec) 3))
         (cons (cadr spec) (caddr spec)))
        (else (compile-error "invalid export spec: ~a" spec))))

;; compiles one (define-library <name> <declaration> ...) form against
;; its own fresh <expand-root-env>, then resolves its exports and
;; appends a <library> record to the program.
(define (compile-library-definition form program)
  (let* ((lib-name (cadr form))
         (func (program-init-func program))
         (lib-env (new-expand-root-env (make-empty-environment) #f))
         (cu (expand-root-env-compilation-unit lib-env)))
    (compilation-unit-library-name-set! cu lib-name)
    (let loop ((decls (cddr form))
               (imports '())
               (export-names '())
               (macros '()))
      (if (null? decls)
          (begin
            (raise-if-undefined
             (compilation-unit-undefined-refs cu (expand-root-env-runtime-env lib-env) 'strict))
            (let ((exports (map (lambda (spec)
                                  (resolve-library-export lib-env (car spec) (cdr spec)))
                                export-names)))
              (program-libraries-set!
               program
               (cons (make-library lib-name (reverse imports) exports (reverse macros))
                     (program-libraries program)))))
          (let ((decl (car decls)))
            (unless (and (pair? decl) (symbol? (car decl)))
              (compile-error "invalid define-library declaration: ~a" decl))
            (case (car decl)
              ((import)
               (process-import decl lib-env)
               (loop (cdr decls) (cons decl imports) export-names macros))
              ((export)
               (loop (cdr decls) imports
                     (append export-names (map parse-export-spec (cdr decl))) macros))
              ((begin)
               (loop (cdr decls) imports export-names
                     (append (compile-forms-and-gather-macros func lib-env (cdr decl))
                             macros)))
              ((include)
               (loop (cdr decls) imports export-names
                     (append (compile-forms-and-gather-macros
                              func lib-env (apply append (map read-all-forms (cdr decl))))
                             macros)))
              ((include-library-declarations)
               (loop (append (apply append (map read-all-forms (cdr decl)))
                             (cdr decls))
                     imports export-names macros))
              ((include-ci)
               (compile-error "include-ci is not yet supported in define-library"))
              ((cond-expand)
               (compile-error "cond-expand is not yet supported in define-library"))
              (else
               (compile-error "unknown define-library declaration: ~a" (car decl)))))))))

;; classifies local-name as own (a defines-table hit) or inherited
;; (falls through to the library's imports), and returns its export
;; descriptor under export-name. own values/macros need no meaning
;; recorded: a value's is recomputed by mangling, a macro's transformer
;; is rebuilt from the manifest's macros list instead. inherited
;; primcall/special/aux need no origin tag, since those kinds are
;; global, not library-scoped.
;;
;; TODO: re-exporting an inherited value or macro needs an origin tag
;; (from <lib> <name>), which needs sym_alias in core.c plus (for
;; macros) tracking each import's origin through import-set resolution.
;; Neither is reachable yet: the only importable library, (whisper
;; core), exports neither kind. So deferred for now.
(define (resolve-library-export lib-env local-name export-name)
  (let* ((cu (expand-root-env-compilation-unit lib-env))
         (own (hash-table-ref/default (compilation-unit-defines cu) local-name #f)))
    (if own
        (let ((kind (case (binding-kind own)
                      ((global) 'value)
                      ((macro) 'macro)
                      (else (compile-error "internal error: unexpected own export kind: ~a" (binding-kind own))))))
          ;; a rename needs the local name too: a consumer mangles by
          ;; local name, and the manifest's macros list is keyed by it.
          (if (eq? local-name export-name)
              (list export-name kind)
              (list export-name kind local-name)))
        (let ((entry (environment-lookup (expand-root-env-runtime-env lib-env) local-name)))
          (unless entry
            (compile-error "exported name is neither defined nor imported: ~a" local-name))
          (let ((kind (car entry))
                (value (cdr entry)))
            (case kind
              ((primcall) (list export-name 'primcall value))
              ((special) (list export-name 'special value))
              ((aux) (list export-name 'aux value))
              ((alias)
               (compile-error "re-exporting an imported value is not supported yet: ~a" local-name))
              ((macro)
               (compile-error "re-exporting an imported macro is not supported yet: ~a" local-name))
              (else (compile-error "internal error: unknown export kind: ~a" kind))))))))

;; serialize this program's compiled libraries into one manifest.
(define (write-manifest program filename)
  (let ((port (open-output-file filename)))
    (write `(manifest
             ,@(map (lambda (lib)
                      `(library ,(library-name lib)
                         (imports ,@(library-imports lib))
                         (exports ,@(library-exports lib))
                         (macros ,@(library-macros lib))))
                    (reverse (program-libraries program))))
           port)
    (newline port)
    (close-port port)))

;; raises if any of the given identifiers are undefined; shared by
;; batch and eval's undefined-variable checks.
(define (raise-if-undefined undefined)
  (unless (null? undefined)
    (compile-error "undefined variable~a: ~a"
                   (if (= 1 (length undefined)) "" "s")
                   (string-join (map (lambda (id) (symbol->string (identifier-name id))) undefined) ", "))))

(define (check-for-undefined-vars program)
  ;; only checked for ordinary executables: .so mode may refer to
  ;; variables in an environment passed to it, and library mode already
  ;; checks each library's own body in compile-library-definition.
  (unless (program-library-mode program)
    (let ((root-env (program-env program)))
      (raise-if-undefined
       (compilation-unit-undefined-refs
        (expand-root-env-compilation-unit root-env)
        (expand-root-env-runtime-env root-env)
        'strict)))))

(define (build-compile-cmd cc library-mode cflags c-file out-file core-path)
  (case library-mode
    ((so)
     (format "~a -I~a ~a -DSO_MODE -fPIC -shared -o ~a ~a" cc core-path cflags out-file c-file))
    ((library)
     (let* ((obj (string-append (temp-filename) ".o"))
            (so-cmd (format "~a -I~a ~a -DSO_MODE -fPIC -shared -o ~a.so ~a" cc core-path cflags out-file c-file))
            (obj-cmd (format "~a -I~a ~a -fPIC -c -o ~a ~a" cc core-path cflags obj c-file))
            (ar-cmd (format "rm -f ~a.a && ar rcs ~a.a ~a" out-file out-file obj)))
       (string-join (list so-cmd obj-cmd ar-cmd) " && ")))
    (else
     (format "~a -I~a -ldl ~a -Wl,--export-dynamic -o ~a ~a ~a/core.c" cc core-path cflags out-file c-file core-path))))

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

(define (compile-expr-to-so expr env)
  (let* ((c-file (string-append (temp-filename) ".c"))
         (so-file (string-append (temp-filename) ".so"))
         (program (create-program #f env #f))
         (func (add-function program #f))
         (root-env (program-env program)))
    (program-init-func-set! program func)
    (program-library-mode-set! program 'so)
    (let ((varnum (compile-top-level-form func root-env expr)))
      (if (negative? varnum)
          (gen-code func 1 "return VOID;\n")
          (gen-code func 1 "return x~a;\n" varnum)))
    (raise-if-undefined
     (compilation-unit-undefined-refs
      (expand-root-env-compilation-unit root-env) env 'eval))
    (output-program-code program c-file)
    (let ((cc (or (get-environment-variable "CC") "gcc"))
          (core-path (or (get-environment-variable "WHISPER_HOME") ".")))
      (let ((ret (system (build-compile-cmd cc 'so "" c-file so-file core-path))))
        (delete-file c-file)
        (unless (zero? ret)
          (error (format "gcc returned non-zero exit code: ~a\n" ret)))))
    (commit-defines! root-env)
    so-file))

(define (eval expr env)
  (let ((so (compile-expr-to-so expr env)))
    (let ((result (run-so so env)))
      (delete-file so)
      result)))
