;; This file implements a macro expander for whisper.
;;
;; The expander is fed top-level forms via expand-top-level-form
;; function and for each returns a *list* of expanded forms. A list is
;; returned in case there is a splicing "begin".
;;
;; The expander does the following:
;;  - Read and track macro definitions (local or top-level).
;;  - Expand all forms into core forms.
;;  - The following constructs are eliminated:
;;    + define-syntax
;;    + let-syntax
;;    + letrec-syntax
;;    + internal defines (turned into letrec*)
;;    + named let (turned into letrec)
;;    + splicing begin (just spliced in place, at the top-level or block
;;      body)
;;    + quasiquote
;;  - Replace all symbols with identifier objects.
;;  - (define foo) is desugared to (define foo (void))
;;  - (define (f . x) <body>) is desugared to (define f (lambda x <body>))
;;
;; Identifier Objects
;; ==================
;;
;; An identifier object is a record with the following fields:
;;  - name: The original name as it appears in the code
;;  - rename: Either #f or another symbol used as the unique name of the
;;    identifier. this is only supposed to be used at expansion time.
;;    This is a per-expansion rename which is different from the
;;    meaning field we have per binding (see below).
;;  - binding: a binding object which specifies the "denotation" of the
;;    identifier.
;;
;; Binding Objects
;; ===============
;;
;; A binding object has the following fields:
;;  - kind: one of the following symbols: special, aux, primcall,
;;    global, lexical, macro.
;;  - meaning: depends on kind: the canonical name for special/aux/
;;    primcall, a transformer record for macro, a gensym for lexical,
;;    and the binder key (see binder-key) for global. Two non-lexical
;;    bindings with the same kind and meaning are considered to denote
;;    the same thing regardless of object identity (see
;;    binding-denotes-same? / free-identifier=?); lexical is the only
;;    kind where the binding object's own identity matters.
;;  - mutated?: denotes whether the identifier is ever set!'ed. only
;;    applies to lexicals.
;;  - owner: to be used by codegen
;; The expander creates a binding object per name introduced by a
;; binding construct like let or lambda. The same binding object will be
;; attached to all instances of its use in the binding construct body.
;;
;; How to use the expander
;; =======================
;; 1. The compiler sets the global *find-library* to a function that can
;;    lookup and return library descriptions.
;; 2. It then creates an empty runtime environment object, and wraps
;;    it in a fresh <expand-root-env>, pairing it with a fresh
;;    <compilation-unit> that buffers this compilation's top-level
;;    defines and references until the compilation either succeeds or
;;    fails.
;; 3. The compiler reads top-level forms one-by-one. "include"
;;    directives are to be handled directly by the compiler. Everything
;;    else is passed to the expand-top-level-form function, along with
;;    the root env.
;; 4. expand-top-level-form returns a list of expanded forms per each
;;    form passed to it. This is necessary because the expander removes
;;    splicing begin forms.
;; 5. The expander never loads or links library code. It only records
;;    each directly imported library on the compilation unit. After
;;    expansion, the compiler reads compilation-unit-imports (see also
;;    libraries-in-dependency-order) and makes the code available in
;;    whatever way suits it. In whisper that means linking .a files
;;    for a batch compile, and dlopening .so files for eval.
;; 6. Once compilation succeeds, commit-defines! writes the unit's
;;    macro definitions into the environment object so they are visible
;;    to a later compilation (eval) against the same environment. A
;;    failed compilation leaves the environment untouched.
;;
;; Lowering
;; ========
;;
;; The following lowering happen in the expander:
;;
;; 1. Splicing begin
;; "begin" at the top-level or at the let/lambda body is spliced
;; directly in:
;;
;; (begin
;;   (display "Hi!")
;;   (newline))
;; =>
;; (display "Hi!")
;; (newline)
;;
;; (let ()
;;   (begin
;;     (display "Hi!")
;;     (newline)
;;     (begin
;;       (display "Hi there!"))
;;     (newline)))
;; =>
;; (let ()
;;   (display "Hi!")
;;   (newline)
;;   (display "Hi there!")
;;   (newline))
;;
;; 2. Eliminating named let
;; Named let is converted to letrec:
;;
;; (let loop ((i 10))
;;   (if (zero? i)
;;       (display "done!")
;;       (loop (- i 1))))
;; =>
;; (letrec ((loop (lambda (i)
;;                  (if (zero? i)
;;                      (display "done!")
;;                      (loop (- i 11))))))
;;   (loop 10))
;;
;; 3. Internal defines turned into letrec*
;;
;; (let ()
;;   (define (is-odd? n)
;;     (if (zero? n) #f (is-even? (- n 1))))
;;   (define (is-even n)
;;     (if (zero? n) #t (is-odd? (- n 1))))
;;   (display (is-even? 4)))
;; =>
;; (let ()
;;   (letrec* ((is-odd? (lambda (n)
;;                        (if (zero? n) #f (is-even? (- n 1)))))
;;             (is-even? (lambda (n)
;;                         (if (zero? n) #t (is-odd? (- n 1))))))
;;     (display (is-even? 4))))
;;
;; 4. macro defining/binding constructs entirely eliminated
;;
;; (let-syntax ((m (syntax-rules ()
;;                   ((_ . rest) 42))))
;;   (m))
;; =>
;; 42
;;
;; (define-syntax m
;;   (syntax-rules ()
;;     ((_ . rest) 42)))
;; (m)
;; =>
;; 42
;; m is also buffered in the compilation unit's defines, and committed
;; to the environment object once compilation succeeds (see
;; commit-defines!), so a later compilation against the same environment
;; (i.e. a later eval) can see it too
;;
;; (let ()
;;   (define-syntax m
;;     (syntax-rules ()
;;       ((_ . rest) 42)))
;;   (m))
;; =>
;; (let ()
;;   42)
;;

;;;;;; globals to be injected ;;;;;;

;; this must be set to a function that receives a library name and
;; returns a <library> record.
(define *find-library*)

;;;;;; private globals ;;;;;;

;; loaded libraries, as entries of the form (lib-name lib results) where
;; lib is the <library> record and results the export alist returned by
;; import-library. also used to resolve dependencies when a compilation
;; unit's imports are walked (see libraries-in-dependency-order).
(define *import-cache* '())

(define *libs-being-imported* '())

;;;;;; identifier ;;;;;;

(define-record-type <identifier>
  (%make-identifier name rename binding)
  identifier?

  ;; the original name of the identifier, as in the source code
  (name identifier-name)

  ;; a unique alias for this identifier. used for template-introduced
  ;; identifiers. is set to #f otherwise.
  (rename identifier-rename identifier-rename-set!)

  ;; a binding object specifying the "denotation" of this identifier
  (binding identifier-binding identifier-binding-set!))

(record-set-print <identifier>
                  (lambda (id port)
                    (let ((b (identifier-binding id)))
                      (if b
                          (format port "[~s/~s|~s/~s]"
                                  (identifier-name id)
                                  (identifier-rename id)
                                  (binding-kind b)
                                  (binding-meaning b))
                          (format port "[~s/~s|-]"
                                  (identifier-name id)
                                  (identifier-rename id))))))

(define (make-identifier name binding)
  (%make-identifier name #f binding))

;; create an identifier with a binding already attached
(define (identifier kind name meaning)
  (make-identifier name (new-binding kind meaning)))

;;;;;; binding ;;;;;;

(define-record-type <binding>
  (make-binding kind meaning)
  binding?

  ;; one of: special, aux, primcall, global, lexical, macro, alias
  (kind binding-kind)

  ;; depends on kind:
  ;;  - for special: the canonical name (begin, lambda, etc.)
  ;;  - for aux: the canonical name (else, ..., etc.)
  ;;  - for primcalls: the canonical name (car, cdr, etc.)
  ;;  - for globals: the full name. currently the name itself but in the
  ;;    future might be a mangled name containing library name too.
  ;;  - for lexicals: a unique name; a gensym.
  ;;  - for macros: a transformer object
  (meaning binding-meaning binding-meaning-set!)

  ;; whether this binding has ever been set!'ed and hence needs boxing.
  ;; this is probably only useful for lexicals since globals are always
  ;; modifiable even without boxing.
  (mutated? binding-mutated? binding-mutated?-set!)

  ;; not used by the expander but the codegen can store the lambda
  ;; associated with the identifier for its own purposes here
  (owner binding-owner binding-owner-set!))

(record-set-print <binding>
                  (lambda (b port)
                    (format port "B[~s/~s]" (binding-kind b) (binding-meaning b))))

(define (new-binding kind meaning)
  (let ((b (make-binding kind meaning)))
    (binding-mutated?-set! b #f)
    (binding-owner-set! b #f)
    b))

(define (binding-denotes-same? bx by)
  (or (eq? bx by)
      (and (not (eq? 'lexical (binding-kind bx)))
           (eq? (binding-kind bx) (binding-kind by))
           (eq? (binding-meaning bx) (binding-meaning by)))))

(define (free-identifier=? x y)
  (let ((bx (identifier-binding x))
        (by (identifier-binding y)))
    (cond ((and bx by)
           (or (eq? bx by)
               (binding-denotes-same? bx by)))
          ((or bx by) #f)
          (else (eq? (identifier-name x)
                     (identifier-name y))))))

(define (binding-is-special binding meaning)
  (and (eq? 'special (binding-kind binding))
       (eq? meaning (binding-meaning binding))))

(define (identifier-is-special form meaning env)
  (cond ((identifier? form)
         (let ((b (identifier-binding form)))
           (and b (eq? 'special (binding-kind b)) (eq? meaning (binding-meaning b)))))
        ((symbol? form)
         (let ((b (expand-env-lookup env form)))
           (and b (eq? 'special (binding-kind b)) (eq? meaning (binding-meaning b)))))
        (else #f)))

(define (identifier-is-aux form meaning env)
  (cond ((identifier? form)
         (let ((b (identifier-binding form)))
           (and b (eq? 'aux (binding-kind b)) (eq? meaning (binding-meaning b)))))
        ((symbol? form)
         (let ((b (expand-env-lookup env form)))
           (and b (eq? 'aux (binding-kind b)) (eq? meaning (binding-meaning b)))))
        (else #f)))

;;;;;; transformer ;;;;;;

(define-record-type <transformer>
  (make-transformer def-env proc)
  transformer?
  (def-env transformer-def-env transformer-def-env-set!)
  (proc transformer-proc transformer-proc-set!))

;;;;;; expand environment ;;;;;;

(define-record-type <expand-env>
  (make-expand-env identifiers parent)
  expand-env?

  ;; a list of identifier objects, denoting the names available in this
  ;; environment.
  (identifiers expand-env-identifiers expand-env-identifiers-set!)

  ;; the parent environment. #f for top-level environment.
  (parent expand-env-parent expand-env-parent-set!))

(define (new-expand-env)
  (make-expand-env '() #f))

;;;;;; compilation unit ;;;;;;

;; per-compilation-unit buffer for top-level defines and references.
;; nothing here reaches the environment object until commit-defines!
;; runs after a successful compilation, so a failed unit leaves no trace
;; behind.
(define-record-type <compilation-unit>
  (make-compilation-unit defines refs program-mode? past-imports? seen-import? library-name imports import-origins)
  compilation-unit?

  ;; mapping name (binder key) to binding record, for defines/declares/
  ;; define-syntax seen textually earlier in this unit
  (defines compilation-unit-defines)

  ;; mapping name to ref-info, recorded during expansion for the
  ;; undefined variable check
  (refs compilation-unit-refs)

  ;; whether this compilation unit belongs to a program or not. in a
  ;; program, at least one import is required at the beginning. this is
  ;; set to #f for repl compilation units as well as inside libraries
  ;; where imports are separte from library source.
  (program-mode? compilation-unit-program-mode?)

  ;; flag to indicate whether we've encountered the first non-import
  ;; form in this compilation unit or not.
  (past-imports? compilation-unit-past-imports? compilation-unit-past-imports?-set!)

  ;; flag to indicate whether at least one import form has been
  ;; processed in this compilation unit.
  (seen-import? compilation-unit-seen-import? compilation-unit-seen-import?-set!)

  ;; #f outside library compilation, otherwise the name of the library
  ;; currently being compiled.
  (library-name compilation-unit-library-name compilation-unit-library-name-set!)

  ;; <library> records for the libraries directly imported in this
  ;; unit, most recently imported first. the expander only records
  ;; these. the host reads them after expansion to make library code
  ;; available to its target (see libraries-in-dependency-order).
  (imports compilation-unit-imports compilation-unit-imports-set!)

  ;; mapping a locally-imported value/macro name to the (origin-lib .
  ;; origin-name) pair it was originally exported under, for names
  ;; imported as kind 'value or 'macro. populated by process-import.
  ;; consulted by resolve-library-export when a library re-exports an
  ;; imported name.
  (import-origins compilation-unit-import-origins))

(define (new-compilation-unit program-mode?)
  (make-compilation-unit (make-hash-table) (make-hash-table) program-mode? #f #f #f '() (make-hash-table)))

;;;;;; expand root env ;;;;;;

;; the root frame of an expand-env parent chain. pairs the runtime
;; environment object (the durable half, living as long as the repl
;; session and shared across evals) with a fresh compilation-unit (the
;; transient half, holding this compilation's buffered defines and
;; refs).
;;
;; the environment object is never mutated during expansion. its only
;; writers are commit-defines! (which writes macro entries to the
;; runtime environment, on successful compilation) and executed code
;; (for values, via env_define). A transformer's captured def-env may
;; hold a stale root-env whose unit table no longer reflects later
;; evals; that is safe only because compilation unit part is not used
;; for macro expansion.
(define-record-type <expand-root-env>
  (make-expand-root-env runtime-env compilation-unit)
  expand-root-env?

  ;; the runtime environment object
  (runtime-env expand-root-env-runtime-env)

  ;; this compilation's <compilation-unit>
  (compilation-unit expand-root-env-compilation-unit))

(define (new-expand-root-env env program-mode?)
  (make-expand-root-env env (new-compilation-unit program-mode?)))

;; write this compilation's macro defines into the runtime environment,
;; making them persist across evals. call only after a successful
;; compilation; value entries need no commit since executed code writes
;; them itself via env_define, and a failed unit's defines are simply
;; discarded along with the rest of the unit.
(define (commit-defines! root-env)
  (let ((env (expand-root-env-runtime-env root-env))
        (defines (compilation-unit-defines (expand-root-env-compilation-unit root-env))))
    (hash-table-walk defines
                      (lambda (name binding)
                        (when (eq? 'macro (binding-kind binding))
                          (environment-bind! env name 'macro (binding-meaning binding)))))))

;; fabricate a <binding> from an environment-lookup result. meaning is
;; the canonical name for special/aux/primcall, the transformer for
;; macro, and the binder key itself for globals (matching
;; expand-top-level-define, where meaning = binder key always).
(define (root-binding-from-entry kind value name)
  (case kind
    ((special) (new-binding 'special value))
    ((aux) (new-binding 'aux value))
    ((primcall) (new-binding 'primcall value))
    ((macro) (new-binding 'macro value))
    ((value) (new-binding 'global name))
    ;; kept distinct from global (not reused, not tracked as a possible
    ;; forward reference), even though codegen treats them the same.
    ((alias) (new-binding 'alias value))
    (else (compile-error "internal error: unknown environment-lookup kind ~s" kind))))

;; resolves a name at the root: first the compilation unit's own defines
;; (same-compilation-unit visibility, including forward references
;; within the unit), then the environment object, fabricating a binding
;; from its (kind . value) pair.
;;
;; On a full miss, when compiling a library body and the caller allows
;; it (fallback? is true), the name is assumed to be a forward reference
;; to one of the library's own not-yet-seen top-level defines and gets a
;; fabricated global binding with the mangled name. Otherwise #f.
(define (expand-root-env-lookup root-env name fallback?)
  (let ((cu (expand-root-env-compilation-unit root-env)))
    (or (hash-table-ref/default (compilation-unit-defines cu) name #f)
        (let ((entry (environment-lookup (expand-root-env-runtime-env root-env) name)))
          (if entry
              (root-binding-from-entry (car entry) (cdr entry) name)
              (and fallback?
                   (let ((library-name (compilation-unit-library-name cu)))
                     (and library-name
                          (new-binding 'global (library-mangle-name library-name name))))))))))

;; lookup name (which is a symbol) in the given expand environment and
;; return a binding object associated with it if found. #f otherwise.
;; env may also be #f, meaning "no real environment" rather than a
;; missing parent: qq.scm calls identifier-is-aux/identifier-is-special
;; with a literal #f env while walking the already-processed qq-token
;; tree, where a name can never be a real binding, so this must answer
;; unbound rather than error.
;;
;; ORDERING CONTRACT: identifiers are scanned front to back and the
;; first match wins. expand-env-add-identifier! prepends, so a name
;; added later shadows an earlier one with the same name. Sequential
;; top-level redefinition relies on this: e.g. redefining a primcall
;; adds a new global identifier in front of the primcall, and every
;; later reference then resolves to the global. Do not reorder the
;; identifier list or change the prepend/first-match convention without
;; accounting for this.
(define (expand-env-lookup env name)
  (expand-env-lookup-walk env name #t))

;; like expand-env-lookup, but for looking up an identifier's rename. A
;; rename is a compiler-generated key that only a binder can put in the
;; environment (via binder-key), never a source-written name, so it can
;; never denote a forward reference to a library's own top-level
;; defines. A full miss must therefore answer #f so callers fall back to
;; the identifier's own attached binding. Letting the library
;; forward-reference fallback fire here would mask that binding with a
;; bogus fresh global, breaking every free template identifier (if,
;; recursive macro self-references, value references) inside a library
;; body.
(define (expand-env-lookup-rename env name)
  (expand-env-lookup-walk env name #f))

(define (expand-env-lookup-walk env name fallback?)
  (cond ((not env) #f)
        ((expand-root-env? env) (expand-root-env-lookup env name fallback?))
        (else
         (let loop ((ids (expand-env-identifiers env)))
           (cond ((null? ids)
                  (expand-env-lookup-walk (expand-env-parent env) name fallback?))
                 ((eq? (identifier-name (car ids)) name)
                  (identifier-binding (car ids)))
                 (else (loop (cdr ids))))))))

;; like expand-env-lookup but only checks the current frame, not its
;; parents. used to detect duplicate definitions within a single body,
;; where shadowing an outer binding is fine but redefining a name in the
;; same body is an error. Same front-to-back, first-match ordering
;; contract as expand-env-lookup (see there). lexical frames only; the
;; compilation unit's defines table is what plays this role at the root.
(define (expand-env-lookup-local env name)
  (when (expand-root-env? env)
    (compile-error "internal error: expand-env-lookup-local called on root env"))
  (let loop ((ids (expand-env-identifiers env)))
    (cond ((null? ids) #f)
          ((eq? (identifier-name (car ids)) name)
           (identifier-binding (car ids)))
          (else (loop (cdr ids))))))

;; prepend an identifier to the frame. Prepending (rather than
;; appending) is required by the lookup ordering contract: a later
;; addition shadows an earlier same-named one because expand-env-lookup
;; returns the first match. See expand-env-lookup. lexical frames only;
;; top-level define/define-syntax/declare write the compilation unit's
;; defines table or the environment object directly instead.
(define (expand-env-add-identifier! env id)
  (when (expand-root-env? env)
    (compile-error "internal error: expand-env-add-identifier! called on root env"))
  (expand-env-identifiers-set! env (cons id (expand-env-identifiers env))))

;; the undefined-variable check for one compilation unit. mode is
;; 'strict (batch) or 'eval. a ref passes if its name is in the unit's
;; defines, or (eval only) already bound in the live environment from a
;; prior eval. of what's left: strict flags every one, eval only flags
;; immediate refs (deferred ones may still resolve at a later eval and
;; raise "unbound variable" at run time if they don't). returns the
;; list of offending identifiers. library (-l) builds never call this.
(define (compilation-unit-undefined-refs unit env mode)
  (let ((defines (compilation-unit-defines unit))
        (undefined '()))
    (hash-table-walk (compilation-unit-refs unit)
                      (lambda (name entry)
                        (let ((id (car entry))
                              (immediate? (cdr entry)))
                          (unless (or (hash-table-ref/default defines name #f)
                                      (and (eq? mode 'eval) (environment-lookup env name)))
                            (when (or (eq? mode 'strict) immediate?)
                              (set! undefined (cons id undefined)))))))
    undefined))


;;;;;; helpers ;;;;;;

(define (symbol-or-identifier? x)
  (or (symbol? x) (identifier? x)))

;; the env key a binder is inserted under (and references resolve by)
(define (binder-key name)
  (cond ((symbol? name) name)
        ((identifier-rename name) (identifier-rename name))
        (else (identifier-name name))))

;; the original source symbol, for readable meanings/diagnostics
(define (binder-source-name name)
  (if (symbol? name) name (identifier-name name)))

;; a fresh lexical identifier for a binder, gensym-ing a unique name
(define (lexical-binder formal)
  (make-identifier (binder-key formal)
                   (new-binding 'lexical (gensym (symbol->string (binder-source-name formal))))))

;; helper for resolving the head of a list
(define (resolve-head head env)
  (cond ((identifier? head)
         (if (identifier-rename head)
             (or (expand-env-lookup-rename env (identifier-rename head))
                 (identifier-binding head))
             (identifier-binding head)))
        ((symbol? head) (expand-env-lookup env head))
        (else #f)))

;; walks the chain of parents in the given expand-env until it finds
;; the <expand-root-env> at the top.
(define (find-global-env env)
  (if (expand-root-env? env)
      env
      (find-global-env (expand-env-parent env))))

;; record that global `id` was referenced, for the undefined-variable
;; check. refs maps name to (id . immediate?-so-far); immediate? is #t
;; when the reference occurs directly at the top level (root-env? env),
;; as opposed to nested inside a lambda/let/etc. Only immediate refs
;; can be flagged before running anything in eval mode; deferred ones
;; are legitimate forward references (e.g. a function body calling a
;; not-yet-defined helper) and are left to raise at run time if still
;; unresolved when reached. Once #t for a name it stays #t.
;;
;; accepted edge: immediate? is root-env? at the reference site, so a
;; reference inside a bare (let () ...) at the top level counts as
;; deferred even though it runs immediately, same as one under a real
;; lambda. Caught at run time instead of statically; not worth
;; special-casing non-lambda frames for.
(define (record-global-ref! root-env id immediate?)
  (let* ((name (identifier-name id))
         (refs (compilation-unit-refs (expand-root-env-compilation-unit root-env)))
         (prior (hash-table-ref/default refs name #f)))
    (hash-table-set! refs name (cons id (or immediate? (and prior (cdr prior)))))))

;;;;;; libraries ;;;;;;

(define-record-type <library>
  (make-library name imports exports macros defines code-handle)
  library?

  ;; the library name, e.g. (foo bar)
  (name library-name)

  ;; a list of import forms this library depends on
  (imports library-imports)

  ;; a list of export descriptors that look like this:
  ;;     (spam value)
  ;;     (xcar primcall car)
  ;;     (mac macro)
  ;;     (kw aux kw)
  (exports library-exports)

  ;; a flat list of define-syntax forms, for all macros (public or
  ;; private) in this library
  (macros library-macros)

  ;; a flat list of the library's own top-level value names (plain local
  ;; names, unmangled). exported macro templates may reference any of
  ;; these, so the importer must know they exist.
  (defines library-defines)

  ;; opaque token the provider understands, or #f when there's no
  ;; artifact to provide (a builtin library, or a code-free one).
  (code-handle library-code-handle))

(define (library-mangle-name lib-name name)
  (define (mangle-part part)
    (cond ((integer? part) (format "i~a" part))
          ((symbol? part) (let* ((s (symbol->string part))
                                 (len (string-length s)))
                            (if (< len 10)
                                (format "~a~a" len s)
                                (format "[~a]~a" len s))))
          (else (compile-error "internal error: invalid library name part: ~s" part))))
  (string->symbol
   (string-append (format "##~a-" (length lib-name))
                  (string-join (map mangle-part lib-name) "-")
                  "-"
                  (symbol->string name))))

;;;;;; expander ;;;;;;

;; expand a top-level into a _list_ of core forms. The return value is a
;; list of forms because a splicing begin is eliminated and is turned
;; into a list of expanded forms.
(define (expand-top-level-form form env)
  (let* ((cu (expand-root-env-compilation-unit env))
         (program-mode? (compilation-unit-program-mode? cu))
         (past-imports? (compilation-unit-past-imports? cu)))
    (cond
     ((and (not past-imports?)
           (pair? form)
           (eq? (car form) 'import))
      (process-import form env)
      (compilation-unit-seen-import?-set! cu #t)
      '())
     ((not past-imports?)
      (when (and program-mode? (not (compilation-unit-seen-import? cu)))
        (compile-error "a program must begin with at least one import"))
      (compilation-unit-past-imports?-set! cu #t)
      (expand-top-level-form form env))
     ((atom? form)
      (list (expand-form form env)))
     (else
      (let ((head (car form)))
        (if (symbol-or-identifier? head)
            (let ((binding (resolve-head head env)))
              (cond ((not binding)
                     (list (expand-form form env)))
                    ((binding-is-special binding 'define)
                     (list (expand-top-level-define binding form env)))
                    ((binding-is-special binding 'define-syntax)
                     (process-define-syntax form env)
                     '())
                    ((binding-is-special binding 'begin)
                     (apply append (map (lambda (form)
                                          (expand-top-level-form form env))
                                        (cdr form))))
                    ((eq? 'macro (binding-kind binding))
                     (expand-top-level-form (expand-macro binding form env) env))
                    (else (list (expand-form form env)))))
            (list (expand-form form env))))))))

(define (process-import form env)
  (let loop ((import-sets (cdr form)))
    (unless (null? import-sets)
      (let loop-alist ((alist (process-import-set (car import-sets))))
        (unless (null? alist)
          (let* ((entry (car alist))
                 (name (car entry))
                 (kind (cadr entry))
                 (value (caddr entry))
                 (origin (cadddr entry)))
            (environment-bind! (expand-root-env-runtime-env env)
                               name
                               (if (eq? kind 'value) 'alias kind)
                               value)
            (when origin
              (hash-table-set! (compilation-unit-import-origins
                                 (expand-root-env-compilation-unit env))
                                name
                                origin))
            (loop-alist (cdr alist)))))
      (record-import! env (import-set-library-name (car import-sets)))
      (loop (cdr import-sets)))))

;; record a directly imported library on env's compilation unit. the
;; library is in the cache by now, since process-import-set just
;; imported it.
(define (record-import! env lib-name)
  (let ((cu (expand-root-env-compilation-unit env))
        (lib (import-cache-lookup lib-name)))
    (unless (memq lib (compilation-unit-imports cu))
      (compilation-unit-imports-set! cu (cons lib (compilation-unit-imports cu))))))

(define (process-import-set form)
  (unless (list? form)
    (compile-error "invalid import-set: ~a" form))
  (case (car form)
    ((only) (process-import-set-only form))
    ((except) (process-import-set-except form))
    ((prefix) (process-import-set-prefix form))
    ((rename) (process-import-set-rename form))
    (else (process-import-set-library form))))

(define (process-import-set-library lib-name)
  (let loop ((rest lib-name))
    (if (atom? rest)
        (unless (null? rest)
          (compile-error "invalid library name: ~a" lib-name))
        (if (or (symbol? (car rest)) (integer? (car rest)))
            (loop (cdr rest))
            (compile-error "invalid library name: ~a" lib-name))))
  (import-library lib-name))

(define (process-import-set-only/except form clause-name keep?)
  (let* ((base-import-set (cadr form))
         (base-bindings (process-import-set base-import-set))
         (names (cddr form)))
    (unless (all? (map symbol? names))
      (compile-error "not all imported names are symbols: ~a" form))
    (unless (all? (map (lambda (x)
                         (assq x base-bindings))
                       names))
      (compile-error "not all import-~a names are in the base import-set" clause-name))
    (filter (lambda (x)
              (let ((name (car x)))
                (if keep?
                    (memq name names)
                    (not (memq name names)))))
            base-bindings)))

(define (process-import-set-only form)
  (process-import-set-only/except form "only" #t))

(define (process-import-set-except form)
  (process-import-set-only/except form "except" #f))

(define (process-import-set-rename form)
  (unless (>= (length form) 2)
    (compile-error "invalid 'import rename' clause: ~a" form))
  (let* ((base-import-set (cadr form))
         (base-bindings (process-import-set base-import-set))
         (renames (cddr form))
         (renames (map (lambda (x)
                         (unless (and (list? x) (= 2 (length x)))
                           (compile-error "invalid import rename clause: ~a" x))
                         (cons (car x) (cadr x)))
                       renames)))
    (for-each (lambda (x)
                (unless (and (symbol? (car x)) (symbol? (cdr x)))
                  (compile-error "invalid import rename: ~a" (list (car x) (cdr x))))
                (unless (assq (car x) base-bindings)
                  (compile-error "rename-from not in base import-set: ~a" (car x))))
              renames)
    (map (lambda (x)
           (let ((rename-entry (assq (car x) renames)))
             (if rename-entry
                 (cons (cdr rename-entry) (cdr x))
                 x)))
         base-bindings)))

(define (process-import-set-prefix form)
  (when (null? (cddr form))
    (compile-error "no prefix specified for 'import prefix' clause: ~a" form))
  (let* ((base-import-set (cadr form))
         (base-bindings (process-import-set base-import-set))
         (prefix (caddr form)))
    (unless (symbol? prefix)
      (compile-error "import prefix is not a symbol: ~a" form))
    (let ((prefix-str (symbol->string prefix)))
      (map (lambda (x)
             (cons (string->symbol (string-append prefix-str (symbol->string (car x))))
                   (cdr x)))
           base-bindings))))

;; strip only/except/prefix/rename wrappers off an import-set, leaving
;; the library name.
(define (import-set-library-name form)
  (case (car form)
    ((only except prefix rename) (import-set-library-name (cadr form)))
    (else form)))

;; return the <library> record cached for the given name, or #f if the
;; library has not been imported yet.
(define (import-cache-lookup lib-name)
  (let ((desc (assoc lib-name *import-cache*)))
    (and desc (cadr desc))))

;; return the given <library> records plus all their transitive
;; dependencies, with dependencies before dependents. hosts use this
;; on a compilation unit's recorded imports to decide load or link
;; order. dependencies are resolved through the import cache, which
;; has an entry for every library imported so far.
(define (libraries-in-dependency-order libs)
  (let ((seen '())
        (result '()))
    (define (visit lib)
      (unless (member (library-name lib) seen)
        (set! seen (cons (library-name lib) seen))
        (for-each (lambda (import-form)
                    (for-each (lambda (import-set)
                                (visit (import-cache-lookup
                                        (import-set-library-name import-set))))
                              (cdr import-form)))
                  (library-imports lib))
        (set! result (cons lib result))))
    (for-each visit libs)
    (reverse result)))

;; resolve one export descriptor from lib-name's manifest into an
;; import-library entry (name kind value origin). a from-tagged
;; descriptor is a re-export, resolved through origin-lib (already
;; loaded); otherwise it's own to lib-name, keyed by local name.
(define (resolve-import-binding x lib-name macs)
  (let ((name (car x))
        (kind (cadr x)))
    (if (and (pair? (cddr x)) (eq? 'from (caddr x)))
        (let* ((origin-lib (cadddr x))
               (origin-name (car (cddddr x)))
               (value (case kind
                        ((value) (library-mangle-name origin-lib origin-name))
                        ((macro) (caddr (assq origin-name (import-library origin-lib))))
                        (else (compile-error "unexpected re-exported kind: ~a" kind)))))
          (cons name (list kind value (cons origin-lib origin-name))))
        (let* ((local-name (if (null? (cddr x)) name (caddr x)))
               (value (case kind
                        ((primcall special aux) (caddr x))
                        ((macro) (cdr (assq local-name macs)))
                        ((value) (library-mangle-name lib-name local-name))
                        (else (compile-error "unknown library export type: ~a" kind))))
               (origin (case kind
                         ((value macro) (cons lib-name local-name))
                         (else #f))))
          (cons name (list kind value origin))))))

(define (import-library lib-name)
  (define (object->string obj)
    (let ((port (open-output-string)))
      (write obj port)
      (get-output-string port)))
  (let ((desc (assoc lib-name *import-cache*)))
    (if desc
        (caddr desc)
        (begin
          (when (member lib-name *libs-being-imported*)
            (compile-error "circular dependencies: ~a -> ~a"
                           (string-join (map object->string (reverse *libs-being-imported*)) " -> ")
                           lib-name))
          (set! *libs-being-imported* (cons lib-name *libs-being-imported*))
          (let ((lib (*find-library* lib-name))
                (lib-env (new-expand-root-env (make-empty-environment) #f)))
            (unless lib
              (compile-error "could not find library: ~a" lib-name))
            (let loop ((imports (library-imports lib)))
              (unless (null? imports)
                (process-import (car imports) lib-env)
                (loop (cdr imports))))
            (for-each (lambda (name)
                        (environment-bind! (expand-root-env-runtime-env lib-env)
                                           name
                                           'alias
                                           (library-mangle-name lib-name name)))
                      (library-defines lib))
            (let* ((macs (compile-lib-macros lib lib-env))
                   (results (map (lambda (x) (resolve-import-binding x lib-name macs))
                                 (library-exports lib))))
              (set! *libs-being-imported* (cdr *libs-being-imported*))
              (set! *import-cache* (cons (list lib-name lib results) *import-cache*))
              results))))))

;; receives a library description as returned by *find-library*, and
;; returns an alist with elements in the form (<macro-name> . <transformer>).
(define (compile-lib-macros lib env)
  (let loop ((macs (library-macros lib))
             (results '()))
    (if (null? macs)
        results
        (let ((name (cadar macs))
              (transformer (process-define-syntax (car macs) env)))
          (loop (cdr macs) (cons (cons name transformer) results))))))

;; expand the given list form, the head of which is known to be a macro.
;; head-binding is the pre-resolved binding of the head of the list,
;; that is the binding containing the macro transformer.
;;
;; expansion is not recursive here and we depend on this behavior in the
;; expand-body function.
(define (expand-macro head-binding form env)
  (let* ((transformer (binding-meaning head-binding))
         (proc (transformer-proc transformer)))
    (proc form env)))

;; expand a top-level define form and add it to the given global
;; environment. head-binding is the pre-resolved binding of the "define"
;; keyword at the head of the given list. shorthand function definitions
;; like (define (f . formals) <body>) are desugared to normal define
;; with lambda. valueless defines like (define x) are converted to
;; (define x (void)).
(define (expand-top-level-define head-binding form env)
  (let ((form (validate-and-normalize-define form env)))
    (let* ((name (cadr form))
           (key (binder-key name))
           (existing (expand-env-lookup env key))
           ;; only reuse an existing binding if it is already a global,
           ;; that is a forward reference we are now defining. if the
           ;; name currently denotes a primcall, special, macro, or aux,
           ;; a top-level define introduces a fresh global that shadows
           ;; it.
           (reuse? (and existing (eq? 'global (binding-kind existing))))
           (library-name (and (expand-root-env? env)
                               (compilation-unit-library-name (expand-root-env-compilation-unit env))))
           (binding (cond (reuse? existing)
                          (library-name (new-binding 'global (library-mangle-name library-name key)))
                          (else (new-binding 'global key))))
           (name (make-identifier key binding)))
      (hash-table-set! (compilation-unit-defines (expand-root-env-compilation-unit env)) key binding)
      (let ((define (if (identifier? (car form))
                        (car form)
                        (make-identifier (car form) head-binding)))
            (expanded-value (expand-form (caddr form) env)))
        (list define name expanded-value)))))

;; validate the given define form and then normalize it so it's always
;; in the form of (define name value).
;;
;; This means that the following:
;;     (define (name . formals) body1 body2 ...)
;; is converted to:
;;     (define name (lambda formals body1 body2 ...))
;; And the following:
;;     (define name)
;; is converted to:
;;     (define name (void))
(define (validate-and-normalize-define form env)
  (let ((form-len (length form)))
    (when (or (= form-len 1)
              (and (pair? (cadr form))
                   (< form-len 3))
              (and (or (symbol? (cadr form))
                       (identifier? (cadr form)))
                   (> form-len 3))
              (and (not (pair? (cadr form)))
                   (not (symbol? (cadr form)))
                   (not (identifier? (cadr form)))))
      (compile-error "invalid define form")))

  (if (pair? (cadr form))
      (let ((define (car form))
            (name (caadr form))
            (lambda (identifier 'special 'lambda 'lambda))
            (formals (cdadr form))
            (body (cddr form)))
        `(,define ,name (,lambda ,formals ,@body)))
      (if (= 3 (length form))
          form
          (append form (list (list (identifier 'primcall 'void 'void)))))))

(define (process-define-syntax form env)
  (let* ((transformer (compile-transformer (caddr form) env))
         (key (binder-key (cadr form)))
         (binding (new-binding 'macro transformer)))
    (if (expand-root-env? env)
        (hash-table-set! (compilation-unit-defines (expand-root-env-compilation-unit env)) key binding)
        (expand-env-add-identifier! env (make-identifier key binding)))
    transformer))

(define (compile-transformer form env)
  (let ((b (resolve-head (car form) env)))
    (unless (and b (binding-is-special b 'syntax-rules))
      (compile-error "invalid macro transformer (only syntax-rules supported)")))
  (compile-syntax-rules form env))

(define (expand-form form env)
  (cond ((identifier? form)
         ;; if there's a rename, look it up in current local environment
         ;; first to see if it's beeing rebound by the same template
         ;; that introduced it.
         (if (identifier-rename form)
             (let ((b (expand-env-lookup-rename env (identifier-rename form))))
               (if b
                   (make-identifier (identifier-name form) b)
                   ;; free template identifier: could denote a global
                   ;; (record it, meaning = source name already denotes
                   ;; the right thing) or a special/primcall/aux from
                   ;; the definition environment (e.g. a macro template
                   ;; referencing if/begin/error), which is already
                   ;; resolved and must not be tracked as a reference.
                   (begin
                     (when (eq? 'global (binding-kind (identifier-binding form)))
                       (record-global-ref! (find-global-env env) form (expand-root-env? env)))
                     form)))
             form))
        ((symbol? form)
         (let* ((binding (or (expand-env-lookup env form) (new-binding 'global form)))
                (id (make-identifier form binding)))
           (when (eq? 'global (binding-kind binding))
             (record-global-ref! (find-global-env env) id (expand-root-env? env)))
           id))
        ((atom? form) form)
        (else ;; pair
         (let* ((head (car form))
                (head-binding (resolve-head head env)))
           (cond ((not head-binding) (expand-other form env))
                 ((eq? 'macro (binding-kind head-binding))
                  (expand-form (expand-macro head-binding form env) env))
                 ((binding-is-special head-binding 'quote)
                  (list (if (identifier? head)
                            head
                            (make-identifier head head-binding))
                        (cadr form)))
                 ((binding-is-special head-binding 'quasiquote)
                  (expand-quasiquote form env))
                 ((binding-is-special head-binding 'set!)
                  (process-set! head-binding form env))
                 ((binding-is-special head-binding 'define)
                  (compile-error "define in expression position"))
                 ((binding-is-special head-binding 'define-syntax)
                  (compile-error "define-syntax in expression position"))
                 ((binding-is-special head-binding 'lambda)
                  (expand-lambda head-binding form env))
                 ((binding-is-special head-binding 'let)
                  (expand-let head-binding form env))
                 ((binding-is-special head-binding 'letrec)
                  (expand-letrec head-binding form env))
                 ((binding-is-special head-binding 'letrec*)
                  (expand-letrec* head-binding form env))
                 ((binding-is-special head-binding 'let-syntax)
                  (expand-let-syntax head-binding form env))
                 ((binding-is-special head-binding 'letrec-syntax)
                  (expand-letrec-syntax head-binding form env))
                 (else (expand-other form env)))))))

;; expands a list which can be a procedure application or a special form
;; that does not introduce new bindings or otherwise need special
;; handling. we simply map over the list with expand-form.
(define (expand-other form env)
  (map (lambda (form)
         (expand-form form env))
       form))

(define (expand-quasiquote form env)
  (qq-quasiquote form env))

(define (process-set! head-binding form env)
  (unless (= 3 (length form))
    (compile-error "invalid set! form"))
  (let ((target (expand-form (cadr form) env)))
    (unless (identifier? target)
      (compile-error "invalid set! target"))
    (binding-mutated?-set! (identifier-binding target) #t)
    (list (if (identifier? (car form))
              (car form)
              (make-identifier (car form) head-binding))
          target
          (expand-form (caddr form) env))))

(define (expand-lambda head-binding form env)
  (when (< (length form) 3)
    (compile-error "invalid lambda form"))

  ;; first convert all formals to a list of ids (including a dotted list
  ;; or just a name to capture all parameters)
  (let ((ids (let loop ((formals (cadr form)) (ids '()))
               (if (pair? formals)
                   (loop (cdr formals)
                         (begin
                           (unless (symbol-or-identifier? (car formals))
                             (compile-error "bad formal: ~s" (car formals)))
                           (cons (lexical-binder (car formals)) ids)))
                   (if (null? formals)
                       (reverse ids)
                       (begin
                         (unless (symbol-or-identifier? formals)
                           (compile-error "bad formal: ~s" formals))
                         (reverse (cons (lexical-binder formals) ids))))))))
    ;; then create a child environment with those ids
    (let ((new-env (make-expand-env ids env)))
      ;; and then expand the body in the new environment
      (let ((body (expand-body (cddr form) new-env)))
        `(,(if (identifier? (car form))
               (car form)
               (make-identifier (car form) head-binding))
          ,(let loop ((formals (cadr form)) (ids ids))
             (cond ((null? formals) '())
                   ((pair? formals) (cons (car ids) (loop (cdr formals) (cdr ids))))
                   (else (car ids))))
          ,@body)))))

(define (expand-let head-binding form env)
  (cond ((< (length form) 3)
         (compile-error "invalid let form"))
        ((and (not (list? (cadr form)))
              (not (symbol-or-identifier? (cadr form))))
         (compile-error "invalid let form"))
        ((and (symbol-or-identifier? (cadr form))
              (not (list? (caddr form))))
         (compile-error "invalid let form"))
        ((and (symbol-or-identifier? (cadr form))
              (< (length form) 4))
         (compile-error "invalid let form"))
        ((let ((bindings (if (symbol-or-identifier? (cadr form))
                             (caddr form)
                             (cadr form))))
           (not (all? (map (lambda (x)
                             (and (list? x)
                                  (= (length x) 2)
                                  (symbol-or-identifier? (car x))))
                           bindings))))
         (compile-error "invalid let bindings")))

  (if (symbol-or-identifier? (cadr form))
      ;; named let
      (let ((letrec (identifier 'special 'letrec 'letrec))
            (name (cadr form))
            (lambda (identifier 'special 'lambda 'lambda))
            (vars (map car (caddr form)))
            (inits (map cadr (caddr form)))
            (body (cdddr form)))
        (expand-form `(,letrec ((,name (,lambda (,@vars)
                                         ,@body)))
                       (,name ,@inits))
                     env))
      ;; regular let
      ;;  - expand inits in env.
      ;;  - replace var names with lexical identifiers
      ;;  - add var identifiers to a new child env
      ;;  - expand body
      (let* ((bindings (cadr form))
             (inits (map cadr bindings))
             (expanded-inits (map (lambda (form)
                                    (expand-form form env))
                                  inits))
             (vars (map (lambda (b) (lexical-binder (car b))) bindings))
             (expanded-bindings (map list vars expanded-inits))
             (new-env (make-expand-env vars env))
             (body (cddr form)))
        `(,(if (identifier? (car form))
               (car form)
               (make-identifier (car form) head-binding))
          (,@expanded-bindings)
          ,@(expand-body body new-env)))))

(define (%expand-letrec form-name head-binding form env)
  (cond ((< (length form) 3)
         (compile-error "bad ~a form" form-name))
        ((not (list? (cadr form)))
         (compile-error "bad ~a form" form-name))
        ((not (all? (map (lambda (x)
                           (and (list? x)
                                (= (length x) 2)
                                (symbol-or-identifier? (car x))))
                         (cadr form))))
         (compile-error "bad ~a bindings" form-name)))
  (let* ((bindings (cadr form))
         (vars (map (lambda (b) (lexical-binder (car b))) bindings))
         (new-env (make-expand-env vars env))
         (inits (map cadr bindings))
         (expanded-inits (map (lambda (form)
                                (expand-form form new-env))
                              inits))
         (expanded-bindings (map list vars expanded-inits))
         (body (cddr form)))
    `(,(if (identifier? (car form))
           (car form)
           (make-identifier (car form) head-binding))
      (,@expanded-bindings)
      ,@(expand-body body new-env))))

(define (expand-letrec head-binding form env)
  (%expand-letrec "letrec" head-binding form env))

(define (expand-letrec* head-binding form env)
  ;; from the expander's point of view, letrec* is exactly the same as
  ;; letrec
  (%expand-letrec "letrec*" head-binding form env))

(define (%expand-let-syntax form-name recursive? head-binding form env)
  (when (< (length form) 3)
    (compile-error "invalid ~a form" form-name))

  ;; - create a new env with the macro names bound to empty macro
  ;;   bindings
  ;; - compile each transformer and patch it into its binding. for
  ;;   letrec-syntax the transformers are compiled in the new-env so
  ;;   they can see each other and themselves. for let-syntax they are
  ;;   compiled in the outer env so they cannot.
  ;; - expand body and return it as (begin ...) form
  (let* ((bindings (cadr form))
         (ids (map (lambda (b)
                     (make-identifier (binder-key (car b))
                                      (new-binding 'macro #f)))
                   bindings))
         (new-env (make-expand-env ids env))
         (def-env (if recursive? new-env env)))
    (for-each (lambda (b id)
                (binding-meaning-set! (identifier-binding id)
                                      (compile-transformer (cadr b) def-env)))
              bindings ids)
    (let ((expanded-body (expand-body (cddr form) new-env)))
      `(,(identifier 'special 'begin 'begin)
        ,@expanded-body))))

(define (expand-let-syntax head-binding form env)
  (%expand-let-syntax "let-syntax" #f head-binding form env))

(define (expand-letrec-syntax head-binding form env)
  (%expand-let-syntax "letrec-syntax" #t head-binding form env))

;; this functions expands the body of a lambda/let/letrec/letrec* per
;; the following rules:
;;
;; pass 1:
;;  - create a new child environment for internal definitions
;;  - go through each body level expression and classify each form by
;;    looking at the head. if it's a macro, replace the form with its
;;    expanded form until we reach something else.
;;  - if it's a define-syntax, compile it and install it in the internal
;;    environment.
;;  - if it's a define, normalize it (i.e. desugar it so that after
;;    define there's always a name), and collect it _unexpanded_, and
;;    add it to internal env.
;;  - if it's a begin, replace it with its contents.
;;  - if we hit a non-define/define-syntax after expansion, stop pass 1.
;;    note that we should keep this last expression as the starting
;;    point of pass 2.
;;
;; pass 2:
;;  - expand defines fully now and hold onto the results.
;;  - expand the remaining parts of the body normally in the inner
;;    environment. "begin" is now a normal expression, not splicing,
;;    and define/define-syntax are errors.
;;  - after we reach the end, check that there's actually a body after
;;    defines, otherwise throw an error about having an empty body.
;;  - if there are any internal defines collected in pass 1, convert them
;;    into a letrec* with define inits as letrec* variable inits, and
;;    the body as letrec* body, otherwise return the expanded body
;;    expressions.
(define (expand-body body env)
  (let ((internal-env (make-expand-env '() env)))
    (let* ((forms-defines (scan-body-for-defines body internal-env))
           (forms (car forms-defines))
           (defines (cdr forms-defines))
           (expanded-defines (let loop ((defines defines) (expanded '()))
                               (if (null? defines)
                                   (reverse expanded)
                                   (let ((name (caar defines))
                                         (init (cdar defines)))
                                     (loop (cdr defines)
                                           (cons (cons name (expand-form init internal-env))
                                                 expanded)))))))
      (let loop ((forms forms) (expanded '()))
        (if (null? forms)
            (if (null? defines)
                (reverse expanded)
                (let ((bindings (map (lambda (x)
                                       (list (car x) (cdr x)))
                                     expanded-defines)))
                  (list `(,(identifier 'special 'letrec* 'letrec*)
                          (,@bindings)
                          ,@(reverse expanded)))))
            (loop (cdr forms)
                  (cons (expand-form (car forms) internal-env)
                        expanded)))))))

;; scan body for defines and define-syntax forms. the given environment
;; is the internal env inside a lambda/let/etc. macros are immediately
;; installed in the environment. defines are collected, normalzied but
;; unexpanded.
;;
;; Returns the (forms . defines) in which forms is the rest of the body
;; when the first non-define form is reached, and defines is an alist of
;; (name . init-form) for each define form collected, name being an
;; identifier.
(define (scan-body-for-defines body env)
  (let loop ((forms body) (defines '()))
    (when (null? forms)
      (compile-error "empty body"))
    (let* ((form (car forms))
           (head-binding (and (pair? form)
                              (resolve-head (car form) env))))
      (cond ((not head-binding) (cons forms (reverse defines)))
            ((eq? 'macro (binding-kind head-binding))
             (loop (cons (expand-macro head-binding form env)
                         (cdr forms))
                   defines))
            ((binding-is-special head-binding 'define-syntax)
             (process-define-syntax form env)
             (loop (cdr forms) defines))
            ((binding-is-special head-binding 'define)
             (let* ((define-form (validate-and-normalize-define form env))
                    (define-name (cadr define-form))
                    (name-identifier (lexical-binder define-name)))
               (expand-env-add-identifier! env name-identifier)
               (loop (cdr forms) (cons (cons name-identifier (caddr define-form))
                                       defines))))
            ((binding-is-special head-binding 'begin)
             (loop (append (cdar forms) (cdr forms)) defines))
            (else ;; hit an expression
             (cons forms (reverse defines)))))))
