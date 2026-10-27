;; since this imports (whisper compiler) for eval, load and the
;; environment procedures, we put it in a separate library. if we put it
;; in scheme.sld, every program using (scheme base) would pull in the
;; whole compiler.
(define-library (scheme r5rs)
  (import (whisper) (whisper compiler))
  (include-library-declarations "scheme-r5rs-exports.scm"))
