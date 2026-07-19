(define-library (scheme base)
  (import (whisper))
  (include-library-declarations "lib/scheme-base-exports.scm"))

(define-library (scheme cxr)
  (import (whisper))
  (include-library-declarations "lib/scheme-cxr-exports.scm"))

(define-library (scheme char)
  (import (whisper))
  (include-library-declarations "lib/scheme-char-exports.scm"))

(define-library (scheme case-lambda)
  (import (whisper))
  (include-library-declarations "lib/scheme-case-lambda-exports.scm"))

(define-library (scheme file)
  (import (whisper))
  (include-library-declarations "lib/scheme-file-exports.scm"))

(define-library (scheme process-context)
  (import (whisper))
  (include-library-declarations "lib/scheme-process-context-exports.scm"))

(define-library (scheme write)
  (import (whisper))
  (include-library-declarations "lib/scheme-write-exports.scm"))
