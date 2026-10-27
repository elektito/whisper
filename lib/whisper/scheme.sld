(define-library (scheme base)
  (import (whisper))
  (include-library-declarations "scheme-base-exports.scm"))

(define-library (scheme cxr)
  (import (whisper))
  (include-library-declarations "scheme-cxr-exports.scm"))

(define-library (scheme char)
  (import (whisper))
  (include-library-declarations "scheme-char-exports.scm"))

(define-library (scheme case-lambda)
  (import (whisper))
  (include-library-declarations "scheme-case-lambda-exports.scm"))

(define-library (scheme file)
  (import (whisper))
  (include-library-declarations "scheme-file-exports.scm"))

(define-library (scheme inexact)
  (import (whisper))
  (include-library-declarations "scheme-inexact-exports.scm"))

(define-library (scheme lazy)
  (import (whisper))
  (include-library-declarations "scheme-lazy-exports.scm"))

(define-library (scheme process-context)
  (import (whisper))
  (include-library-declarations "scheme-process-context-exports.scm"))

(define-library (scheme read)
  (import (whisper))
  (include-library-declarations "scheme-read-exports.scm"))

(define-library (scheme time)
  (import (whisper))
  (include-library-declarations "scheme-time-exports.scm"))

(define-library (scheme write)
  (import (whisper))
  (include-library-declarations "scheme-write-exports.scm"))
