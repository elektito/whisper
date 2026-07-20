(define-library (scheme eval)
  (import (whisper core))
  (include "whisper.scm")
  (begin
    ;; the compiler's only required runtime global: the library
    ;; provider.
    (init-find-library (resolve-library-search-path '())))
  (export environment eval))
