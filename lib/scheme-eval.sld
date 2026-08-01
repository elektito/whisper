(define-library (scheme eval)
  (import (whisper core))
  (include "../whisper.scm")
  (begin
    ;; the compiler's required runtime globals: the library provider and
    ;; the include reader.
    (init-find-library (resolve-library-search-path '()))
    (init-read-included-file (resolve-include-search-path '())))
  (export environment eval))
