(define-library (srfi 151)
  (import (scheme base))
  (import (scheme case-lambda))

  ;; non-core operations are implemented in scheme, copied directly from
  ;; the srfi.
  (include "bitwise-33.scm")
  (include "bitwise-60.scm")
  (include "bitwise-other.scm")

  (export bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2
          bitwise-orc1 bitwise-orc2)
  (export bitwise-if bit-set? copy-bit bit-swap any-bit-set?
          every-bit-set?  first-set-bit)
  (export bit-field bit-field-any? bit-field-every?  bit-field-clear bit-field-set
          bit-field-replace  bit-field-replace-same
          bit-field-rotate bit-field-reverse)
  (export bits->list list->bits bits->vector vector->bits bits
          bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator)

  ;; core is implemented in c
  (c-include "bitwise.c")
  (c-export (bitwise-not "bitwise_not" 1 1)
            (bitwise-and "bitwise_and" -1 -1)
            (bitwise-ior "bitwise_ior" -1 -1)
            (bitwise-xor "bitwise_xor" -1 -1)
            (bitwise-eqv "bitwise_eqv" -1 -1)
            (arithmetic-shift "arithmetic_shift" 2 2)
            (bit-count "bit_count" 1 1)
            (integer-length "integer_length" 1 1)))
