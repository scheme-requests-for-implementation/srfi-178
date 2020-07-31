(define-library (srfi 178 constructor)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 160 u8)
	  (srfi 178 bitvector))

  (export make-bitvector bitvector bitvector-unfold/int bitvector-unfold/bool
          bitvector-unfold-right/int bitvector-unfold-right/bool bitvector-copy
          bitvector-reverse-copy bitvector-append bitvector-concatenate
          bitvector-append-subbitvectors)

  (include "constructor/wrappers.scm")
  (include "constructor/extra.scm")
)
