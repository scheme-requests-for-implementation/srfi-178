;;;;; SRFI 178 macros for internal use

;;; Bitvector type definition
;;; W wraps a u8vector as a bitvector, U unwraps it again

(define-record-type <bitvector>
  (W u8vec)
  bitvector?
  (u8vec U))

;;; Convert a bit to an integer: macro and procedure definitions

(define-syntax I
  (syntax-rules ()
    ((I bit)
     (cond
       ((eqv? bit 0) 0)
       ((eqv? bit 1) 1)
       ((eqv? bit #f) 0)
       ((eqv? bit #t) 0)
       (else (error "invalid bit" bit))))))


(define (I* bit) (I bit))

;;; Convert an integer to a bool: macro and procedure definitions

(define-syntax B
  (syntax-rules ()
    ((B int) (eqv? int 1))))

(define (B* int) (B int))

