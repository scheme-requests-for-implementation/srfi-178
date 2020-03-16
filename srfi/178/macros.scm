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
       ((not bit) 0)
       (else 1)))))


(define (I* bit) (I bit))

;;; Convert a bit to a bool: macro and procedure definitions

(define-syntax B
  (syntax-rules ()
    ((B int) (not (or (eqv? bit 0) (not bit))))))

(define (B* int) (B int))

