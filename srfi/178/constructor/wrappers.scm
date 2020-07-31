;;;; Constructors which are wrappers for SRFI 160 procedures.

(define make-bitvector
  (case-lambda
    ((size) (W (make-u8vector size)))
    ((size fill) (W (make-u8vector size (I fill))))))

(define (bitvector . args)
  (W (u8vector->list (map bit->integer args))))

(define (bitvector-unfold/int f length seed)
  (W (u8vector-unfold f length seed)))

(define (bitvector-unfold-right/int f length seed)
  (W (u8vector-unfold-right f length seed)))

(define bitvector-copy
  (case-lambda
    ((bvec) (W (u8vector-copy (U bvec))))
    ((bvec start) (W (u8vector-copy (U bvec) start)))
    ((bvec start end) (W (u8vector-copy (U bvec) start end)))))

(define bitvector-reverse-copy
  (case-lambda
    ((bvec) (W (u8vector-reverse-copy (U bvec))))
    ((bvec start) (W (u8vector-reverse-copy (U bvec) start)))
    ((bvec start end) (W (u8vector-reverse-copy (U bvec) start end)))))

(define (bitvector-append . bvecs)
  (bitvector-concatenate bvecs))

(define (bitvector-concatenate bvecs)
  (W (u8vector-concatenate (map U bvecs))))

(define (bitvector-append-subbitvectors . args)
  (W (u8vector-append-subvectors
       (map (lambda (x) (if (bitvector? x) (U x) x)) args))))
