(define (unspecified) (if #f #f))

;; Return a bitvector of length len.  The element at index i is
;; given by (proc i).
(define (%bitvector-tabulate len proc)
  (let ((result (make-u8vector len)))
    (let lp ((i 0))
      (if (>= i len)
          (W result)
          (begin
           (u8vector-set! result i (I (proc i)))
           (lp (+ i 1)))))))
