(define (bitvector-unfold f length seed)
  (let ((result (make-u8vector length)))
    (let lp ((i 0) (state seed))
      (if (>= i length)
	  (W result)
	  (let-values (((b state*) (f i state)))
            (u8vector-set! result i (I b))
	    (lp (+ i 1) state*))))))

(define (bitvector-unfold-right f length seed)
  (let ((result (make-u8vector length)))
    (let lp ((i (- length 1)) (state seed))
      (if (< i 0)
          (W result)
	  (let-values (((b state*) (f i state)))
	    (u8vector-set! result i (I b))
	    (lp (- i 1) state*))))))