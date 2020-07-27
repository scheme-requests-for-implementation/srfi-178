(define (bitvector-logical-shift bvec count bit)
  #f)

(define (bitvector-logical-shift! bvec count bit)
  #f)

(define (bitvector-count bit bvec)
  (bitvector-fold/int (lambda (n b) (if (= b bit) (+ n 1) n))
		      0
		      bvec))

(define (bitvector-if if-bvec then-bvec else-bvec)
  (bitvector-map/bool (lambda (bit then-bit else-bit)
			(if bit then-bit else-bit))
		      if-bvec
		      then-bvec
		      else-bvec))

(define (bitvector-first-bit bit bvec)
  (let ((len (bitvector-length bvec)))
    (let lp ((i 0))
      (cond ((>= i len) -1)
	    ((= bit (bitvector-ref/int bvec i)) i)
	    (else (lp (+ i 1)))))))

