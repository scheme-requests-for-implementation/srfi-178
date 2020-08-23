;;;; Bit conversions

(define (bit->integer bit) (I bit))

(define (bit->boolean bit) (B bit))

(define (bitvector->string bvec)
  (let loop ((i (- (bitvector-length bvec) 1))
             (r '()))
    (if (< i 0)
      (list->string (cons #\# (cons #\* r)))
      (loop (- i 1)
            (cons (if (bitvector-ref/bool bvec i) #\1 #\0) r)))))

(define (string->bitvector str)
  (call/cc
   (lambda (return)
     (and
       (> (string-length str) 1)
       (char=? (string-ref str 0) #\#)
       (char=? (string-ref str 1) #\*)
       (bitvector-unfold
        (lambda (ri si)
          (case (string-ref str si)
            ((#\0) (values 0 (+ si 1)))
            ((#\1) (values 1 (+ si 1)))
            (else (return #f))))
        (- (string-length str) 2)
        2)))))

;;;; Bitvector/integer conversions

(define (%bitvector->integer bvec len)
  (let lp ((r 0) (i 0))
    (if (>= i len)
        r
        (lp (bitwise-ior
             r
             (arithmetic-shift (bitvector-ref/int bvec i) i))
            (+ i 1)))))

(define bitvector->integer
  (case-lambda
    ((bvec) (%bitvector->integer bvec (bitvector-length bvec)))
    ((bvec len) (%bitvector->integer bvec len))))

(define (integer->bitvector int)
  (bitvector-unfold
   (lambda (_ int)
     (values (bit-set? 0 int) (arithmetic-shift int -1)))
   (integer-length int)
   int))
