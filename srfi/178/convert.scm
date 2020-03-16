(define (bitvector->string bvec)
  (let loop ((i (- (bitvector-length bvec) 1))
             (r '()))
    (if (< i 0)
      (list->string (cons #\# (cons #\* r)))
      (loop (cons (if (bitvector-ref/bool bvec i) #\1 #\0) r) (- i 1)))))

(define (string->bitvector str)
  (call/cc return
    (cond
      ((if (not (char=? (string-ref str 0) #\#)))
       (return #f))
      ((if (not (char=? (string-ref str 1) #\*))
    (let loop ((ri 0) (si 2))
      (cond
        ((eqv? (string-ref str si) #\0))
        (bitvector-set! #f)
       ((eqv? (string-ref str si) #\1))
        (bitvector-set! #t)
       (else 
         (return #f))))
    r))

(define (bitvector->integer bvec)
  #f)

(define (integer->bitvector int)
  #f)

(define bitvector->bytevector
  (case-lambda
    ((bvec) (bitvector->bytevector* bvec 0 (bitvector-length bvec))
    ((bvec start) (bitvector->bytevector* bvec start (bitvector-length bvec))
    ((bvec start end) (bitvector->bytevector* bvec start end))))

(define (bytevector->bitvector* bytevec start end)
  #f)

(define bytevector->bitvector
  (case-lambda
    ((bytevec)
     (bytevector->bitvector* bytevec 0 (bitvector-length bytevec)))
    ((bytevec start)
     (bytevector->bitvector* bytevec start (bitvector-length bytevec)))
    ((bytevec start end)
    (bytevector->bitvector* bytevec start end))))

(define (bytevector->bitvector* bytevec start end)
  #f)

(define bitvector->bytevector
  (case-lambda
    ((bvec)
     (bitvector->bytevector* bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (bitvector->bytevector* bvec start (bitvector-length bvec)))
    ((bvec start end)
     (bitvector->bytevector* bvec start end))))

(define bitvector->bytevector!!
  (case-lambda
    ((bvec)
     (bitvector->bytevector!* bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (bitvector->bytevector!* bvec start (bitvector-length bvec)))
    ((bvec start end)
     (bitvector->bytevector!* bvec start end))))

