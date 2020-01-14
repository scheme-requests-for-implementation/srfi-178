(define (bitvector->string bvec)
  #f)

(define (string->bitvector str)
  #f)

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

