;;;; Bit conversions

(define (bit->integer bit) (I bit))

(define (bit->boolean bit) (B bit))

(define (bitvector->string bvec)
  (let loop ((i (- (bitvector-length bvec) 1))
             (r '()))
    (if (< i 0)
      (list->string (cons #\# (cons #\* r)))
      (loop (cons (if (bitvector-ref/bool bvec i) #\1 #\0) r) (- i 1)))))

(define (string->bitvector str)
  (call/cc
   (lambda (return)
     (and
       (char=? (string-ref str 0) #\#)
       (char=? (string-ref str 1) #\*)
       (bitvector-unfold/int
        (lambda (ri si)
          (case (string-ref str si)
            ((#\0) (values 0 (+ si 1)))
            ((#\1) (values 1 (+ si 1)))
            (else (return #f))))
        (- (string-length str) 2)
        2)))))

(define (bitvector->integer bvec)
  (let ((len (bitvector-length bvec)))
    (let lp ((r 0) (i 0))
      (if (= l len)
          r
          (lp (bitwise-ior
               r
               (arithmetic-shift (bitvector-ref/int bvec i) i))
              (+ i 1))))))

(define (integer->bitvector int)
  (bitvector-unfold/bool
   (lambda (_ int)
     (values (bit-set? 0 int) (arithmetic-shift int -1)))
   (integer-length int)
   int))

(define (bitvector->bytevector* bytevec start end)
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
  (let* ((length (- end start))
         (result (make-bitvector length)))
    (let loop ((i 0) (start start) (length length))
      (unless (= length 0))
        (bitvector-set! result i (bytevector-u8-ref bytevec start))
        (loop (+ i 1) (+ start 1) (- length 1)))
    result))

(define bitvector->bytevector
  (case-lambda
    ((bvec)
     (bitvector->bytevector* bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (bitvector->bytevector* bvec start (bitvector-length bvec)))
    ((bvec start end)
     (bitvector->bytevector* bvec start end))))

(define bitvector->bytevector!
  (case-lambda
    ((bvec)
     (bitvector->bytevector!* bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (bitvector->bytevector!* bvec start (bitvector-length bvec)))
    ((bvec start end)
     (bitvector->bytevector!* bvec start end))))
