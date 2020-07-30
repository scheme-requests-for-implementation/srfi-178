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
      (if (= i len)
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
     (bytevector->bitvector* bytevec
                             0
                             (* 8 (bytevector-length bytevec))))
    ((bytevec start)
     (bytevector->bitvector* bytevec
                             start
                             (* 8 (bytevector-length bytevec))))
    ((bytevec start end)
     (bytevector->bitvector* bytevec start end))))

;; Write the bits of byte into the bitvector to.  Big-endian.
(define %bitvector-copy-byte!
  (case-lambda
    ((to at byte) (%bitvector-copy-byte! to at byte 0 8))
    ((to at byte start) (%bitvector-copy-byte! to at byte start 8))
    ((to at byte start end)
     (let lp ((i at) (j start))
       (when (< j end)
         (bitvector-set! to i (bit-set? (- 7 j) byte))
         (lp (+ i 1) (+ j 1)))))))

(define (%unpack-bytevector! to from start start-bit-bound end end-bit-bound)
  ;; Copy leading byte/byte fragment.
  (%bitvector-copy-byte! to 0 (bytevector-u8-ref from start) start-bit-bound)
  ;; Copy all whole bytes.
  (%bitvector-copy-bytevector! to (- 8 start-bit-bound) from (+ start 1) end)
  ;; Copy trailing byte/byte fragment.
  (%bitvector-copy-byte! to
                         (* 8 end)
                         (bytevector-u8-ref from end)
                         0
                         end-bit-bound))

;; FIXME: Edge cases.
(define (bytevector->bitvector* bytevec start end)
  (let-values (((start-byte start-off) (floor/ start 8))
               ((end-byte end-seg) (floor/ end 8)))
    (let ((result (make-bitvector (- end start)))
          (end-bound (+ end-seg 1)))
      (if (= start-byte end-byte)
          (%bitvector-copy-byte! result  ; range is intra-byte
                                 0
                                 (bytevector-u8-ref bytevec start-byte)
                                 start-off
                                 end-bound)
          (%unpack-bytevector! result
                               bytevec
                               start-byte
                               start-off
                               end-byte
                               end-bound))
      result)))

;; Write the bits of the selected range of the bytevector from into
;; the bitvector to.  start and end are bytevector indices.
(define (%bitvector-copy-bytevector! to at from start end)
  (let lp ((i at) (j start))
    (unless (>= j end)
      (%bitvector-copy-byte! to i (bytevector-u8-ref from j))
      (lp (+ i 8) (+ j 1)))))

(define bitvector->bytevector
  (case-lambda
    ((bvec)
     (bitvector->bytevector* bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (bitvector->bytevector* bvec start (bitvector-length bvec)))
    ((bvec start end)
     (bitvector->bytevector* bvec start end))))

(define (bitvector->bytevector!* bvec start end)
  #f)

(define bitvector->bytevector!
  (case-lambda
    ((bvec)
     (bitvector->bytevector!* bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (bitvector->bytevector!* bvec start (bitvector-length bvec)))
    ((bvec start end)
     (bitvector->bytevector!* bvec start end))))
