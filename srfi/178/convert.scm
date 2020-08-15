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

(define (bitvector->integer bvec)
  (bitvector-fold-right/int (lambda (r b) (+ (* r 2) b)) 0 bvec))

(define integer->bitvector
  (case-lambda
    ((int) (integer->bitvector int (integer-length int)))
    ((int len)
     (bitvector-unfold
      (lambda (_ int)
        (values (bit-set? 0 int) (arithmetic-shift int -1)))
      len
      int))))

;;;; Bytvector to bitvector conversion

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

;; Write the bits of byte into the bitvector bvec starting at index at.
;; Big-endian.
(define (%bitvector-copy-byte! to at byte)
  (let lp ((i at) (j 0))
    (when (< j 8)
      (bitvector-set! to i (bit-set? (- 7 j) byte))
      (lp (+ i 1) (+ j 1)))))

;; Unpack the bytes of bytevec into a fresh bitvector.  start and end
;; are bytevector indices.
(define (%unpack-bytevector bytevec start end)
  (let ((result (make-bitvector (* 8 (- end start)))))
    (let lp ((i 0) (j start))
      (cond ((>= j end) result)
            (else
             (%bitvector-copy-byte! result i (bytevector-u8-ref bytevec j))
             (lp (+ i 8) (+ j 1)))))))

;; Unpack the bytes of bytevec into a fresh bitvector.  NB: start and
;; end are bit indices!
;;
;; FIXME: This implementation is inefficient when start and end do not
;; align to byte boundaries; in this case, a subbitvector is copied.
(define (bytevector->bitvector* bytevec start end)
  (let-values (((start-byte start-off) (floor/ start 8))
               ((last-whole end-seg) (floor/ end 8)))
    ;; If there are trailing bits (i.e. if end doesn't align to a
    ;; byte boundary), then copy the whole trailing byte.
    (let* ((end-byte (if (zero? end-seg) last-whole (+ last-whole 1)))
           (bvec (%unpack-bytevector bytevec start-byte end-byte)))
      (if (and (zero? start-off) (zero? end-seg))
          bvec
          (bitvector-copy bvec start-off (+ start-off (- end start)))))))

;;;; Bitvector to bytevector conversions

(define bitvector->bytevector
  (case-lambda
    ((bvec)
     (bitvector->bytevector* bvec 0 (bitvector-length bvec)))
    ((bvec start)
     (bitvector->bytevector* bvec start (bitvector-length bvec)))
    ((bvec start end)
     (bitvector->bytevector* bvec start end))))

(define (bitvector->bytevector* bvec start end)
  (let ((result (make-bytevector (ceiling (/ (- end start) 8)))))
    (bitvector->bytevector!* result 0 bvec start end)
    result))

;; Convert len (< 8) bits in bvec to a big-endian integer.
(define (%bitvector->be-byte bvec start len)
  (let lp ((r 0) (i 0))
    (if (>= i len)
        r
        (lp (bitwise-ior
             r
             (arithmetic-shift (bitvector-ref/int bvec (+ i start))
                               (- 7 i)))
            (+ i 1)))))

(define (bitvector->bytevector!* bytevec at bvec start end)
  (let lp ((i 0) (j start))
    (when (< j end)
      (let ((len (min 8 (- end j))))
        (bytevector-u8-set! bytevec i (%bitvector->be-byte bvec j len))
        (lp (+ i 1) (+ j len))))))

(define bitvector->bytevector!
  (case-lambda
    ((bytevec at bvec)
     (bitvector->bytevector!* bytevec at bvec 0 (bitvector-length bvec)))
    ((bytevec at bvec start)
     (bitvector->bytevector!* bytevec at bvec start (bitvector-length bvec)))
    ((bytevec at bvec start end)
     (bitvector->bytevector!* bytevec at bvec start end))))
