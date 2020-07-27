;;;; SRFI 178 procedures that are just wrappers

(define make-bitvector
  (case-lambda
    ((size) (W (make-u8vector size)))
    ((size fill) (W (make-u8vector size (I fill))))))

(define (bitvector-unfold/int f length seed)
  (W (u8vector-unfold f length seed)))

(define (bitvector-unfold/bool f length seed)
  (W (u8vector-unfold (lambda (i x) (I (f i x))) length seed)))

(define (bitvector-unfold-right/int f length seed)
  (W (u8vector-unfold-right f length seed)))

(define (bitvector-unfold-right/bool f length seed)
  (W (u8vector-unfold-right (lambda (i x) (I (f i x))) length seed)))

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

(define (bitvector-empty? bvec)
  (eqv? 0 (u8vector-length (U bvec))))

(define (bitvector= . bvecs)
  (apply u8vector= (map U bvecs)))

(define (bitvector-ref/int bvec i)
  (u8vector-ref (U bvec) i))

(define (bitvector-ref/bool bvec i)
  (B (u8vector-ref (U bvec) i)))

(define (bitvector-length bvec)
  (u8vector-length (U bvec)))

(define (bitvector-take bvec n)
  (W (u8vector-take (U bvec) n)))

(define (bitvector-take-right bvec n)
  (W (u8vector-take-right (U bvec) n)))

(define (bitvector-drop bvec n)
  (W (u8vector-drop (U bvec) n)))

(define (bitvector-drop-right bvec n)
  (W (u8vector-drop-right (U bvec) n)))

(define (bitvector-segment bvec n)
  (W (u8vector-segment (U bvec) n)))

(define bitvector-fold/int
  (case-lambda
    ((kons knil bvec) (u8vector-fold kons knil (U bvec)))
    ((kons knil . bvecs) (u8vector-fold kons knil (map U bvecs)))))

(define bitvector-fold/bool
  (case-lambda
    ((kons knil bvec)
     (u8vector-fold (lambda (x b) (kons x (B b)))
                    knil
                    (U bvec)))
    ((kons knil . bvecs)
     (u8vector-fold (lambda (x . bits)
                      (apply kons x (map bit->boolean bits)))
                    knil
                    (map U bvecs)))))

(define bitvector-fold-right/int
  (case-lambda
    ((kons knil bvec) (u8vector-fold-right kons knil (U bvec)))
    ((kons knil . bvecs) (u8vector-fold-right kons knil (map U bvecs)))))

(define bitvector-fold-right/bool
  (case-lambda
    ((kons knil bvec)
     (u8vector-fold-right (lambda (x bit) (kons x (B bit)))
                          knil
                          (U bvec)))
    ((kons knil . bvecs)
     (u8vector-fold-right (lambda (x . bits)
                            (apply kons x (map bit->integer bits)))
                          knil
                          (map U bvecs)))))

(define bitvector-map/int
  (case-lambda
    ((f bvec)
     (W (u8vector-map f (U bvec))))     ; fast path
    ((f . bvecs)
     (W (apply u8vector-map f (map U bvecs))))))

;; FIXME: The variadic case--yowch.
(define bitvector-map/bool
  (case-lambda
    ((f bvec)
     (W (u8vector-map (lambda (n) (I (f (B n)))) (U bvec))))  ; fast path
    ((f . bvecs)
     (W (apply u8vector-map
	       (lambda ns (I (apply f (map bit->boolean ns))))
	       (map U bvecs))))))

(define bitvector-map!/int
  (case-lambda
    ((f bvec)
     (u8vector-map! f (U bvec)))    ; fast path
    ((f . bvecs)
     (apply u8vector-map! f (map U bvecs)))))

;; FIXME: The variadic case--yowch.
(define bitvector-map!/bool
  (case-lambda
    ((f bvec)
     (u8vector-map! (lambda (n) (I (f (B n)))) (U bvec)))    ; fast path
    ((f . bvecs)
     (apply u8vector-map!
	    (lambda ns (I (apply f (map bit->boolean ns))))
	    (map U bvecs)))))

(define (bitvector-for-each/int f . bvecs)
    (u8vector-for-each f (map U bvecs)))

(define (bitvector-for-each/bool f . bvecs)
    (u8vector-for-each (lambda (x) (B (f (I x)))) (map U bvecs)))

(define (bitvector-set! bvec i bit)
  (u8vector-set! (U bvec) i (I bit)))

(define (bitvector-swap! bvec i j)
  (u8vector-swap! (U bvec) i j))

(define bitvector-fill!
  (case-lambda
    ((bvec fill)
     (u8vector-fill! (U bvec) fill))
    ((bvec fill start)
     (u8vector-fill! start (U bvec) fill start))
    ((bvec fill start end)
     (u8vector-fill! start end (U bvec) fill start end))))

(define bitvector-reverse!
  (case-lambda
    ((bvec)
     (u8vector-reverse! (U bvec)))
    ((bvec start)
     (u8vector-reverse! (U bvec) start))
    ((bvec start end)
     (u8vector-reverse! (U bvec) start end))))

(define bitvector-copy!
  (case-lambda
    ((to at from)
     (u8vector-copy! (U to) at (U from)))
    ((to at from start)
     (u8vector-copy! (U to) at (U from) start))
    ((to at from start end)
     (u8vector-copy! (U to) at (U from) start end))))

(define bitvector-reverse-copy!
  (case-lambda
    ((to at from)
     (u8vector-reverse-copy! (U to) at (U from)))
    ((to at from start)
     (u8vector-reverse-copy! (U to) at (U from) start))
    ((to at from start end)
     (u8vector-reverse-copy! (U to) at (U from) start end))))

(define bitvector->list/int
  (case-lambda
    ((bvec)
     (u8vector->list (U bvec)))
    ((bvec start)
     (u8vector->list (U bvec) start))
    ((bvec start end)
     (u8vector->list (U bvec) start end))))

(define bitvector->list/bool
  (case-lambda
    ((bvec)
     (map B* (u8vector->list (U bvec))))
    ((bvec start)
     (map B* (u8vector->list (U bvec) start)))
    ((bvec start end)
     (map B* (u8vector->list (U bvec) start end)))))

(define reverse-bitvector->list/int
  (case-lambda
    ((bvec)
     (reverse-u8vector->list (U bvec)))
    ((bvec start)
     (reverse-u8vector->list (U bvec) start))
    ((bvec start end)
     (reverse-u8vector->list (U bvec) start end))))

(define reverse-bitvector->list/bool
  (case-lambda
    ((bvec)
     (map B* (reverse-u8vector->list (U bvec))))
    ((bvec start)
     (map B* (reverse-u8vector->list (U bvec) start)))
    ((bvec start end)
     (map B* (reverse-u8vector->list (U bvec) start end)))))

(define bitvector->vector/int
  (case-lambda
    ((bvec)
     (u8vector->vector (U bvec)))
    ((bvec start)
     (u8vector->vector (U bvec) start))
    ((bvec start end)
     (u8vector->vector (U bvec) start end))))

(define bitvector->vector/bool
  (case-lambda
    ((bvec)
     (map B* (u8vector->vector (U bvec))))
    ((bvec start)
     (map B* (u8vector->vector (U bvec) start)))
    ((bvec start end)
     (map B* (u8vector->vector (U bvec) start end)))))

(define (list->bitvector list)
  (W (list->u8vector (map I* list))))

(define (reverse-list->bitvector list)
  (W (reverse-list->u8vector (map I* list))))

(define (bitvector . args) (list->bitvector args))

(define (vector->bitvector vector)
  (W (vector->u8vector (vector-map I* vector))))

