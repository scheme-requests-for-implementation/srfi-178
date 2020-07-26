(define (%bitvector-map->list ref-proc f bvecs)
  (if (null? (cdr bvecs))
      (let ((bvec (car bvecs)))        ; fast path
        (list-tabulate (bitvector-length bvec)
                       (lambda (i) (f (ref-proc bvec i)))))
      (list-tabulate
       (bitvector-length (car bvecs))
       (lambda (i)
         (apply f (map (lambda (bv) (ref-proc bv i)) bvecs))))))

(define (bitvector-map->list/int f . bvecs)
  (%bitvector-map->list bitvector-ref/int f bvecs))

(define (bitvector-map->list/bool f . bvecs)
  (%bitvector-map->list bitvector-ref/bool f bvecs))

