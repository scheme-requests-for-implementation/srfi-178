(define (check-constructors)
  (print-header "Checking constructors...")

  (check (bitvector-length (make-bitvector 8))       => 8)
  (check (bitvector->list/int (make-bitvector 4 0))  => '(0 0 0 0))
  (check (bitvector->list/int (make-bitvector 4 #t)) => '(1 1 1 1))

  ;;; unfolds

  (check (bitvector->list/int
          (bitvector-unfold (lambda (i n) (values 0 0)) 4 0))
   => '(0 0 0 0))
  (check (bitvector->list/int
          (bitvector-unfold (lambda (i n) (values n (if (zero? n) 1 0)))
                                4
                                1))
   => '(1 0 1 0))
  (check (bitvector->list/int
          (bitvector-unfold-right (lambda (i n) (values 0 0)) 4 0))
   => '(0 0 0 0))
  (check (bitvector->list/int
          (bitvector-unfold-right
           (lambda (i n) (values n (if (zero? n) 1 0)))
           4
           1))
   => '(0 1 0 1))

  ;;; copy

  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector= bvec (bitvector-copy bvec)) => #t)
    (check (eqv? bvec (bitvector-copy bvec)) => #f))  ; fresh copy
  (check (bitvector->list/int (bitvector-copy (bitvector 1 0 1 0) 1))
   => '(0 1 0))
  (check (bitvector->list/int (bitvector-copy (bitvector 1 0 1 0) 2 4))
   => '(1 0))

  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector->list/int (bitvector-reverse-copy bvec))
     => (reverse (bitvector->list/int bvec)))
    (check (eqv? bvec (bitvector-reverse-copy bvec)) => #f))  ; fresh copy
  (check (bitvector->list/int (bitvector-reverse-copy (bitvector 1 0 1 0) 1))
   => '(0 1 0))
  (check (bitvector->list/int (bitvector-reverse-copy (bitvector 1 0 1 0) 2 4))
   => '(0 1))

  ;;; append & concatenate

  (check (bitvector->list/int
          (bitvector-append (bitvector 1 0) (bitvector 0 1)))
   => '(1 0 0 1))
  (check (bitvector->list/int
          (bitvector-append (bitvector 1 0) (bitvector 0 1) (bitvector)))
   => '(1 0 0 1))
  (check (bitvector->list/int
          (bitvector-concatenate
           (list (bitvector 1 0) (bitvector 0 1) (bitvector))))
   => '(1 0 0 1))
  (check (bitvector->list/int
          (bitvector-append-subbitvectors (bitvector 1 0 0 1) 0 2
                                          (bitvector 1 1 1 1) 2 4))
   => '(1 0 1 1)))
