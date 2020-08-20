(define (check-constructors)
  (print-header "Checking constructors...")

  (check (bitvector-length (make-bitvector 8))       => 8)
  (check (bitvector->list/int (make-bitvector 4 0))  => '(0 0 0 0))
  (check (bitvector->list/int (make-bitvector 4 #t)) => '(1 1 1 1))

  ;;; unfolds

  (check (bitvector=
          (bitvector-unfold (lambda (_) 0) 4)
          (bitvector 0 0 0 0))
   => #t)
  (check (bitvector=
          (bitvector-unfold (lambda (_ b) (values b (not b))) 4 #f)
          (bitvector 0 1 0 1))
   => #t)
  (check (bitvector=
          (bitvector-unfold (lambda (_ b c)
                              (values (and b c) (not b) c))
                            4
                            #t
                            #t)
          (bitvector 1 0 1 0))
   => #t)
  (check (bitvector=
          (bitvector-unfold-right (lambda (_) 0) 4)
          (bitvector 0 0 0 0))
   => #t)
  (check (bitvector=
          (bitvector-unfold-right (lambda (_ b) (values b (not b))) 4 #f)
          (bitvector 1 0 1 0))
   => #t)
  (check (bitvector=
          (bitvector-unfold-right (lambda (_ b c)
                                    (values (and b c) (not b) c))
                                  4
                                  #t
                                  #t)
          (bitvector 0 1 0 1))
   => #t)

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
