(define (check-mutators)
  (print-header "Checking mutators...")

  (check
   (let ((bvec (bitvector 1 0 1 0)))
     (bitvector= (begin (bitvector-set! bvec 1 1) bvec)
                 (bitvector 1 1 1 0)))
   => #t)
  (check
   (let ((bvec (bitvector 1 0 1 0)))
     (bitvector= (begin (bitvector-set! bvec 0 #f) bvec)
                 (bitvector 0 0 1 0)))
   => #t)

  (check
   (let ((bvec (bitvector 1 0 1 0)))
     (bitvector= (begin (bitvector-swap! bvec 0 1) bvec)
                 (bitvector 0 1 1 0)))
   => #t)

  ;;; reverse!

  (check
   (let ((bvec (bitvector 1 0 1 0)))
     (bitvector= (begin (bitvector-reverse! bvec) bvec)
                 (bitvector 0 1 0 1)))
   => #t)
  (check
   (let ((bvec (bitvector 1 0 1 0)))
     (bitvector= (begin (bitvector-reverse! bvec 2) bvec)
                 (bitvector 1 0 0 1)))
   => #t)
  (check
   (let ((bvec (bitvector 1 0 1 0)))
     (bitvector= (begin (bitvector-reverse! bvec 1 3) bvec)
                 (bitvector 1 1 0 0)))
   => #t)

  ;;; copy!

  (check
   (let ((bvec (bitvector 0 0 0 0)))
     (bitvector= (begin (bitvector-copy! bvec 0 (bitvector 1 0)) bvec)
                 (bitvector 1 0 0 0)))
   => #t)
  (check
   (let ((bvec (bitvector 0 0 0 0)))
     (bitvector= (begin (bitvector-copy! bvec 1 (bitvector 1 1 0) 1) bvec)
                 (bitvector 0 1 0 0)))
   => #t)
  (check
   (let ((bvec (bitvector 0 0 0 0)))
     (bitvector= (begin (bitvector-copy! bvec 1 (bitvector 1 0 1) 0 2) bvec)
                 (bitvector 0 1 0 0)))
   => #t)

  ;;; reverse-copy!

  (check
   (let ((bvec (bitvector 0 0 0 0)))
     (bitvector= (begin (bitvector-reverse-copy! bvec 0 (bitvector 1 0))
                        bvec)
                 (bitvector 0 1 0 0)))
   => #t)
  (check
   (let ((bvec (bitvector 0 0 0 0)))
     (bitvector= (begin (bitvector-reverse-copy! bvec 1 (bitvector 0 0 1) 1)
                        bvec)
                 (bitvector 0 1 0 0)))
   => #t)
  (check
   (let ((bvec (bitvector 0 0 0 0)))
     (bitvector= (begin (bitvector-reverse-copy! bvec 1 (bitvector 0 1 1) 0 2)
                        bvec)
                 (bitvector 0 1 0 0)))
   => #t)
)
