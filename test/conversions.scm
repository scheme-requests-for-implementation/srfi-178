(define (check-bitvector-conversions)
  (print-header "Checking bitvector conversions...")

  (check (bitvector->list/int (bitvector))             => '())
  (check (bitvector->list/int (bitvector 1 0 1 0))     => '(1 0 1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 2)   => '(1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 1 3) => '(0 1)))
