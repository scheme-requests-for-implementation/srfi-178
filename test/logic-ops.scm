(define (%integer->bitvector-byte int)
  (bitvector-pad-right 0 (integer->bitvector int) 8))

(define (check-bitwise-operations)
  (define test-int1 #xb5)
  (define test-int2 #xca)
  (define test-int3 #xfe)
  (define test-bvec1 (%integer->bitvector-byte test-int1))
  (define test-bvec2 (%integer->bitvector-byte test-int2))
  (define test-bvec3 (%integer->bitvector-byte test-int3))
  (print-header "Checking bitwise operations...")

  ;;; not

  (check (bitvector= (bitvector-not test-bvec1)
                     (bitvector 0 1 0 1 0 0 1 0))
   => #t)
  (check (bitvector= (bitvector-not (bitvector-not test-bvec1)) test-bvec1)
   => #t)

  ;;; Associative binary operations.

  (check (bitvector=
          (bitvector-and test-bvec1 test-bvec2)
          (%integer->bitvector-byte (bitwise-and test-int1 test-int2)))
   => #t)
  (check (bitvector=
          (bitvector-ior test-bvec1 test-bvec2)
          (%integer->bitvector-byte (bitwise-ior test-int1 test-int2)))
   => #t)
  (check (bitvector=
          (bitvector-xor test-bvec1 test-bvec2)
          (%integer->bitvector-byte (bitwise-xor test-int1 test-int2)))
   => #t)
  (check (bitvector=
          (bitvector-eqv test-bvec1 test-bvec2)
          (%integer->bitvector-byte (bitwise-eqv test-int1 test-int2)))
   => #t)
)
