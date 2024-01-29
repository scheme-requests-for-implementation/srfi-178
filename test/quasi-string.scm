;;; SPDX-FileCopyrightText: 2020 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

(define (check-quasi-string-ops)
  (print-header "Checking quasi-string operations...")

  ;;; prefix & suffix

  (check (bitvector-prefix-length (bitvector 1 0 0) (bitvector 1 0 1)) => 2)
  (check (bitvector-prefix-length (bitvector) (bitvector 1 0 1))       => 0)
  (let ((bvec (bitvector 1 0 1)))
    (check (= (bitvector-prefix-length bvec bvec) (bitvector-length bvec))
     => #t)
    (check (= (bitvector-suffix-length bvec bvec) (bitvector-length bvec))
     => #t))
  (check (bitvector-suffix-length (bitvector 1 0 0) (bitvector 0 0 0)) => 2)
  (check (bitvector-suffix-length (bitvector) (bitvector 1 0 1))       => 0)

  (check (bitvector-prefix? (bitvector 1) (bitvector 1 0)) => #t)
  (check (bitvector-prefix? (bitvector 0) (bitvector 1 0)) => #f)
  (check (bitvector-suffix? (bitvector 0) (bitvector 1 0)) => #t)
  (check (bitvector-suffix? (bitvector 1) (bitvector 1 0)) => #f)
  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector-prefix? bvec bvec) => #t)
    (check (bitvector-suffix? bvec bvec) => #t))

  ;;; pad & trim

  (check (bitvector=
          (bitvector-pad 0 (bitvector 1) 4)
          (bitvector 0 0 0 1))
   => #t)
  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector= (bitvector-pad 0 bvec (bitvector-length bvec))
                       bvec)
     => #t)
    (check (bitvector= (bitvector-pad-right 0 bvec (bitvector-length bvec))
                       bvec)
     => #t))
  (check (bitvector=
          (bitvector-pad-right 0 (bitvector 1) 4)
          (bitvector 1 0 0 0))
   => #t)
  (check (bitvector= (bitvector-trim 0 (bitvector 0 0 0 1))
                     (bitvector 1))
   => #t)
  (check (bitvector= (bitvector-trim 0 (bitvector 1 0 1))
                     (bitvector 1 0 1))
   => #t)
  (check (bitvector= (bitvector-trim-right 0 (bitvector 1 0 1))
                     (bitvector 1 0 1))
   => #t)
  (check (bitvector= (bitvector-trim-right 0 (bitvector 1 0 0 0))
                     (bitvector 1))
   => #t)
  (check (bitvector= (bitvector-trim-both 1 (bitvector 1 0 1))
                     (bitvector 0))
   => #t)
  (check (bitvector= (bitvector-trim-both 0 (bitvector 1 0 1))
                     (bitvector 1 0 1))
   => #t)
)
