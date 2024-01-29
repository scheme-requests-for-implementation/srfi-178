;;; SPDX-FileCopyrightText: 2020 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

(define (check-bitvector-conversions)
  (print-header "Checking bitvector conversions...")

  ;;; lists

  (check (bitvector->list/int (bitvector))             => '())
  (check (bitvector->list/int (bitvector 1 0 1 0))     => '(1 0 1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 2)   => '(1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 1 3) => '(0 1))
  (check (bitvector->list/bool (bitvector)) => '())
  (check (bitvector->list/bool (bitvector 1 0 1 0)) => '(#t #f #t #f))
  (check (bitvector->list/bool (bitvector 1 0 1 0) 2) => '(#t #f))
  (check (bitvector->list/bool (bitvector 1 0 1 0) 1 3) => '(#f #t))

  (check (reverse-bitvector->list/int (bitvector)) => '())
  (check (reverse-bitvector->list/int (bitvector 1 0 1 0) 2) => '(0 1))
  (check (reverse-bitvector->list/int (bitvector 1 0 1 0) 1 3) => '(1 0))
  (let ((bvec (bitvector 1 0 1 0)))
    (check (equal? (reverse-bitvector->list/int bvec)
                   (reverse (bitvector->list/int bvec)))
     => #t)
    (check (equal? (reverse-bitvector->list/bool bvec)
                   (reverse (bitvector->list/bool bvec)))
     => #t))
  (check (reverse-bitvector->list/bool (bitvector)) => '())
  (check (reverse-bitvector->list/bool (bitvector 1 0 1 0) 2) => '(#f #t))
  (check (reverse-bitvector->list/bool (bitvector 1 0 1 0) 1 3) => '(#t #f))

  (check (bitvector= (list->bitvector '(1 0 #t #f)) (bitvector 1 0 1 0)) => #t)
  (let ((bs '(1 0 1 0)))
    (check (equal? bs (bitvector->list/int (list->bitvector bs))) => #t)
    (check (equal? bs
                   (reverse-bitvector->list/int
                    (reverse-list->bitvector bs)))
     => #t))
  (check (bitvector= (reverse-list->bitvector '(1 0 #t #f)) (bitvector 0 1 0 1))
   => #t)

  ;;; vectors

  (check (bitvector->vector/int (bitvector))              => #())
  (check (bitvector->vector/int (bitvector 1 0 1 0))      => #(1 0 1 0))
  (check (bitvector->vector/int (bitvector 1 0 1 0) 1)    => #(0 1 0))
  (check (bitvector->vector/int (bitvector 1 0 1 0) 1 3)  => #(0 1))
  (check (bitvector->vector/bool (bitvector))             => #())
  (check (bitvector->vector/bool (bitvector 1 0 1 0))     => #(#t #f #t #f))
  (check (bitvector->vector/bool (bitvector 1 0 1 0) 1)   => #(#f #t #f))
  (check (bitvector->vector/bool (bitvector 1 0 1 0) 1 3) => #(#f #t))

  (check (reverse-bitvector->vector/int (bitvector))              => #())
  (check (reverse-bitvector->vector/int (bitvector 1 0 1 0))      => #(0 1 0 1))
  (check (reverse-bitvector->vector/int (bitvector 1 0 1 0) 2)    => #(0 1))
  (check (reverse-bitvector->vector/int (bitvector 1 0 1 0) 1 3)  => #(1 0))
  (check (reverse-bitvector->vector/bool (bitvector))             => #())
  (check (reverse-bitvector->vector/bool (bitvector 1 0 1 0))
   => #(#f #t #f #t))
  (check (reverse-bitvector->vector/bool (bitvector 1 0 1 0) 2)   => #(#f #t))
  (check (reverse-bitvector->vector/bool (bitvector 1 0 1 0) 1 3) => #(#t #f))

  (check (bitvector-empty? (vector->bitvector #())) => #t)
  (check (bitvector= (vector->bitvector #(1 0 #t #f))
                     (bitvector 1 0 1 0))
   => #t)
  (check (bitvector= (vector->bitvector #(1 0 1 0) 1)
                     (bitvector 0 1 0))
   => #t)
  (check (bitvector= (vector->bitvector #(1 0 1 0) 1 3)
                     (bitvector 0 1))
   => #t)
  (check (bitvector-empty? (reverse-vector->bitvector #())) => #t)
  (check (bitvector= (reverse-vector->bitvector #(1 0 #t #f))
                     (bitvector 0 1 0 1))
   => #t)
  (check (bitvector= (reverse-vector->bitvector #(1 0 1 0) 2)
                     (bitvector 0 1))
   => #t)
  (check (bitvector= (reverse-vector->bitvector #(1 0 1 0) 1 3)
                     (bitvector 1 0))
   => #t)

  ;;; strings

  (check (bitvector->string (bitvector 1 0 1 0))     => "#*1010")
  (check (bitvector->string (bitvector))             => "#*")
  (check (bitvector= (string->bitvector "#*1010") (bitvector 1 0 1 0))
   => #t)
  (check (bitvector-empty? (string->bitvector "#*")) => #t)
  (check (string->bitvector "")                      => #f)
  (check (string->bitvector "1010")                  => #f)
  (check (string->bitvector "#")                     => #f)
  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector= (string->bitvector (bitvector->string bvec))
                       bvec)
     => #t))

  ;;; integers

  ;; Remember, these are little-endian!
  (check (bitvector->integer (bitvector 0 1 0 1)) => #xa)
  (check (bitvector->integer (bitvector 1 0 1 0 1 1 0 1)) => #xb5)
  (check (bitvector= (integer->bitvector #xa) (bitvector 0 1 0 1)) => #t)
  (check (bitvector= (integer->bitvector #xb5) (bitvector 1 0 1 0 1 1 0 1))
    => #t)
  (check (bitvector= (integer->bitvector #xb5 4) (bitvector 1 0 1 0)) => #t)
)
