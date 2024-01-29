;;; SPDX-FileCopyrightText: 2020 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

(define (check-iterators)
  (print-header "Checking iteration...")

  ;;; take & take-right

  (check (bitvector= (bitvector-take (bitvector 1 0 1 0) 2)
                     (bitvector 1 0))
   => #t)
  (check (bitvector-empty? (bitvector-take (bitvector 1 0) 0)) => #t)
  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector= (bitvector-take bvec (bitvector-length bvec))
                       bvec)
     => #t)
    (check (bitvector= (bitvector-take-right bvec (bitvector-length bvec))
                       bvec)
     => #t))
  (check (bitvector= (bitvector-take-right (bitvector 1 0 1 0) 3)
                     (bitvector 0 1 0))
   => #t)
  (check (bitvector-empty? (bitvector-take-right (bitvector 1 0) 0)) => #t)

  ;;; drop & drop-right

  (check (bitvector= (bitvector-drop (bitvector 1 0 1 0) 1)
                     (bitvector 0 1 0))
   => #t)
  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector-empty? (bitvector-drop bvec (bitvector-length bvec)))
     => #t)
    (check (bitvector= (bitvector-drop bvec 0) bvec) => #t)
    (check (bitvector= (bitvector-drop-right bvec 0) bvec) => #t)
    (check (bitvector-empty?
            (bitvector-drop-right bvec (bitvector-length bvec)))
     => #t))
  (check (bitvector= (bitvector-drop-right (bitvector 1 0 1 0) 1)
                     (bitvector 1 0 1))
   => #t)

  ;;; segment

  (check (bitvector= (car (bitvector-segment (bitvector 1 0 1 0) 2))
                     (bitvector 1 0))
   => #t)
  (let ((bvec (bitvector 1 0 1 0)))
    (check (bitvector= (bitvector-concatenate (bitvector-segment bvec 1))
                       bvec)
     => #t))

  ;;; fold

  (check (bitvector-fold/int + 0 (bitvector)) => 0)
  (check (bitvector-fold/int + 0 (bitvector 1)) => 1)
  (check (bitvector-fold/bool proc-or #f (bitvector)) => #f)
  (check (bitvector-fold/bool proc-or #f (bitvector #t)) => #t)
  (check (bitvector-fold-right/int + 0 (bitvector)) => 0)
  (check (bitvector-fold-right/int + 0 (bitvector 1)) => 1)
  (check (bitvector-fold-right/bool proc-or #f (bitvector)) => #f)
  (check (bitvector-fold-right/bool proc-or #f (bitvector #t)) => #t)

  ;;; map

  (check (bitvector-empty? (bitvector-map/int values (bitvector))) => #t)
  (check (bitvector= (bitvector-map/int (constantly 1) (bitvector 0 0 1))
                     (bitvector 1 1 1))
   => #t)
  (check (bitvector= (bitvector-map/int (lambda (a b c) b)
                                        (bitvector 1 0 0)
                                        (bitvector 0 1 0)
                                        (bitvector 0 0 1))
                     (bitvector 0 1 0))
   => #t)
  (check (bitvector-empty? (bitvector-map/bool values (bitvector))) => #t)
  (check (bitvector= (bitvector-map/bool (constantly #t)
                                         (bitvector #f #f #t))
                     (bitvector #t #t #t))
   => #t)
  (check (bitvector= (bitvector-map/bool (lambda (a b c) b)
                                         (bitvector #t #f #f)
                                         (bitvector #f #t #f)
                                         (bitvector #f #f #t))
                     (bitvector #f #t #f))
   => #t)

  ;;; map!

  (check (let ((bvec (bitvector)))
           (bitvector-map!/int values bvec)
           (bitvector-empty? bvec))
   => #t)
  (check (let ((bvec (bitvector 1 0 1 0)))
           (bitvector-map!/int (constantly 1) bvec)
           (bitvector= bvec (bitvector 1 1 1 1)))
   => #t)
  (check (let ((bvec1 (bitvector 1 0 0))
               (bvec2 (bitvector 0 1 0))
               (bvec3 (bitvector 0 0 1)))
           (bitvector-map!/int (lambda (a b c) b) bvec1 bvec2 bvec3)
           (bitvector= bvec1 bvec2))
   => #t)
  (check (let ((bvec (bitvector)))
           (bitvector-map!/bool values bvec)
           (bitvector-empty? bvec))
   => #t)
  (check (let ((bvec (bitvector #t #f #t #f)))
           (bitvector-map!/bool (constantly #t) bvec)
           (bitvector= bvec (bitvector #t #t #t #t)))
   => #t)
  (check (let ((bvec1 (bitvector #t #f #f))
               (bvec2 (bitvector #f #t #f))
               (bvec3 (bitvector #f #f #t)))
           (bitvector-map!/bool (lambda (a b c) b) bvec1 bvec2 bvec3)
           (bitvector= bvec1 bvec2))
   => #t)


  ;;; map->list

  (check (bitvector-map->list/bool values (bitvector)) => '())
  (check (bitvector-map->list/int (constantly 1) (bitvector 1 0 0)) => '(1 1 1))
  (check (bitvector-map->list/int list (bitvector 1 0) (bitvector 0 1))
   => '((1 0) (0 1)))
  (check (bitvector-map->list/bool values (bitvector)) => '())
  (check (bitvector-map->list/bool (constantly #t) (bitvector 1 0 0))
   => '(#t #t #t))
  (check (bitvector-map->list/bool list (bitvector 1 0) (bitvector 0 1))
   => '((#t #f) (#f #t)))

  ;;; for-each

  (let ((bvec (bitvector 1 0 1 0)))
    (check (let ((c 0))
             (bitvector-for-each/int (lambda (_) (set! c (+ c 1))) bvec)
             c)
     => (bitvector-length bvec))
    (check (let ((lis '()))
             (bitvector-for-each/int (lambda (b) (set! lis (cons b lis))) bvec)
             lis)
     => (reverse-bitvector->list/int bvec))
    (check (let ((c 0))
             (bitvector-for-each/bool (lambda (_) (set! c (+ c 1))) bvec)
             c)
     => (bitvector-length bvec))
    (check (let ((lis '()))
             (bitvector-for-each/bool (lambda (b) (set! lis (cons b lis))) bvec)
             lis)
     => (reverse-bitvector->list/bool bvec)))
)
