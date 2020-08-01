;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:

;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base))
(import (scheme write))
(import (srfi 178))

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define *tests-failed* 0)
      (define-syntax check
        (syntax-rules (=>)
          ((check expr => expected)
           (if (equal? expr expected)
             (begin
               (display 'expr)
               (display " => ")
               (display expected)
               (display " ; correct")
               (newline))
             (begin
               (set! *tests-failed* (+ *tests-failed* 1))
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))))))
      (define (check-report)
        (if (zero? *tests-failed*)
            (begin
             (display "All tests passed.")
             (newline))
            (begin
             (display "TESTS FAILED: ")
             (display *tests-failed*)
             (newline)))))))

(define (print-header message)
  (newline)
  (display ";;; ")
  (display message)
  (newline))

;;;; Utility

(define (proc-or a b) (or a b))

(define (constantly x) (lambda (_) x))

(define (check-bit-conversions)
  (print-header "Checking bit conversions...")

  (check (bit->integer 0)  => 0)
  (check (bit->integer 1)  => 1)
  (check (bit->integer #f) => 0)
  (check (bit->integer #t) => 1)
  (check (bit->boolean 0)  => #f)
  (check (bit->boolean 1)  => #t)
  (check (bit->boolean #f) => #f)
  (check (bit->boolean #t) => #t))

(define (check-constructors)
  (print-header "Checking constructors...")

  (check (bitvector-length (make-bitvector 8))       => 8)
  (check (bitvector->list/int (make-bitvector 4 0))  => '(0 0 0 0))
  (check (bitvector->list/int (make-bitvector 4 #t)) => '(1 1 1 1))

  ;;; unfolds

  (check (bitvector->list/int
          (bitvector-unfold/int (lambda (i n) (values 0 0)) 4 0))
   => '(0 0 0 0))
  (check (bitvector->list/int
          (bitvector-unfold/int (lambda (i n) (values n (if (zero? n) 1 0)))
                                4
                                1))
   => '(1 0 1 0))
  (check (bitvector->list/int
          (bitvector-unfold/bool (lambda (i b) (values #f #f)) 4 #f))
   => '(0 0 0 0))
  (check (bitvector->list/int
          (bitvector-unfold/bool (lambda (i b) (values b (not b))) 4 #t))
   => '(1 0 1 0))
  (check (bitvector->list/int
          (bitvector-unfold-right/int (lambda (i n) (values 0 0)) 4 0))
   => '(0 0 0 0))
  (check (bitvector->list/int
          (bitvector-unfold-right/int
           (lambda (i n) (values n (if (zero? n) 1 0)))
           4
           1))
   => '(0 1 0 1))
  (check (bitvector->list/int
          (bitvector-unfold-right/bool (lambda (i b) (values #f #f)) 4 #f))
   => '(0 0 0 0))
  (check (bitvector->list/int
          (bitvector-unfold-right/bool (lambda (i b) (values b (not b))) 4 #t))
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

(define (check-predicates)
  (print-header "Checking predicates...")

  (check (bitvector? (bitvector))        => #t)
  (check (bitvector? (make-bitvector 1)) => #t)

  (check (bitvector-empty? (bitvector))   => #t)
  (check (bitvector-empty? (bitvector 1)) => #f)

  (check (bitvector= (bitvector) (bitvector)) => #t)
  (check (bitvector= (bitvector 1 0 0) (bitvector 1 0 0)) => #t)
  (check (bitvector= (bitvector 1 0 0) (bitvector 1 0 1)) => #f)
  (check (bitvector= (bitvector 1 0 0) (bitvector 1 0))   => #f)
  (check (bitvector= (bitvector 1 0 0)
                     (bitvector 1 0 0)
                     (bitvector 1 0 0))
   => #t)
  (check (bitvector= (bitvector 1 0 0)
                     (bitvector 1 0 1)
                     (bitvector 1 0 0))
   => #f))

(define (check-selectors)
  (print-header "Checking selectors...")

  (check (bitvector-length (bitvector))             => 0)
  (check (bitvector-length (bitvector 1 0 1 0))     => 4)
  (check (bitvector-ref/int (bitvector 1 0 1 0) 0)  => 1)
  (check (bitvector-ref/int (bitvector 1 0 1 0) 3)  => 0)
  (check (bitvector-ref/bool (bitvector 1 0 1 0) 0) => #t)
  (check (bitvector-ref/bool (bitvector 1 0 1 0) 3) => #f))

(define (check-bitvector-conversions)
  (print-header "Checking bitvector conversions...")

  (check (bitvector->list/int (bitvector))             => '())
  (check (bitvector->list/int (bitvector 1 0 1 0))     => '(1 0 1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 2)   => '(1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 1 3) => '(0 1)))

(define (check-all)
  ;; Check predicates, bitvector conversions, and selectors first,
  ;; since they're used extensively in later tests.
  (check-predicates)
  (check-bitvector-conversions)
  (check-selectors)
  (check-bit-conversions)
  (check-constructors)
  (check-iterators)

  (newline)
  (check-report))

(define (check-selectors)
  (print-header "Checking selectors...")

  (check (bitvector-length (bitvector))             => 0)
  (check (bitvector-length (bitvector 1 0 1 0))     => 4)
  (check (bitvector-ref/int (bitvector 1 0 1 0) 0)  => 1)
  (check (bitvector-ref/int (bitvector 1 0 1 0) 3)  => 0)
  (check (bitvector-ref/bool (bitvector 1 0 1 0) 0) => #t)
  (check (bitvector-ref/bool (bitvector 1 0 1 0) 3) => #f))

(define (check-bitvector-conversions)
  (print-header "Checking bitvector conversions...")

  (check (bitvector->list/int (bitvector))             => '())
  (check (bitvector->list/int (bitvector 1 0 1 0))     => '(1 0 1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 2)   => '(1 0))
  (check (bitvector->list/int (bitvector 1 0 1 0) 1 3) => '(0 1)))

(include "test/iterators.scm")

(define (check-all)
  ;; Check predicates, bitvector conversions, and selectors first,
  ;; since they're used extensively in later tests.
  (check-predicates)
  (check-bitvector-conversions)
  (check-selectors)
  (check-bit-conversions)
  (check-constructors)

  (newline)
  (check-report))
