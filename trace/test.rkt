#lang racket

(module test-expressions "trace-lang.rkt"
  (require "diff.rkt")
  (provide (all-defined-out))
  (define (const_one x)
    ([D/f 0 (λ (y) (+ x y))] 1.0))
  (define expect-one ((D/f 0 (λ (x) (* x (const_one x)))) 1.0))
  
  ;;
  
  (define (cube x) (* x (* x x)))
  (define Dcube_5 ((D/f 0 cube) 5))

  ;;

  (define (pow x n)
    (define (rec x n r)
      (if (<= n 0)
          r
          (rec x (- n 1) (* r x))))
    (rec x n 1.0))
  (define Dpow_2_5 ((D/f 0 pow) 2.0 5))

  ;;

  (define (f x y) (+ x (* y y)))
  (define Df_2_1 ((grad/f f) 2.0 1.0)))


(require rackunit
         "trace.rkt"
         'test-expressions)

(test-case "derivatives"
  (check-equal? (top-val expect-one) 1.0)
  (check-equal? (top-val Dcube_5) 75.0)
  (check-equal? (top-val Dpow_2_5) 80.0)
  (check-equal? (top-val Df_2_1) '(1.0 2.0)))
