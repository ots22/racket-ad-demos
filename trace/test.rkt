#lang racket

(require rackunit
         "trace.rkt")

(module derivatives-1 "trace-lang.rkt"
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
  (define Df_2_1_fm ((J/f f) 2.0 1.0))
  (define Df_2_1_rm ((J/r f) 2.0 1.0))

  ;;

  (define (gcar x y)
    (car (cons (* y 2) (* x 3))))
  (define Dgcar_1_1_fm ((J/f gcar) 1.0 1.0))
  (define Dgcar_1_1_rm ((J/r gcar) 1.0 1.0))

  ;;

  (define (gcdr x y)
    (cdr (cons (* y 2) (* x 3))))
  (define Dgcdr_1_1_fm ((J/f gcdr) 1.0 1.0))
  (define Dgcdr_1_1_rm ((J/r gcdr) 1.0 1.0))

  ;;

  )

(require 'derivatives-1)

(test-case "derivatives-1"
  (check-equal? (top-val expect-one) 1.0)
  (check-equal? (top-val Dcube_5) 75.0)
  (check-equal? (top-val Dpow_2_5) 80.0)

  (check-equal? (top-val Df_2_1_fm) '(1.0 2.0))
  (check-equal? (top-val Df_2_1_rm) '(1.0 2.0))

  (check-equal? (top-val Dgcar_1_1_fm) '(0.0 2.0))
  (check-equal? (top-val Dgcar_1_1_rm) '(0.0 2.0))

  (check-equal? (top-val Dgcdr_1_1_fm) '(3.0 0.0))
  (check-equal? (top-val Dgcdr_1_1_rm) '(3.0 0.0)))


;; ----------------------------------------

(module derivatives-2 "trace-lang.rkt"
  (require "diff.rkt")
  (provide (all-defined-out))

  (define (h x y)
    (list (* x y) (* x y) (* y y)))
  (define Dh_2_3_fm ((J/f h) 2.0 3.0))
  (define Dh_2_3_rm ((J/r h) 2.0 3.0))

)

(require 'derivatives-2)

(test-case "derivatives-2"
  ;; note J/r returns the "transpose" of J/f
  (check-equal? (top-val Dh_2_3_fm) '((3.0 3.0 0.0) (2.0 2.0 6.0)))
  (check-equal? (top-val Dh_2_3_rm) '((3.0 2.0) (3.0 2.0) (0.0 6.0))))
