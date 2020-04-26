#lang racket

(require rackunit
         quickcheck
         rackunit/quickcheck
         "trace.rkt"
         "trace-core.rkt"
         "diff.rkt"
         "util.rkt"
         "test-util.rkt")

(module derivatives-1 "trace-lang.rkt"
  (require "diff.rkt")
  (provide (all-defined-out))

  (define (const_one x)
    ([partial/f 0 (λ (y) (+ x y))] 1.0))

  ;;

  (define (cube x) (* x (* x x)))

  (define cube_0  (partial/f 0 cube))
  (define (cube_0/expect x)  (* 3.0 (* x x)))

  (define Dcube/f (D/f cube))

  (define Dcube/r (D/r cube))

  (define (Dcube/expect x) (list (cube_0/expect x)))

  ;;

  (define (pow x n)
    (define (rec x n r)
      (if (<= n 0)
          r
          (rec x (- n 1) (* r x))))
    (rec x n 1.0))

  (define pow_0 (partial/f 0 pow))
  (define (pow_0/expect x n) (* n (pow x (- n 1))))

  ;;

  (define (f x y) (+ x (* y y)))

  (define f_0 (partial/f 0 f))
  (define (f_0/expect x y) 1.0)

  (define f_1 (partial/f 1 f))
  (define (f_1/expect x y) (* 2.0 y))

  (define Df/f (D/f f))
  (define Df/r (D/r f))

  (define (Df/expect x y) (list (f_0/expect x y)
                                (f_1/expect x y)))


  ;;

  (define (g1 x y)
    (car (cons (* y 2) (* x 3))))

  (define (g2 x y)
    (cdr (cons (* y 2) (* x 3))))

  (define Dg1/f (D/f g1))
  (define Dg1/r (D/r g1))

  (define (Dg1/expect x y) (list 0.0 2.0))

  (define Dg2/f (D/f g2))
  (define Dg2/r (D/r g2))

  (define (Dg2/expect x y) (list 3.0 0.0))

)



(require 'derivatives-1)

(module+ test
  (test-case "derivatives-1"
    (check-property
     (property ([x arbitrary-real])
               (top-val
                ((top-val =&) ((top-val const_one) (val->trace x))
                              (val->trace 1.0)))))

    (check-property
     (property ([x (gen-trace (choose-real -1e2 1e2))])
               (and
                (equal? (top-val ((top-val cube_0) x))
                        (top-val ((top-val cube_0/expect) x)))

                (equal? (top-val ((top-val Dcube/f) x))
                        (top-val ((top-val Dcube/expect) x)))

                (equal? (top-val ((top-val Dcube/r) x))
                        (top-val ((top-val Dcube/expect) x))))))

    (check-property
     (property ([x (gen-trace (choose-real 0.0 1e2))]
                [n (gen-trace (choose-integer 0 10))])
               (within-rel 1e-15
                           (top-val ((top-val pow_0) x n))
                           (top-val ((top-val pow_0/expect) x n)))))

    (check-property
     (property ([x (gen-trace (choose-real -1e5 1e5))]
                [y (gen-trace (choose-real -1e5 1e5))])
               (and
                (equal? (top-val ((top-val f_0) x y))
                        (top-val ((top-val f_0/expect) x y)))
                (equal? (top-val ((top-val f_1) x y))
                        (top-val ((top-val f_1/expect) x y)))
                (equal? (top-val ((top-val Df/f) x y))
                        (top-val ((top-val Df/expect) x y)))
                (equal? (top-val ((top-val Df/r) x y))
                        (top-val ((top-val Df/expect) x y))))))

    (check-property
     (property ([x (gen-trace (choose-real -1e10 1e10))]
                [y (gen-trace (choose-real -1e10 1e10))])
               (and
                (equal? (top-val ((top-val Dg1/f) x y))
                        (top-val ((top-val Dg1/expect) x y)))
                (equal? (top-val ((top-val Dg1/r) x y))
                        (top-val ((top-val Dg1/expect) x y)))
                (equal? (top-val ((top-val Dg2/f) x y))
                        (top-val ((top-val Dg2/expect) x y)))
                (equal? (top-val ((top-val Dg2/r) x y))
                        (top-val ((top-val Dg2/expect) x y))))))))


;; ----------------------------------------

(module derivatives-2 "trace-lang.rkt"
  (require "diff.rkt")
  (provide (all-defined-out))

  ;;

  (define (h x y)
    (list (* x y) (* x y) (* y y)))

  (define Dh/f (D/f h))

  ;; note D/r returns the "transpose" of D/f
  (define (Dh/f/expect x y)
    (list (list y y 0.0)
          (list x x (* 2.0 y))))

  (define Dh/r (D/r h))

  (define (Dh/r/expect x y)
    (list (list y x) (list y x) (list 0.0 (* 2.0 y))))

  ;;

)

(require 'derivatives-2)

(module+ test
  (test-case "derivatives-2"
    (check-property
     (property ([x (gen-trace (choose-real -1e2 1e2))]
                [y (gen-trace (choose-real -1e2 1e2))])
               (checks->preds
                (and
                 (check-within (top-val ((top-val Dh/f) x y))
                               (top-val ((top-val Dh/f/expect) x y))
                               0.0)
                 (check-within (top-val ((top-val Dh/r) x y))
                               (top-val ((top-val Dh/r/expect) x y))
                               0.0)))))))
