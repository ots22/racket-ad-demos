#lang racket

(require rackunit
         quickcheck
         rackunit/quickcheck
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "diff.rkt"
         "util.rkt"
         "test-util.rkt"
         "let-traced.rkt")

(module+ test
  (test-case "datum"
    (define d (datum& . 1.0))
    (check-equal? (top-val d) 1.0)
    (check-equal? (top-expr d) '(constant 1.0))
    (check-equal? (length (trace-items d)) 1))

  (test-case "app"
    (check-equal? (top-val [traced (not& #t)]) #f))

  (test-case "lambda"
    (check-true (trace? (lambda& x x)))
    (check-equal? (top-val [traced ((lambda& (x y) (+& x y)) 1 2)]) 3)
    (check-equal? (top-val [traced (define& f (lambda& (x y) (+& x y)))
                                   (f 1 2)])
                  3)
    (check-equal? (top-val [traced ((lambda& x x) 1 2 3)])
                  (list 1 2 3))
    (check-equal? (top-val [traced ((lambda& x x) 1 2 3)])
                  (list 1 2 3)))

  (test-case "definitions"
    (define& (f) (val->trace 1))
    (check-equal? (top-val [traced (f)]) 1)

    (define& (g x . xs) (val->trace 1))
    (check-equal? (top-val [traced (g 1)]) 1)

    ;; curried function definition not yet supported
    (check-exn exn:fail? (λ () (expand #'(define& ((f) x) x) (void))))

    (define& (h x) x)
    ;; wrong number of arguments
    (check-exn exn:fail? (λ () (app& h)))
    ;; invalid empty trace
    (check-exn exn:fail? (λ () (app& h (trace-append)))))

  ;; property-based tests to check that the traced operators produce
  ;; the same result as their counterparts
  (test-case "traced ops"
    (for ([bin-op (list (cons + +&)
                        (cons - -&)
                        (cons * *&)
                        (cons / /&)
                        (cons expt expt&)
                        (cons = =&)
                        (cons < <&)
                        (cons > >&)
                        (cons <= <=&)
                        (cons >= >=&)
                        (cons cons cons&))])
      (define plain-op (car bin-op))
      (define traced-op (cdr bin-op))
      (with-check-info (['plain-op plain-op]
                        ['traced-op traced-op])
        (check-property
         (property ([x (gen-trace (choose-real -1e4 1e4))]
                    [y (gen-trace (choose-real -1e4 1e4))])
                   (equal?
                    (top-val [traced (traced-op x y)])
                    (plain-op (top-val x) (top-val y)))))))

    (check-property
     (property ([x (gen-trace (choose-real -1e4 1e4))])
               (= (top-val [traced (exp& x)])
                  (exp (top-val x)))))

    (check-property
     (property ([x (gen-trace (choose-real 0 1e+8))])
               (= (top-val [traced (log& x)])
                  (log (top-val x)))))

    (check-property
     (property ([xs (arbitrary-pair arbitrary-real
                                    (arbitrary-list arbitrary-real))])
               (let ([xs& (cons->trace xs)])
                 (and (equal? (top-val [traced (car& xs&)])
                              (car xs))
                      (equal? (top-val [traced (cdr& xs&)])
                              (cdr xs))))))

    (check-true (top-val [traced (not& #f)]))
    (check-false (top-val [traced (not& #t)]))

    (check-true (top-val [traced (null?& null&)]))
    (check-true (top-val [traced (pair?& (list& 1 2))]))
    (check-false (top-val [traced (pair?& null&)]))
    (check-false (top-val [traced (pair?& 1)]))

    )) ; test-case, module+

;; ----------------------------------------

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
     (property ([x (gen-trace (choose-real -1e4 1e4))])
               (top-val
                [traced (=& (const_one x) 1.0)])))

    (check-property
     (property ([x (gen-trace (choose-real -1e2 1e2))])
               (and
                (equal? (top-val [traced (cube_0 x)])
                        (top-val [traced (cube_0/expect x)]))

                (equal? (top-val [traced (Dcube/f x)])
                        (top-val [traced (Dcube/expect x)]))

                (equal? (top-val [traced (Dcube/r x)])
                        (top-val [traced (Dcube/expect x)])))))

    (check-property
     (property ([x (gen-trace (choose-real 0.0 1e2))]
                [n (gen-trace (choose-integer 0 10))])
               (within-rel 1e-15
                           (top-val [traced (pow_0 x n)])
                           (top-val [traced (pow_0/expect x n)]))))

    (check-property
     (property ([x (gen-trace (choose-real -1e5 1e5))]
                [y (gen-trace (choose-real -1e5 1e5))])
               (and
                (equal? (top-val [traced (f_0 x y)])
                        (top-val [traced (f_0/expect x y)]))
                (equal? (top-val [traced (f_1 x y)])
                        (top-val [traced (f_1/expect x y)]))
                (equal? (top-val [traced (Df/f x y)])
                        (top-val [traced (Df/expect x y)]))
                (equal? (top-val [traced (Df/r x y)])
                        (top-val [traced (Df/expect x y)])))))

    (check-property
     (property ([x (gen-trace (choose-real -1e10 1e10))]
                [y (gen-trace (choose-real -1e10 1e10))])
               (and
                (equal? (top-val [traced (Dg1/f x y)])
                        (top-val [traced (Dg1/expect x y)]))
                (equal? (top-val [traced (Dg1/r x y)])
                        (top-val [traced (Dg1/expect x y)]))
                (equal? (top-val [traced (Dg2/f x y)])
                        (top-val [traced (Dg2/expect x y)]))
                (equal? (top-val [traced (Dg2/r x y)])
                        (top-val [traced (Dg2/expect x y)])))))))

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
                 (check-within (top-val [traced (Dh/f x y)])
                               (top-val [traced (Dh/f/expect x y)])
                               0.0)
                 (check-within (top-val [traced (Dh/r x y)])
                               (top-val [traced (Dh/r/expect x y)])
                               0.0)))))))
