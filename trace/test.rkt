#lang racket

(require rackunit
         quickcheck
         rackunit/quickcheck
         "trace.rkt"
         "trace-core.rkt"
         "diff.rkt"
         "util.rkt"
         "test-util.rkt")

(module+ test
  (test-case "datum"
    (define d (datum& . 1.0))
    (check-equal? (top-val d) 1.0)
    (check-equal? (syntax->datum (top-expr d)) 1.0)
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
    ([D/f (λ (y) (+ x y)) 1.0] 1.0))

  ;;

  (define (cube x) (* x (* x x)))

  (define cube_0  (D/f cube 1.0))
  (define cube_0-memo  (D/f cube 1.0))
  (define (cube_0/expect x)  (* 3.0 (* x x)))

  (define (∇cube/f x) (list ((D/f cube 1.0) x)))
  (define (∇cube/f-memo x) (list ((D/f cube 1.0) x)))
  (define ∇cube/r (A/r cube 1.0))
  (define ∇cube/r-memo (A/r cube 1.0))
  (define (∇cube/expect x) (list (cube_0/expect x)))

  ;;

  (define (pow x n)
    (define (rec x n r)
      (if (<= n 0)
          r
          (rec x (- n 1) (* r x))))
    (rec x n 1.0))

  (define pow_0 (D/f pow 1.0 0.0))
  (define pow_0-memo (D/f-memo pow 1.0 0.0))
  (define (pow_0/expect x n) (* n (pow x (- n 1))))

  ;;

  (define (f x y) (+ x (* y y)))

  (define f_0 (D/f f 1.0 0.0))
  (define f_0-memo (D/f-memo f 1.0 0.0))
  (define (f_0/expect x y) 1.0)

  (define f_1 (D/f f 0.0 1.0))
  (define f_1-memo (D/f-memo f 0.0 1.0))
  (define (f_1/expect x y) (* 2.0 y))

  (define (∇f/f x y) (list (f_0 x y) (f_1 x y)))
  (define (∇f/f-memo x y) (list (f_0-memo x y) (f_1-memo x y)))
  (define ∇f/r (A/r f 1.0))
  (define ∇f/r-memo (A/r-memo f 1.0))
  (define (∇f/expect x y) (list (f_0/expect x y) (f_1/expect x y)))

  ;;

  (define (g1 x y)
    (car (cons (* y 2) (* x 3))))

  (define (g2 x y)
    (cdr (cons (* y 2) (* x 3))))

  (define (∇g1/f x y)
    (list ((D/f g1 1.0 0.0) x y)
          ((D/f g1 0.0 1.0) x y)))
  (define (∇g1/f-memo x y)
    (list ((D/f-memo g1 1.0 0.0) x y)
          ((D/f-memo g1 0.0 1.0) x y)))

  (define ∇g1/r (A/r g1 1.0))
  (define ∇g1/r-memo (A/r-memo g1 1.0))
  (define (∇g1/expect x y) (list 0.0 2.0))

  (define (∇g2/f x y)
    (list ((D/f g2 1.0 0.0) x y)
          ((D/f g2 0.0 1.0) x y)))
  (define (∇g2/f-memo x y)
    (list ((D/f-memo g2 1.0 0.0) x y)
          ((D/f-memo g2 0.0 1.0) x y)))

  (define ∇g2/r (A/r g2 1.0))
  (define ∇g2/r-memo (A/r-memo g2 1.0))
  (define (∇g2/expect x y) (list 3.0 0.0))

  ;;

  (define (g3 c1 c2)
    (cons (cdr c1) (car c1)))

  (define (Jg3/f c1 c2)
    (list (cons ((D/f g3 (cons 1.0 0.0) (cons 0.0 0.0)) c1 c2)
                ((D/f g3 (cons 0.0 1.0) (cons 0.0 0.0)) c1 c2))
          (cons ((D/f g3 (cons 0.0 0.0) (cons 1.0 0.0)) c1 c2)
                ((D/f g3 (cons 0.0 0.0) (cons 0.0 1.0)) c1 c2))))

  (define (Jg3/f-memo c1 c2)
    (list (cons ((D/f-memo g3 (cons 1.0 0.0) (cons 0.0 0.0)) c1 c2)
                ((D/f-memo g3 (cons 0.0 1.0) (cons 0.0 0.0)) c1 c2))
          (cons ((D/f-memo g3 (cons 0.0 0.0) (cons 1.0 0.0)) c1 c2)
                ((D/f-memo g3 (cons 0.0 0.0) (cons 0.0 1.0)) c1 c2))))

  (define (Jg3/expect c1 c2)
    (list (cons (cons 0.0 1.0) (cons 1.0 0.0))
          (cons (cons 0.0 0.0) (cons 0.0 0.0))))

  (define (J*g3/r c1 c2)
    (cons ((A/r g3 (cons 1.0 0.0)) c1 c2)
          ((A/r g3 (cons 0.0 1.0)) c1 c2)))

  (define (J*g3/r-memo c1 c2)
    (cons ((A/r-memo g3 (cons 1.0 0.0)) c1 c2)
          ((A/r-memo g3 (cons 0.0 1.0)) c1 c2)))

  (define (J*g3/expect c1 c2)
    (cons (list (cons 0.0 1.0) (cons 0.0 0.0))
          (list (cons 1.0 0.0) (cons 0.0 0.0))))

  ;;

  (define (g4 c1 c2)
    (cons-add
     (cons-zero c1)
     (cons-add (cons (* 2 (car c1)) (cdr c1))
               (cons (cdr c2) (* 2 (car c2))))))

  (define (g4_0/f c1 c2)
    (cons ((D/f g4 (cons 1.0 0.0) (cons-zero c2)) c1 c2)
          ((D/f g4 (cons 0.0 1.0) (cons-zero c2)) c1 c2)))

  (define (g4_0/f-memo c1 c2)
    (cons ((D/f-memo g4 (cons 1.0 0.0) (cons-zero c2)) c1 c2)
          ((D/f-memo g4 (cons 0.0 1.0) (cons-zero c2)) c1 c2)))

  (define (g4_0/r c1 c2)
    (cons (car ((A/r g4 (cons 1.0 0.0)) c1 c2))
          (car ((A/r g4 (cons 0.0 1.0)) c1 c2))))

  (define (g4_0/r-memo c1 c2)
    (cons (car ((A/r-memo g4 (cons 1.0 0.0)) c1 c2))
          (car ((A/r-memo g4 (cons 0.0 1.0)) c1 c2))))

  (define (g4_0/expect c1 c2)
    (cons (cons 2.0 0.0) (cons 0.0 1.0)))

  ;;
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
                (all-equal? (top-val [traced (cube_0 x)])
                            (top-val [traced (cube_0-memo x)])
                            (top-val [traced (cube_0/expect x)]))

                (all-equal? (top-val [traced (∇cube/expect x)])
                            (top-val [traced (∇cube/f x)])
                            (top-val [traced (∇cube/f-memo x)])
                            (top-val [traced (∇cube/r x)])
                            (top-val [traced (∇cube/r-memo x)]))
                )))

    (check-property
     (property ([x (gen-trace (choose-real 0.0 1e2))]
                [n (gen-trace (choose-integer 0 10))])
               (let ([pow_0-expected (top-val (traced (pow_0/expect x n)))])
                 (and
                  (within-rel 1e-15
                              (top-val [traced (pow_0 x n)])
                              pow_0-expected)
                  (within-rel 1e-15
                              (top-val [traced (pow_0-memo x n)])
                              pow_0-expected)))))

    (check-property
     (property ([x (gen-trace (choose-real -1e5 1e5))]
                [y (gen-trace (choose-real -1e5 1e5))])
               (and
                (all-equal? (top-val [traced (f_0/expect x y)])
                            (top-val [traced (f_0 x y)])
                            (top-val [traced (f_0-memo x y)]))

                (all-equal? (top-val [traced (f_1/expect x y)])
                            (top-val [traced (f_1 x y)])
                            (top-val [traced (f_1-memo x y)]))

                (all-equal? (top-val [traced (∇f/expect x y)])
                            (top-val [traced (∇f/f x y)])
                            (top-val [traced (∇f/f-memo x y)])
                            (top-val [traced (∇f/r x y)])
                            (top-val [traced (∇f/r-memo x y)])))))

    (check-property
     (property ([x (gen-trace (choose-real -1e5 1e5))]
                [y (gen-trace (choose-real -1e5 1e5))])
               (and
                (all-equal? (top-val [traced (∇g1/expect x y)])
                            (top-val [traced (∇g1/f x y)])
                            (top-val [traced (∇g1/f-memo x y)])
                            (top-val [traced (∇g1/r x y)])
                            (top-val [traced (∇g1/r-memo x y)]))

                (all-equal? (top-val [traced (∇g2/expect x y)])
                            (top-val [traced (∇g2/f x y)])
                            (top-val [traced (∇g2/f-memo x y)])
                            (top-val [traced (∇g2/r x y)])
                            (top-val [traced (∇g2/r-memo x y)])))))

    (check-property
     (property ([x1 (gen-trace (choose-real -1e5 1e5))]
                [x2 (gen-trace (choose-real -1e5 1e5))]
                [y1 (gen-trace (choose-real -1e5 1e5))]
                [y2 (gen-trace (choose-real -1e5 1e5))])
               (let ([c1 (traced (cons& x1 x2))]
                     [c2 (traced (cons& y1 y2))])
                 (and
                  (all-equal? (top-val [traced (Jg3/expect c1 c2)])
                              (top-val [traced (Jg3/f c1 c2)])
                              (top-val [traced (Jg3/f-memo c1 c2)]))
                  (all-equal? (top-val [traced (J*g3/expect c1 c2)])
                              (top-val [traced (J*g3/r c1 c2)])
                              (top-val [traced (J*g3/r-memo c1 c2)]))))))

    (check-property
     (property ([x1 (gen-trace (choose-real -1e5 1e5))]
                [x2 (gen-trace (choose-real -1e5 1e5))]
                [y1 (gen-trace (choose-real -1e5 1e5))]
                [y2 (gen-trace (choose-real -1e5 1e5))])
               (let ([c1 (traced (cons& x1 x2))]
                     [c2 (traced (cons& y1 y2))])
                 (all-equal?
                  (top-val [traced (g4_0/expect c1 c2)])
                  (top-val [traced (g4_0/f c1 c2)])
                  (top-val [traced (g4_0/f-memo c1 c2)])
                  (top-val [traced (g4_0/r c1 c2)])
                  (top-val [traced (g4_0/r-memo c1 c2)])))))
    ;;
    ))

;; ----------------------------------------

(module derivatives-2 "trace-lang.rkt"
  (require "diff.rkt")
  (provide (all-defined-out))

  ;;

  (define (h x y)
    (list (* x y) (* x y) (* y y)))

  (define (Jh/f x y)
    (list ((D/f h 1.0 0.0) x y)
          ((D/f h 0.0 1.0) x y)))

  (define (Jh/expect x y)
    (list (list y y 0.0)
          (list x x (* 2.0 y))))

  (define (J*h/r x y)
    (list ((A/r h (list 1.0 0.0 0.0)) x y)
          ((A/r h (list 0.0 1.0 0.0)) x y)
          ((A/r h (list 0.0 0.0 1.0)) x y)))

  (define (J*h/expect x y)
    (list (list y x)
          (list y x)
          (list 0.0 (* 2.0 y))))

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
                 (check-within (top-val [traced (Jh/f x y)])
                               (top-val [traced (Jh/expect x y)])
                               0.0)
                 (check-within (top-val [traced (J*h/r x y)])
                               (top-val [traced (J*h/expect x y)])
                               0.0)))))))
