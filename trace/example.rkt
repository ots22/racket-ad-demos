#lang s-exp "trace-lang.rkt"

(require "diff.rkt")

(define (cube x) (* x (* x x)))
(((A/r cube) 1.0) 1.0)

(define (f x y) (* (+ x y) (* x x)))
(((D/f f) 2.0 10.0) 1.0 0.0)
(((D/f f) 2.0 10.0) 0.0 1.0)

(((A/r f) 2.0 10.0) 1.0)


(define (g x y) (list (* x x) y))
(trace-display (((D/f g) 2.0 3.0) 1.0 0.0))
(trace-display (((D/f g) 2.0 3.0) 0.0 1.0))

(trace-display (((A/r g) 2.0 3.0) (list 1.0 0.0)))
(trace-display (((A/r g) 2.0 3.0) (list 0.0 1.0)))

(define (h x y) (car (cdr (list (* x x) (* x y) (* y y)))))

(((D/f (λ (x y) (((D/f h) x y) 1.0 0.0))) 3.0 4.0) 1.0 0.0)

(((A/r (λ (x y) (((A/r h) x y) 1.0))) 3.0 4.0) (list 1.0 0.0))

(define ∇h (λ (x y) (((A/r h) x y) 1.0)))

(define (k x) (car (list x x)))

(((A/r k) 3.0) 1.0)

(define (m x y)
  (define z 1)
  (+ x y))

