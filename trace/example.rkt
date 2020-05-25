#lang s-exp "trace-lang.rkt"

(require "diff.rkt")

(define (cube x) (* x (* x x)))
((A/r cube 1.0) 1)

(define (f x y) (* (+ x y) (* x x)))
((D/f f 1.0 0.0) 2 10)
((D/f f 0.0 1.0) 2 10)

((A/r f 1.0) 2 10)


(define (g x y) (list (* x x) y))
(trace-display ((D/f g 1.0 0.0) 2 3))
(trace-display ((D/f g 0.0 1.0) 2 3))

(trace-display ((A/r g (list 1.0 0.0)) 2 3))
(trace-display ((A/r g (list 0.0 1.0)) 2 3))

(define (h x y) (car (cdr (list (* x x) (* x y) (* y y)))))

((D/f (D/f h 1.0 0.0) 1.0 0.0) 3.0 4.0)

((A/r (A/r h 1.0) (list 1.0 0.0)) 3.0 4.0)

(define ∇h (A/r h 1.0))

(define (k x) (car (list x x)))

((A/r k 1.0) 3.0)

(define (m x y)
  (define z 1)
  (+ x y))

