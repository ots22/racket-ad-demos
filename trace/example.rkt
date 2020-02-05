#lang s-exp "trace-lang.rkt"

(require "diff.rkt")

(define (cube x) (* x (* x x)))
((J/r cube) 1)

(define (f x y) (* (+ x y) (* x x)))
((D/f 0 f) 2 10)
((D/f 1 f) 2 10)

((J/r f) 2 10)


(define (g x y) (list (* x x) y))
(trace-display ((D/f 0 g) 2 3))
(trace-display ((D/f 1 g) 2 3))

(trace-display ((J/r g) 2 3))
(trace-display ((J/r g) 2 3))

(define (h x y) (car (cdr (list (* x x) (* x y) (* y y)))))

((J/f (J/f h)) 3.0 4.0)

((J/r (J/r h)) 3.0 4.0)

(define Dh (J/r h))

(define (k x) (car (list x x)))

((J/r k) 3.0)

(define (m x y)
  (define z 1)
  (+ x y))

