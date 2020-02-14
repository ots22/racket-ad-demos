#lang s-exp "trace-lang.rkt"

(require "diff.rkt")

(define (cube x) (* x (* x x)))
((D/r cube) 1)

(define (f x y) (* (+ x y) (* x x)))
((partial/f 0 f) 2 10)
((partial/f 1 f) 2 10)

((D/r f) 2 10)


(define (g x y) (list (* x x) y))
(trace-display ((partial/f 0 g) 2 3))
(trace-display ((partial/f 1 g) 2 3))

(trace-display ((D/r g) 2 3))
(trace-display ((D/r g) 2 3))

(define (h x y) (car (cdr (list (* x x) (* x y) (* y y)))))

((D/f (D/f h)) 3.0 4.0)

((D/r (D/r h)) 3.0 4.0)

(define Dh (D/r h))

(define (k x) (car (list x x)))

((D/r k) 3.0)

(define (m x y)
  (define z 1)
  (+ x y))

