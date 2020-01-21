#lang racket

(require "forward.rkt")



;; ((D1 (D0 (λ (x y) (+ x (* x y))))) 1 2)


;; perturbation confusion example (erroneously gets 2 -- x not "held
;; constant"):
;; (define (constant_one x) ((D0 (λ (y) (+ x y))) 1))
;; ((D0 (λ (x) (* x (constant_one x)))) 1)

;; ;; with multiplication (erroneously gets 0):
;; ((D0 (λ (x) ((D0 (λ (y) (* x y))) 2))) 1)

;; ;; e.g.
;; ((λ (x) ((D0 (λ (y) (* x y))) 2)) 100) ;; => 100
;; (define (ex x) ((D0 (λ (y) (* x y))) 2))

(define (q x n)
  (if (= n 0)
      x
      (q (* x x) (- n 1))))
