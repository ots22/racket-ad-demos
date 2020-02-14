#lang racket

(require "main.rkt")

((partial/f 1 (partial/f 0 (λ (x y) (+ x (* x y))))) 1 2)

;; perturbation confusion example (erroneously gets 2 -- x not "held
;; constant"):
(define (constant_one x) ((partial/f 0 (λ (y) (+ x y))) 1))
((partial/f 0 (λ (x) (* x (constant_one x)))) 1)

;; Erroneously gets 0:
((partial/f 0 (λ (x) ((partial/f 0 (λ (y) (* x y))) 2))) 1)
