#lang s-exp "trace-lang.rkt"

(define (const_one x)
  ([D 0 (λ (y) (+ x y))] 1.0))

(trace-display ((D 0 (λ (x) (* x (const_one x)))) 1.0))

(define (pow x n r)
  (if (<= n 0)
      r
      (pow x (- n 1) (* r x))))
