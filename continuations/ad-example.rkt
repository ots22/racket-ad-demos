#lang racket

;; this uses the idea directly from Wang et al 2019 - Penultimate
;; Backpropagator

;; uses mutation

(require racket/control)

(require "../dual-numbers/dual-number.rkt")
(require "../dual-numbers/main.rkt")

(define (+k x y)
  (shift k
         ;; set the dual part of "sum" to 0.0 (do this by dropping the
         ;; dual part in both addends)
         (let ([sum (+ (dual-number (primal x) 0.0)
                       (dual-number (primal y) 0.0))])
           (begin0 (k sum)
             (set-dual-number-d! x (+ (dual x) (dual sum)))
             (set-dual-number-d! y (+ (dual y) (dual sum)))))))

(define ((grad f) . xs)
  (reset
   (let ((result (apply f xs)))
     (set-dual-number-d! result 1.0)
     xs)))

(define (f x y)
  (+k (+k x y) y))

((grad f) (dual-number 2 0) (dual-number 1 0))
