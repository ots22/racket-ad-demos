#lang racket

;; Idea of Wang et al 2019 "Demystifying Differentiable Programming:
;; Shift/Reset the Penultimate Backpropagator"

;; uses mutation

(require racket/control)
(module+ test
  (require rackunit))

;; shadow arithmetic operators with those working on both "number?"
;; and "dual-number?"
(require "../dual-numbers/main.rkt")

;; +k : dual-number? dual-number? -> dual-number?
(define (+k x y)
  (shift k
         ;; set the dual part of "sum" to 0.0 (do this by dropping the
         ;; dual part in both addends)
         (let ([sum (+ (dual-number (primal x) 0.0)
                       (dual-number (primal y) 0.0))])
           (begin0 (k sum)
             (set-dual-number-d! x (+ (dual x) (dual sum)))
             (set-dual-number-d! y (+ (dual y) (dual sum)))))))

;; grad : (dual-number? ... -> dual-number?) -> (Listof number?)
(define ((grad f) . xs)
  (get-dual-part
   (reset
    (let ((result (apply f xs)))
      (set-dual-number-d! result 1.0)
      xs))))

(module+ test
  (define (f x y)
    (+k (+k x y) y))

  (check-equal?
   ((grad f) (dual-number 10.0 0.0)
             (dual-number 5.0 0.0))
   '(1.0 2.0)))
