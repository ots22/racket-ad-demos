#lang racket

;; this uses the idea directly from Wang et al 2019 - Penultimate
;; Backpropagator

;; uses mutation

(require racket/control)

(require "../forward/dual-number.rkt")
(require "../forward/main.rkt")

;; more operations over dual numbers

(define (+k x y)
  ;; dual part 0.0 in the sum (drop it in both addends with primal)
  (shift k
         (let ([sum (+ (dual-number (primal x) 0.0)
                       (dual-number (primal y) 0.0))])
           (display (format "about to call k with~% sum ~a. ~%x ~a, y
~a~%" sum x y))
           (begin0 (k sum)
             (display (format "after: ~%sum ~a ~% x ~a y ~a~%" sum x y))
             (set-dual-number-d! x (+ (dual x) (dual sum)))
             (set-dual-number-d! y (+ (dual y) (dual sum)))
             (display (format "after update x ~a, y ~a~%" x y))))))

(define ((grad f) . xs)
  (reset
   (let ((result (apply f xs)))
     (println "In grad")
     (set-dual-number-d! result 1.0)
     xs)))

;; (reset
;;  (let* ((a (dual-number 1.0 0.0))
;;         (b (dual-number 1.0 0.0))
;;         (result (+k (+k a b) b)))
;;    (set-dual-number-d! result 1.0)
;;    (list a b)))


(define (f x y)
  (+k (+k x y) y))

((grad f) (dual-number 2 0) (dual-number 1 0))



