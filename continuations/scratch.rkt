#lang racket

(require racket/control)

;; shift/reset control/prompt are very similar
;; reset and prompt are aliases, in fact
(define (f x)
  (reset
   (+ 2 (shift k (+ 1 (shift k1 (k1 6)))))))

(define (g x)
  (prompt
   (+ 2 (control k (+ 1 (control k1 (k1 6)))))))

(define (skip) '())
(define (bye) (println "Capturing and discarding the continuation...") 42)
(prompt
 (let ([act (control k (begin
                         (k skip)
                         (k (λ () (prompt (control _ (bye)))))
                         (k skip)))])
   (act)
   (println "Doing stuff")))
