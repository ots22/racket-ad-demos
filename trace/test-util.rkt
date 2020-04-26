#lang racket

(provide gen-trace)

(require "trace.rkt"
         quickcheck)

;; Take a quickcheck generator and produce another that generates the
;; same values, but with a trace
;;
(define (gen-trace gen)
  (bind-generators ([v gen])
                   (val->trace v)))

