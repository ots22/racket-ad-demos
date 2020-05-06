#lang racket

(provide apply&)

(require "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt")

(define& (apply& f xs)
  (apply (top-val f) (trace->cons xs)))
