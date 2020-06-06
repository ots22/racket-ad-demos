#lang racket

(provide pattern-lambda
         (rename-out [pattern-lambda pat-λ]))

(require (for-syntax syntax/parse))

;; like lambda, except all arguments are treated as pattern variables
;; for constructing a syntax object
(define-syntax pattern-lambda
  (syntax-parser
    [(_ (args ...) body)
     #'(lambda (args ...)
         (with-syntax ([args args] ...)
           body))]))
