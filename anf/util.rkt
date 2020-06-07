#lang racket

(provide pattern-lambda
         (rename-out [pattern-lambda pat-λ])
         syntax-class->predicate)

(require syntax/parse
         (for-syntax syntax/parse))

;; like lambda, except all arguments are treated as pattern variables
;; for constructing a syntax object
(define-syntax pattern-lambda
  (syntax-parser
    [(_ (args ...) body)
     #'(lambda (args ...)
         (with-syntax ([args args] ...)
           body))]))

(define-syntax-rule (syntax-class->predicate stx-class)
  (syntax-parser
    [(~var _ stx-class) #t]
    [_ #f]))
