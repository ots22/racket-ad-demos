#lang racket

(provide pattern-lambda
         (rename-out [pattern-lambda pat-λ])
         syntax-class->predicate
         bind-vars)

(require syntax/parse
         racket/syntax
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

(define (bind-vars vars expr [k identity])
  (if (null? vars)
      k
      (with-syntax ([expr expr]
                    [tmp (generate-temporary)]
                    [a (car vars)])
        (bind-vars (cdr vars) #'(cdr tmp)
                   (pattern-lambda (t) (k #'(let-values (((tmp) expr))
                                              (let-values (((a) (car tmp)))
                                                t))))))))
