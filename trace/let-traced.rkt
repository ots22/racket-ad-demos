#lang racket
(require (for-syntax syntax/parse)
         "trace.rkt"
         "trace-core.rkt")

(provide traced)

(define-syntax (traced stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([app (datum->syntax stx '#%app)]
                   [datum (datum->syntax stx '#%datum)])
       #'(let-syntax ([app (make-rename-transformer #'app&)]
                      [datum (make-rename-transformer #'datum&)])
           body ...))]))
