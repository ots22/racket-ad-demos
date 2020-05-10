#lang racket

(require memoize
         (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "trace-apply.rkt"
         "let-traced.rkt"
         "primitive-partial.rkt")

(define& (directional-derivative/f f& r&)
  (val->trace
   (lambda xs
     (define dxs
       (make-immutable-hash (map cons (map top-id xs) (trace-e r&))))
     ;; memoize 'rec' on top-id of y&
     (define (helper tr)
       ;; d : symbol? -> trace?
       (define/memo (d y)
         ;; so we can make a recursive call inside 'traced' conveniently
         (define d& (val->trace d))
         (define y& (trace-get y tr))
         (hash-ref
          dxs y 
          (λ () (match (top-expr (trace-get y tr))
                  [(list 'constant '())  (traced null&)]
                  [(list 'constant c)    (traced 0.0)]
                  [(list 'app 'cons a b) (traced (cons& (d& a) (d& b)))]
                  [(list 'app 'car ls)   (traced (car& (d& ls)))]
                  [(list 'app 'cdr ls)   (traced (cdr& (d& ls)))]
                  [(list 'app op xs ...)
                   (let ([xs-trs (map (curryr trace-get tr) xs)])
                     (for/fold ([acc& (traced 0.0)])
                               ([x xs]
                                [i (in-naturals)])
                       (let ([d-op (apply (partial i op) xs-trs)])
                         (traced (+& acc& (*& d-op (d& x)))))))]))))
       (d (top-id tr)))
     (helper (apply (top-val f&) xs)))))
