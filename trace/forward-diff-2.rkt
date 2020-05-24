#lang racket

(provide directional-derivative/f)

(require memoize
         (for-syntax syntax/parse)
         "util.rkt"
         "trace.rkt"
         "trace-core.rkt"
         "trace-util.rkt"
         "trace-apply.rkt"
         "let-traced.rkt"
         "primitive-partial.rkt")

(define directional-derivative/f
  (val->trace
   (λ (f& . rs)
     (val->trace
      (λ xs
        (define dxs
          (make-immutable-hash (map cons (map top-id xs) rs)))
        ;; memoize 'rec' on top-id of y&
        (define tr (apply (top-val f&) xs))
        ;; d : symbol? -> trace?
        (define/memo (d y)
          ;; so we can make a recursive call inside 'traced' conveniently
          (define d& (val->trace d))
          (define y& (trace-get y tr))
          (hash-ref
           dxs y 
           (λ () (match (top-expr y&)
                   [(list 'cons a b) (traced (cons& (d& a) (d& b)))]
                   [(list 'car ls)   (traced (car& (d& ls)))]
                   [(list 'cdr ls)   (traced (cdr& (d& ls)))]
                   [(list 'cons-add a b) (traced (cons-add& (d& a) (d& b)))]
                   [(list 'cons-zero x) y&]
                   [(list op xs ...)
                    (let ([xs-trs (map (curryr trace-get tr) xs)])
                      (for/fold ([acc& (traced 0.0)])
                                ([x xs]
                                 [i (in-naturals)])
                        (let ([d-op (apply (partial i op) xs-trs)])
                          (traced (+& acc& (*& d-op (d& x)))))))]
                   ['()  (traced null&)]
                   [c    (traced 0.0)]))))
        (d (top-id tr)))))))
