#lang racket

(provide id* quoted-symbol lambda-list)

(require syntax/parse)

;; Are the identifiers in maybe-ids distinct from one another?
;; Elements that aren't identifiers are ignored.
;;
;; distinct-ids? : (listof syntax) -> boolean?
;;
(define (distinct-ids? maybe-ids)
  (define ids (filter identifier? maybe-ids))
  (= (length (remove-duplicates ids free-identifier=?))
     (length ids)))

(define-syntax-class id*
  #:description "identifier (or null)"
  #:opaque
  (pattern id:id #:attr [maybe-id 1] (list #'id))
  (pattern ()    #:attr [maybe-id 1] '()))

(define-syntax-class quoted-symbol
  #:description "quoted symbol"
  (pattern (quote id:id)))

(define-syntax-class lambda-list
  #:description "argument list"
  ;; Collect all argument ids into args-maybe-valid first,
  ;; then check them with ~parse after the cut. This
  ;; avoids an invalid argument always resulting in an
  ;; error from the final ~rest clause
  (pattern (;;
            args-maybe-valid ...
            ~! (~parse ((~var args id #:role "procedure argument") ...)
                       #'(args-maybe-valid ...))
            ~rest (~var rest-args id* #:role "procedure rest argument"))
           #:attr [maybe-rest-args 1] (attribute rest-args.maybe-id)
           #:fail-unless
           (distinct-ids? (flatten (syntax-e #'(args ... . rest-args))))
           "duplicate argument identifier"))
