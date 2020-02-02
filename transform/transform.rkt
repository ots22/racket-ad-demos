#lang racket
(require (for-syntax racket/syntax))

(provide def
        ; assign
         #%module-begin
         #%top
         #%app
         #%top-interaction
         #%datum
         +
         *)

;; make sure define is in the correct form:
;; define is like "assignment" now

(define-for-syntax (handle-assignments stx)
  (syntax-case stx (assign)
    [(assign a e) #'(define a e)]
    [a #'a]))

(define-syntax (def stx)
  (syntax-case stx (assign)
    [(_ (f args ...) body ...) 
     (with-syntax ([(body* ...) (datum->syntax #'(body ...)
                                  (map handle-assignments 
                                       (syntax-e #'(body ...))))])
       #'(define (f args ...)
           body* ...))]
  ))




;; should be an error if used outside a def form
;; (define-syntax (assign stx)
;;   (syntax-case stx ()
;;     ;; check at this point that expr has the correct form
;;     [(_ var expr) #'(define var expr)]))
