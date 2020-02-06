#lang racket

(require racket/syntax
         "../trace/util.rkt")


;; idea: exactly like trace, except instead of manipulating traces of
;; assignments, we directly manipulate syntax.

;; likely to need:
;; dfn-fresh
; dfn-get
; dfn-append
; dfn-add
; dfn-remove-duplicates
; dfn-filter-out (?)
; dfn-prune (?)
; dfn-last

(define (dfn-id dfn-form)
  (syntax-case dfn-form ()
    [(_ id rest ...) #'id]))

(define (last-id dfn-forms)
  (dfn-id (last dfn-forms)))

(define (dfn-build op . dfn-lists)
  (with-syntax ([(arg-ids ...) (map last-id dfn-lists)]
                [fresh-id (generate-temporary)]
                [op op])
    (remove-duplicates 
     (append (apply append dfn-lists) 
             (list #'(define fresh-id (op arg-ids ...)))))))

(module+ test
  (dfn-build #'+
             (list #'(define b 2) #'(define a 1)) 
             (list #'(define c 2))))
