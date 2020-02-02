#lang s-exp "transform.rkt"

;; like ANF, but no λ

(def (f x y)
  (assign a 4) ; expands to a constant 
               ; wrap this in constant
  (assign b (+ a a))
  (assign c (* a b))
  ; (define d (* a 2)) ; -> would be illegal
  c) ;; last expression doesn't need a name

