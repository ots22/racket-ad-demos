#lang slideshow

(require plot/pict)

(require slideshow/code)
;(require scribble/eval)
(require slideshow/repl)

(slide
 #:title "racket in one slide"
  (code
   (code:comment "function application")
   (print x)
   (+ 1 2 3) (code:comment " => 6")
   (= (+ 9 16) 25)   (code:comment " => #t")
   
   (code:comment "quotation")
   (quote a)
   'a
   '(+ 1 2 3)

   (code:comment "conses and lists")
   (cons 1 2) (code:comment " => '(1 . 2)")
   (cons 1 (cons 2 3))    (code:comment " => '(1 . (2 . 3))")
                          (code:comment "      = '(1 2 . 3)")
   (cons 1 (cons 2 '()))  (code:comment " => '(1 . (2 . ()))")
                          (code:comment "      = '(1 2)")
   (list 1 2)             (code:comment " same ")

   (code:comment "anonymous functions")
   (lambda (x) (/ 1 x))

   ; higher order functions
   (map cos (list 0 (/ pi 4) (/ pi 2)))
   ; true/false
   ))


(slide
 #:title "racket in one slide"
 (let ()
   (define-exec-code (p r s)
     (+ 1 1)
     )
   p
  )
 )

(slide
 #:title "interactive"
 (repl-area #:width client-w #:height (* 0.7 client-h))
 )

(slide
 #:title "...well two slides"
 (code
  (code:comment "syntax")
  #'(f x)))


(slide
 (plot (list
        (function (λ (x) (sqrt x)) 0 10 #:label "(sqrt x)")
        (function (λ (x) (+ (* 0.25 x) 1)) 0 10 #:label "((derivative sqrt) x)"))))


; define a slide that, given a function, returns a slide with the plot, and it's derivative, like the above
;(define (slide f)
;  )
