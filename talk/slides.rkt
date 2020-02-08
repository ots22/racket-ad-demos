#lang slideshow

(require plot/pict
         slideshow/code
         slideshow/text
         ;scribble/eval
         slideshow/repl
         ;rsvg
)

(module+ slideshow
  (start-at-recent-slide)
  (define racket-logo (bitmap "racket-logo.png"))
  (define λ-days-logo (bitmap "lambda-days-logo.png"))
  (define frac-client-h (blank (* 0.15 client-h) (* 0.15 client-h)))

  (define (stacked-rect #:color color . picts)
    (lambda ([fit-width (blank 0)])
      (define stacked (apply vc-append picts))
      (define rect
        (scale-to-fit (cellophane 
                       (filled-rectangle 1 1
                                         #:color color 
                                         #:draw-border? #f) 
                       0.1)
                      (vc-append stacked (scale-to-fit fit-width
                                                       (pict-width fit-width)
                                                       0
                                                       #:mode 'distort))
                      #:mode 'distort))

      (code-align (cc-superimpose rect stacked))))

  (plot-font-size 16)
  (plot-width (inexact->exact (round (* 0.8 client-w))))
  (plot-height (inexact->exact (round (* 0.8 client-h))))

  ;; ----------------------------------------

  {slide
   (titlet "A functional tour of automatic differentiation")
   (titlet "with Racket")
   (t "Oliver Strickson")
   (t "2020-02-14")
   
   (hc-append
    (scale-to-fit racket-logo frac-client-h)
    (blank (* 0.3 client-w) 0)
    (scale-to-fit λ-days-logo frac-client-h #:mode 'preserve/max))}
  
  ;; ----------------------------------------

  {slide
   ;; Me/Turing Institute intro slide
   }

  ;; ----------------------------------------

  {slide 
   #:title "Introduction"
   }

  ;; ----------------------------------------

  {slide 
   #:title "Differentiation"
   
   ;; 
   }

  ;; ----------------------------------------

  {slide
   #:title "Language-oriented programming"
   
   }

  ;; ----------------------------------------
  
  {slide 
   #:title "Overview"
   (item "Automatic differentiation algorithm")
   (item "Implementation by program tracing")
   (item "Program transformation")
   (item "Local program transformation: Dual numbers")
   (item "Local program transformation: Continuations")
   (item "Other resources")
   }
  
  {slide
   (para "Idea: every value returned by a program was computed by a"
         "sequence of arithmetic operations.")
   (para "Differentiate" (bt "that"))
   }

  {slide
   #:title "Example: sum of squares"
   }
  
  ;; more examples ...


  ;; forward and backwards ...


  {slide 
   #:title "Tracing program execution"
   
   (para "We want to make a particular type of trace, which is:")
   (item (para "flat"))
   (item (para "contains only" (it "primitive operations")))
   }

  {slide
   (para "  " (code (sum-squares x y)))
   (para "=>" (code (sum-squares 3 4)))
   (para "=>" (code 25))
   }

  (define x (stacked-rect #:color "red" (t ".") (t ".")  (code 3)))
  (define y (stacked-rect #:color "blue" (t ".") (t ".") (code 4)))
  (define z (stacked-rect #:color "yellow" (t ".") (t ".") (code 25)))

  {slide
   ;; (para "  " (code (sum-squares x y)))
   ;; (para "=>" (code (sum-squares #,(code-align (frame (vc-append (code 1) (code 2)))) #,(code-align (frame (vc-append (code 1) (code 2)))))))
   ;; (para "=>" (code #,(frame (code 5))))

   (para "   " (code (sum-squares x y)))

   (para "=>" (code (sum-squares #,(frame (x)) #,(frame (y)))))
   (para "=>" (code-align (frame (vc-append 
                                  (x (z))
                                  (y (z))
                                  (z)))))
   }

  (define x* (stacked-rect #:color "red" 
                           (tt "%1 | (constant 3)  |  3")))

  (define y* (stacked-rect #:color "blue"
                           (tt "%2 | (constant 4)  |  4")))

  (define z* (stacked-rect #:color "yellow"
                           (tt "%3 | (app * %1 %1) |  9")
                           (tt "%4 | (app * %2 %2) | 16")
                           (tt "%5 | (app + %3 %4) | 25")))

  {slide
   ;; (para "  " (code (sum-squares x y)))
   ;; (para "=>" (code (sum-squares #,(code-align (frame (vc-append (code 1) (code 2)))) #,(code-align (frame (vc-append (code 1) (code 2)))))))
   ;; (para "=>" (code #,(frame (code 5))))

   (para (code x))
   (para "=>" (frame (x*)))

   (para (code y))
   (para "=>" (frame (y*)))

   (para (code (sum-squares x y)))

   ;(para "=>" (code (sum-squares #,(frame x*) #,(frame y*))))
   (para "=>" (code-align (frame (vc-append (x*) (y*) (z*)))))
   
   }


  {slide 
   #:title "Interposition points"
   ;; #%datum, #%app etc

   ;; one thing we could do is permit literals, and handle non-traces
   ;; when we come across them.  Better is to convert them
   ;; immediately.  If we encounter a value that isn't a trace, it is
   ;; an error.

   ;; break out to DrRacket (maybe - with no hiding produces a lot of stuff)!

   ;; macro step a simple example (+ 1 2)
   ;; (+ 1 2)
   ;; (#%app + 1 2)
   ;; (#%app + (#%datum . 1) (#%datum . 2))
   ;;
   ;; where #%app, #%datum etc belong to the base language

   }

  ;; ...

  {slide
   ;; recap when we've finished the trace, and demonstrate it in a
   ;; repl a few times, without, then with trace-show
   }


  ;; functions-as-values too (need to extend trace)?

  {slide
   ;; the rename-out trick
   (item "Can refer to both the new and old name in the program")
   (item "External interface is only by the new name")
   }

  {slide
   (big (t "http://github.com/ots22/rackpropagator"))
   }

  {slide
   #:title "References"
   
   }

  );; module slideshow



  ;; {slide
  ;;  (plot (list
  ;;         (function (λ (x) (sqrt x)) 0 10 
  ;;                   #:label "sqrt(x)")
  ;;         (function (λ (x) (+ (* 0.25 x) 1)) 0 10
  ;;                   #:label "D sqrt(x)")))}



;; (module+ slideshow
;;   (slide
;;    #:title "racket in one slide"
;;    (code
;;     (code:comment "function application")
;;     (print x)
;;     (+ 1 2 3) (code:comment " => 6")
;;     (= (+ 9 16) 25)   (code:comment " => #t")

;;     (code:comment "quotation")
;;     (quote a)
;;     'a
;;     '(+ 1 2 3)

;;     (code:comment "conses and lists")
;;     (cons 1 2) (code:comment " => '(1 . 2)")
;;     (cons 1 (cons 2 3))    (code:comment " => '(1 . (2 . 3))")
;;     (code:comment "      = '(1 2 . 3)")
;;     (cons 1 (cons 2 '()))  (code:comment " => '(1 . (2 . ()))")
;;     (code:comment "      = '(1 2)")
;;     (list 1 2)             (code:comment " same ")

;;     (code:comment "anonymous functions")
;;     (lambda (x) (/ 1 x))

;;     ; higher order functions
;;     (map cos (list 0 (/ pi 4) (/ pi 2)))
;;     ; true/false
;;     ))


;;   (slide
;;    #:title "racket in one slide"
;;    (let ()
;;      (define-exec-code (p r s)
;;        (+ 1 1)
;;        )
;;      p
;;      )
;;    )

;;   (slide
;;    #:title "interactive"
;;    (repl-area #:width client-w #:height (* 0.7 client-h))
;;    )

;;   (slide
;;    #:title "...well two slides"
;;    (code
;;     (code:comment "syntax")
;;     #'(f x)))


;;   (slide
;;    (plot (list
;;           (function (λ (x) (sqrt x)) 0 10 #:label "(sqrt x)")
;;           (function (λ (x) (+ (* 0.25 x) 1)) 0 10 #:label "((derivative sqrt) x)"))))


;;   ; define a slide that, given a function, returns a slide with the plot, and it's derivative, like the above
;;   ;(define (slide f)
;;   ;  )

;; ) ; module slideshow
