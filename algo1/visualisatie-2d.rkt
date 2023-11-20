#lang racket

(require plot)

(plot-new-window? #t)


(plot3d (surface3d (lambda (x y) (/ (sin (* 10 (+ (sqr x) (sqr y)))) 10))
                   -1 1 -1 1)
          #:title "f(x,y)=sin(10(x^2+y^2))/10"
          #:x-label "x" #:y-label "y" #:z-label "f(x,y)=sin(10(x^2+y^2))/10")

(plot3d (surface3d (lambda (x y) (* 10000 (* (sin (* 5 x)) (/ (cos (* 5 y)) 5))))
                   0 1000 0 10000)
          #:title "f(x,y)= sin(5x)*cos(5y)/5"
          #:x-label "x" #:y-label "y" #:z-label "f(x,y)=sin(5x)*cos(5y)/5")

(plot3d (surface3d (λ (n p) (+ n p))
                   0 100000 0 100000)
          #:title "f(x,y)=x+y"
          #:x-label "x" #:y-label "y" #:z-label "f(x,y)=x+y")

(plot3d (surface3d (λ (n p) (* n p))
                   0 100000 0 100000)
        
          #:title "f(x,y)=x*y"
          #:x-label "x" #:y-label "y" #:z-label "f(x,y)=x*y")

(plot3d (surface3d (λ (n p) n)
                   0 100000 0 400)
        
          #:title "f(x,y)=x"
          #:x-label "x" #:y-label "p" #:z-label "f(x,y)=x")


(plot3d (surface3d (λ (n p) (+ (* n n) (* p p)))
                   -1000 1000 -1000 1000)
        
          #:title "f(x,y) = (+ (* x x) (* y y)))"
          #:x-label "x" #:y-label "y" #:z-label "f(x,y) = (+ (* x x) (* y y)))")