#lang racket

(require plot)

(plot-new-window? #t)

(plot3d (surface3d (λ (n p) (+ n p))
                   0 100000 0 100000)
          #:title "KMP O(n+p)"
          #:x-label "n" #:y-label "p" #:z-label "n+p")

(plot3d (surface3d (λ (n p) (* n p))
                   0 100000 0 100000)
        
          #:title "Brute Force O(n.p)"
          #:x-label "n" #:y-label "p" #:z-label "n.p")

