(define (display-n x n)
  (do ((counter 0 (+ counter 1)))
    ((>= counter n))
    (display x)))


(define (parasol n)
  (do ((i 0 (+ i 1)))
   ((>= i n))
    (display-n " " (- n i 1))
    (display-n "*" (+ (* 2 i) 1))
    (newline))
  (display-n " " (- n 1))
  (display "*")
  (newline)
  (display-n " " (- n 1) )
  (display "*")
  (newline)
  (display-n " "  (- n 1) )
  (display "*")
  (newline))




      
