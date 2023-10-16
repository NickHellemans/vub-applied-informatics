(define (display-n x n)
  (do ((counter 0 (+ counter 1)))
    ((>= counter n))
    (display x)))

(define (display2 x n)
  (if (> n 0)
      (begin
        (display x)
        (display2 x (- n 1)))))

(define (display3 x n)
  (cond
    ((> n 0) (display x)
             (display3 x (- n 1)))))

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



(define (parasol2 n)
  (define (triangle i)
    (if (< i n)
        (begin
          (display-n " " (- n i 1))
          (display-n "*" (+ (* 2 i) 1))
          (newline)
          (triangle (+ i 1)))))

  (define (stick i)
    (if (< i 3)
        (begin
          (display-n " " (- n 1))
          (display "*")(newline)
          (stick (+ i 1)))))
  (triangle 0)
  (stick 0))


      
