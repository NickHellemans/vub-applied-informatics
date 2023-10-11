(define (display-n x n)
  (do ((counter 0 (+ counter 1)))
    ((>= counter n))
    (display x)))

                   