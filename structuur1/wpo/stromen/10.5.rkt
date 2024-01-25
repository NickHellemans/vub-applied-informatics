(load "streams.rkt")

(define (show x)
  (display x) (newline)
  x)


(define x (map-stream show (enumerate-interval 0 10)))
(define x (map-stream show (enumerate-interval 0 10)))