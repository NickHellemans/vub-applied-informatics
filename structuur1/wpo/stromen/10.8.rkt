(load "streams.rkt")

(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))
(define (integers-special stream)
  (streamfilter (lambda (x) (and (not (= (modulo x 2) 0)) (not (= (modulo x 3) 0)) (not (= (modulo x 5) 0)))) stream))