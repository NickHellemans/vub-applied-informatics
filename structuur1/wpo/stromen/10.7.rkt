(load "streams.rkt")

(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))