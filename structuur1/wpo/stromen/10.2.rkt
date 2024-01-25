(load "streams.rkt")

(define (sum-odd-squares stream)
  (accumulate + 0 
              (map-stream (lambda (x) (* x x)) (streamfilter odd? stream))))