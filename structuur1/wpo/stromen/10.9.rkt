(load "streams.rkt")

(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))
(define (triplets)
  (flatten-inf
   (map-stream (lambda (x)
                 (let ((i (car x))
                       (j (cdr x)))
                   (map-stream (lambda (k)
                                 (list i j k))
                               (enumerate-interval 1 (+ i j)))))
               (pairs integers integers))))