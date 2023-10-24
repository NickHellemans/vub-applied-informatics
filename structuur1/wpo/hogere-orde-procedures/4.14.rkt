(define (compose f1 f2)
  (lambda (x) (f1 (f2 x))))


(define (do-n f n)
  (do ((i 0 (+ i 1)))
    ((>= i n))
    (f)))