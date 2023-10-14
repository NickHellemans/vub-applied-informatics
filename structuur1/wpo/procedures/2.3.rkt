(define (my-and a b)
  (if a b #f))

(define (my-or a b)
  (if a #t b))

(define (my-if condition consequence alternative)
  (cond
    (condition consequence)
    (else alternative)))