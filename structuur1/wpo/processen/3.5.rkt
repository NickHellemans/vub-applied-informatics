(define (my-odd? x)
  (if (= x 0) #f (my-even? (- x 1))))

(define (my-even? x)
  (if (= x 0) #t (my-odd? (- x 1))))

