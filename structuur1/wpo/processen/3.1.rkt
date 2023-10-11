(define (1+ x) (+ 1 x))
(define (1- x) (- x 1))

(define (rec-add a b)
  (if (= a 0)
      b
      (+ 1 (rec-add (1- a) b))))

(define (iter-add a b)
  (if (= a 0)
      b
      (iter-add (- a 1) (+ b 1))))