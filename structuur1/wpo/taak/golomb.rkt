(define (golomb n)
  (if (<= n 1)
      1
      (+ 1 (golomb(- n (golomb ( golomb (- n 1))))))))


(define (golomb-reeks n)
  (do ((i 1 (+ i 1)))
    ((> i n))
    (display (golomb i))
    (display " ")))
       