(define (depth-weird x)
  (define (depth-counter count y)
    (cond
      ((= y 1) count)
      ((even? y) (depth-counter (+ count 1) (/ y 2)))
      (else (depth-counter (+ count 1) (+ (* 3 y) 1)))))
  (depth-counter 0 x))


(define (depth-weird-rec x)
  (cond
    ((= x 1) 0)
    ((even? x) (+ 1 (depth-weird-rec (/ x 2))))
    (else (+ 1 (depth-weird-rec (+ (* 3 x) 1))))))
                 