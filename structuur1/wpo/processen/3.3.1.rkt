(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(define (calc-e n)
  (if (= n 0)
      1
      (+ (/ 1 (factorial n))
         (calc-e (- n 1)))))
