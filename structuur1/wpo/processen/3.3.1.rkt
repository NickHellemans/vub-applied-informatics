(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(define (calc-e n)
  (if (= n 0)
      1
      (+ (/ 1 (factorial n))
         (calc-e (- n 1)))))


(define (calc-e2 n)
  (define (iter ctr prev res)
    (if (> ctr n)
        res
        (let ((new-fac (* ctr prev)))
          (iter (+ ctr 1) new-fac (+ res (/ 1 new-fac))))))
  (iter 1 1 1))

