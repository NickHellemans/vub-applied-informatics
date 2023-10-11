(define (count1 x)
  (cond ((= 0 x) (display x))
        (else (display x)
              (count1 (- x 1)))))

(define (count2 x)
  (cond ((= 0 x) (display x))
        (else (count2 (- x 1))
              (display x))))