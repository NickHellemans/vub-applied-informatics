(#%require racket/trace)
(define (display-as-binary n)
  (cond
    ((= n 1) (display "1"))
    ((= n 0) (display "0"))
    (else
     (display-as-binary (quotient n 2))
     (cond ((odd? n) (display "1"))
           (else (display "0"))))))

(define (dpb n)
  (if (> n 1)
      (dpb (quotient n 2)))
  (cond
    ((odd? n) (display "1"))
    (else (display "0"))))

(define (dpb2 n)
  (if (> n 1)
      (dpb2 (quotient n 2)))
  (display (modulo n 2)))
          
;(trace display-as-binary)
