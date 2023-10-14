(define (sign number)
  (cond
    (( = number 0) 0)
    (( < number 0) -1)
    (else 1)))