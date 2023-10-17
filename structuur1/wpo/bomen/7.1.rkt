(define (atom? x)
    (not (pair? x)))
    
(define (depth lst)

  (define helper lst res)
    (cond ((null? lst) 0)
           ((atom? lst) 1)
           (else (+ (helper (car lst) (+ res 1)))))
  (

