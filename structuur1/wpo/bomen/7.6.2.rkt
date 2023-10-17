(define (atom? x)
    (not (pair? x)))

(define (deep-map f lst)
    (cond ((null? lst) '())
           ((atom? lst) (f lst))
           (else (cons (deep-map f (car lst))
                       (deep-map f (cdr lst))))))