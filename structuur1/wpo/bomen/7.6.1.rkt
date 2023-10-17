(define (atom? x)
    (not (pair? x)))
    
(define (deep-combine combiner null-val lst)
    (cond ((null? lst) null-val)
           ((atom? lst) lst)
           (else (combiner (deep-combine combiner null-val (car lst))
                           (deep-combine combiner null-val (cdr lst))))))