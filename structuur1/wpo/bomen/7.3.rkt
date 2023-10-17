(define (atom? x)
    (not (pair? x)))
    
(define (fringe lst)
    (cond ((null? lst) '())
           ((atom? lst) (list lst))
           (else (append (fringe (car lst))
                         (fringe (cdr lst))))))