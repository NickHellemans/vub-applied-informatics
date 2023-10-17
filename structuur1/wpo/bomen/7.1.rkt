(define (atom? x)
    (not (pair? x)))
    
(define (tree-procedure-rec lst)
    (cond ((null? lst) base-result)
           ((atom? lst) atom-result)
           (else (combine-branches (tree-procedure-rec (car lst))
                                   (tree-procedure-rec (cdr lst))))))

