(define (atom? x)
    (not (pair? x)))
    
(define (same-structure? lst1 lst2)
    (cond ((and (null? lst1) (null? lst2)) #t)
          ((and (atom? lst1) (atom? lst2)) #t)
          ((and (atom? lst1) (pair? lst2)) #f)
          ((and (atom? lst2) (pair? lst1)) #f)
          (else (and (same-structure? (car lst1) (car lst2)) (same-structure? (cdr lst1) (cdr lst2))))))
    

(#%require racket/trace)
(trace same-structure?)

