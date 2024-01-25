(define (atom? x)
    (not (pair? x)))
    
(define (leaf-count lst)
    (cond ((null? lst) 0)
           ((atom? lst) 1)
           (else (+ (leaf-count (car lst))
                    (leaf-count (cdr lst))))))



(define (counter tree)
  (cond
    ((null? tree) 0)
    ((atom? tree) 1)
    (else (+ (counter (car tree))
             (counter (cdr tree))))))