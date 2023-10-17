(define (atom? x)
    (not (pair? x)))
    
(define (depth lst)
    (define (helper lst)
      (cond ((null? lst) 0)
           ((atom? lst) 1)
           (else (+ 1 (depth (cdr lst))))))
  (if (null? lst)
      0
  (let ((left (helper (car lst)))
        (right (helper (cdr lst))))
    (if (> left right) left right))))