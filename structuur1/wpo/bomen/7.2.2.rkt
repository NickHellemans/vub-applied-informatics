(define (atom? x)
    (not (pair? x)))
    
(define (my-depth lst)
  (define (helper lst res)
    (cond
      ((null? lst) res)
      ((atom? lst) res)
      (else (helper (car lst) (+ res 1)))))
  (if (null? lst)
      0
      (let ((left (helper (car lst) 0))
            (right (helper (cdr lst) 0)))
        (if (> left right) left right))))

(define (depth lst)
  (cond
    ((null? lst) 0)
    ((atom? lst) 0)
    (else (max (+ 1 (depth (car lst)))
               (depth (cdr lst))))))