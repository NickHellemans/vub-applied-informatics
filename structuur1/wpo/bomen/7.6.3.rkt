(define (atom? x)
    (not (pair? x)))

(define (deep-map f lst)
    (cond ((null? lst) '())
           ((atom? lst) (f lst))
           (else (cons (deep-map f (car lst))
                       (deep-map f (cdr lst))))))

(define (deep-change e1 e2 lst)
  (define (change x)
    (if (equal? x e1) e2 x))
  (deep-map change lst))