(define (atom? x)
    (not (pair? x)))

(define (deep-combine combiner null-val lst)
    (cond ((null? lst) null-val)
           ((atom? lst) lst)
           (else (combiner (deep-combine combiner null-val (car lst))
                           (deep-combine combiner null-val (cdr lst))))))

(define (deep-map f lst)
    (cond ((null? lst) '())
           ((atom? lst) (f lst))
           (else (cons (deep-map f (car lst))
                       (deep-map f (cdr lst))))))

(define (deep-atom-member? e lst)
  (define (is-e? x) (equal? x e))
  (define (my-or x y) (or x y))
  (deep-combine my-or #f (deep-map is-e? lst)))
