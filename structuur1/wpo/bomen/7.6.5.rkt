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

(define (count-atoms lst)
  (define (map-to-one x) 1)
  (deep-combine + 0 (deep-map map-to-one lst)))