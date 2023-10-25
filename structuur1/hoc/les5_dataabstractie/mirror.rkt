(define (mirror tree)
  (cond
    ((null? tree) tree)
    ((atom? tree) tree)
    (else (append (mirror (cdr tree))
                  (list (mirror (car tree)))))))

(define (atom? x) (not (pair? x)))