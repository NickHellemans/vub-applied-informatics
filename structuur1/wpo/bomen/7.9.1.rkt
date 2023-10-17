(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))


(define (atom? x)
    (not (pair? x)))
    
(define (leafs boom)
    (cond ((null? boom) 0)
           ((atom? boom) (if (equal? boom 'blad) 1 0))
           (else (+ (leafs (car boom)) (leafs (cdr boom))))))