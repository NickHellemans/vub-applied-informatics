(define (bewerk-boom2 boom doe-blad doe-appel combiner init)
  (cond
    ((null? boom) init)
    ((atom? boom) init)
    ((equal? (car boom) 'appel) (doe-appel lst))
    ((equal? (car boom) 'blad) (doe-blad lst))
    (else (combiner (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                    (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))


(define (bewerk-boom boom doe-blad doe-appel combiner init)
  (define (helper boom)
    (cond
      ((null? boom) init)
           ((atom? boom)
            (cond
              ((equal? boom 'blad) (doe-blad boom))
              ((equal? boom 'appel) (doe-appel boom))))
           (else (combiner (helper (car boom)) (helper (cdr boom))))))
  (helper boom))