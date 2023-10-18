(define (atom? x)
    (not (pair? x)))

(define (bewerk-boom boom doe-blad doe-appel combiner init)
  (cond
    ((null? boom) init)
    ((atom? boom) init)
    ((equal? (car boom) 'appel) (doe-appel (cdr boom)))
    ((equal? (car boom) 'blad) (doe-blad (car boom)))
    (else (combiner (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                    (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))


(define (leafs-dmv-bewerk boom)
  (bewerk-boom  boom
                (lambda (blad) 1)
                (lambda (appel) 0)
                (lambda (car cdr) (+ car cdr))
                0))


(define (all-apples boom)
    (cond ((null? boom) '())
          ((atom? boom) '())
          ((equal? (car boom) 'appel) (list (cdr boom)))
          (else (append (all-apples (car boom)) (all-apples (cdr boom))))))

(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))

(define (all-apples-dmv-bewerk boom)
    (bewerk-boom  boom
                (lambda (blad) :::)
                (lambda (appel) (list appel))
                (lambda (car cdr) (append car cdr))
                '()))
