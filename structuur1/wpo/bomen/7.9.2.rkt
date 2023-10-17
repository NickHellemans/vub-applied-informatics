(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))

(define result  '(golden granny golden cox))

(define (atom? x)
    (not (pair? x)))
    
(define (all-apples boom)
    (cond ((null? boom) '())
          ((atom? boom) '())
          ((equal? (car boom) 'appel) (list (cdr boom)))
          (else (append (all-apples (car boom)) (all-apples (cdr boom))))))

(#%require racket/trace)
(trace all-apples)