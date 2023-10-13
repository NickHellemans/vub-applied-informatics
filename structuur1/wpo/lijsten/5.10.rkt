(define (change el1 el2 lst)
  (cond ((null? lst)'())
        ((eq? (car lst) el1) (cons el2 (change el1 el2 (cdr lst))))
        (else (cons (car lst) (change el1 el2 (cdr lst))))))


(define (change-dmv-map el1 el2 lst)
  (map (lambda (x) (if (eq? x el1) el2 x)) lst))
