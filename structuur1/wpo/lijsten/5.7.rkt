(define (my-append-rec lst1 lst2)
  (if (and (null? lst1) (null? lst2))
      '()
    (if (null? lst1)
        (my-append lst2 lst1)
        (cons (car lst1) (my-append (cdr lst1) lst2)))))


(define (my-append lst1 lst2)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst) (cons (car lst) res))))
  (iter  (iter lst1 '()) lst2))


(define (my-append lst1 lst2)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst) (cons (car lst) (cons res '())))))
  (iter lst1 lst2))