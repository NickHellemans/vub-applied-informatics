(define (my-append-rec lst1 lst2)
  (if (and (null? lst1) (null? lst2))
      '()
    (if (null? lst1)
        (my-append lst2 lst1)
        (cons (car lst1) (my-append (cdr lst1) lst2)))))


(define (my-append-iter2 lst1 lst2)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst) (cons (car lst) res))))
  (iter  (iter lst1 '()) lst2))


(define (my-append-iter3 lst1 lst2)
  (define (iter lst1 lst2 res)
      (if (and (null? lst1) (null? lst2))
          res
          (if (null? lst1)
              (iter lst2 lst1 res)
              (iter (cdr lst1) lst2 (cons (car lst1) res)))))
    (reverse (iter lst1 lst2 '())))

(define (my-append3 lst1 lst2)
  (define (iter lst res)
    (cond
      ((null? lst) res)
      (else (iter (cdr lst)(cons (car lst) res)))))
  (iter (reverse lst1) lst2))


(define (my-append lst1 lst2)
  (define (iter lst res)
    (cond
      ((null? lst) (reverse res))
      (else (iter (cdr lst)(cons (car lst) res)))))
  (iter lst2 (reverse lst1)))