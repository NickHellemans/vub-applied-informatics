(define (add-to-end el lst)
  (if (null? lst)
      (cons el'())
      (cons (car lst) (add-to-end el (cdr lst)))))


(define (add-to-end-iter el lst)
  (define (iter lst res)
    (if (null? lst)
        res 
        (iter (cdr lst) (cons (car lst) res))))
  (iter (reverse lst) (list el)))