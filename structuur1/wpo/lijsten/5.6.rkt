(define (add-to-end el lst)
  (if (null? lst)
      (cons el'())
      (cons (car lst) (add-to-end el (cdr lst)))))
      