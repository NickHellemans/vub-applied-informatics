(define (rec-reverse lst)
  (if (null? lst)
      '()
      (append (rec-reverse (cdr lst)) (list (car lst)))))

(define (iter-reverse lst)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst) (append (list (car lst)) res))))
  (iter lst '()))