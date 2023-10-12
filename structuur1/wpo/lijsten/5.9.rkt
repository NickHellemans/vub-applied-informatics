(define (last-len lst)
  (if (null? lst)
      #f
  (let ((len (length lst)))
    (list-ref lst (- len 1)))))

(define (last lst)
  (define (iter prev lst)
    (if (null? lst)
        prev
        (iter (car lst) (cdr lst))))
  (iter #f lst))