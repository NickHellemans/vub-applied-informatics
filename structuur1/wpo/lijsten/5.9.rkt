(define (last lst)
  (if (null? lst)
      #f
  (let ((len (length lst)))
    (list-ref lst (- len 1)))))