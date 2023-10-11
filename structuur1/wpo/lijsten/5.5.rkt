(define (list-procedure-rec lst)
  (if (null? lst)
      base-result
      (combine-car/res (do-something-with (car lst))
                       (list-procedure-rec (cdr lst)))))
 
(define (list-procedure-it lst)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst)
              (combine-car/res (do-something-with (car lst))
                               res))))
  (iter lst base-result))