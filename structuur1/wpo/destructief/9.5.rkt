(define (cycles? ring)
  (define (find current seen)
    (cond
      ((null? current) #f)
      ((memq current seen) #t)
      (else (find (cdr current)
                  (cons current seen)))))
  (find ring '()))
       
(cycles? '())


(cycles? '(1 2 3))


(define car-loop
    (let ((first (cons 'a '())))
      (set-car! first first)
      first))
(cycles? car-loop)


(define ring
    (let* ((last (cons '3 '()))
           (list (cons 1 (cons 2 last))))
      (set-cdr! last list)
      list))
(cycles? ring)