(define lijst1 '(1 3 5))
(define lijst2 '(2 4 6 8))


(define (schuif-in! l1 l2)
  (define (merge-in curr r1 r2 ctr)
    (cond 
      ((null? r1) (set-cdr! curr r2))
      ((null? r2) (set-cdr! curr r1))
      ((eq? ctr 0)
       (merge-in curr r2 r1 1))
      (else (set-cdr! curr r1)
            (merge-in r1 (cdr r1) r2 (- ctr 1)))))

  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    (else 
     (let* ((curr l1)
            (r1 (cdr curr))
            (r2 l2))
       (merge-in curr r2 r1 1)
       curr))))


(schuif-in! lijst1 lijst2)
