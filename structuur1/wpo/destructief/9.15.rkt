(define (insert! lst1 lst2)
  (define (last lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) lst)
      (else (last (cdr lst)))))
  
  (define (inserter l1 l2)
    (cond
      ((null? l2))
      (else
       (let ((next-symbol (cdr l2)))
         (set-cdr! (last (car l1)) l2)
         (set-cdr! l2 '())
         (insert! (cdr l1) next-symbol)))))
  
  (inserter lst1 lst2)
  lst1)
    
        
 
(define (display-lst lst)
  (cond
    ((null? lst))
    (else (display (car lst)) (display-lst (cdr lst)))))




(define lst1 '((a 12 q) (b 13) (c 14 r s) (f 18) (j 22 t)))
(define lst2 '(v w x y z))
(insert! lst1 lst2)
(insert! '((1) (2) (3)) '(a b c))
(insert! '((1 1 1) (2 2) (3 3 3 3)) '(a b c))

 ;((a 12 q v) (b 13 w) (c 14 r s x) (f 18 y) (j 22 t z))