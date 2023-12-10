(define (insert2! lst1 lst2)
  
  (define (last lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) lst)
      (else (last (cdr lst)))))

  (cond
    ((null? lst2) (display-lst lst1))
    (else (set-cdr! (last (car lst1)) (car lst2)) (insert! (cdr lst1) (cdr lst2)))))

(define (insert! lst1 lst2)
  (define (last lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) lst)
      (else (last (cdr lst)))))
  
  (define (inserter l1 l2)
    (cond
      ((null? l2))
      (else (set-cdr! (last (car l1)) (last (reverse l2))) (set-car! l2 (cdr l2)) (insert2! (cdr l1) (cdr l2)))))
  
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