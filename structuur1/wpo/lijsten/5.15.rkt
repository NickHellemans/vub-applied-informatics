(#%require racket/trace)

(define (rec-merge-n-2 lst1 lst2 n)
  (define (merge-helper lst1 lst2 ctr)
    (cond
      ((and (null? lst1) (null? lst2)) '())
      ((= ctr 0) (merge-helper lst2 lst1 n))
      ((< (length lst1) ctr) (append lst1 lst2))
      (else (cons (car lst1) (merge-helper (cdr lst1) lst2 (- ctr 1))))))
  (trace merge-helper)
  (merge-helper lst1 lst2 n))

(define (rec-merge-n lst1 lst2 n)
  (define (merge-helper lst1 lst2 ctr)
    (cond
      ((null? lst1) lst2)
      ((= ctr 0) (merge-helper lst2 lst1 n))
      (else (cons (car lst1) (merge-helper (cdr lst1) lst2 (- ctr 1))))))
  (trace merge-helper)
  (merge-helper lst1 lst2 n))

(define (iter-merge-n-2 lst1 lst2 n)
  (define (merge-helper lst1 lst2 res ctr)
    (cond
      ((and (null? lst1) (null? lst2)) res)
      ((= ctr 0) (merge-helper lst2 lst1 res n))
      ((< (length lst1) ctr) (append res lst1 lst2))
      (else (merge-helper (cdr lst1) lst2 (append res (list (car lst1))) (- ctr 1)))))
  (trace merge-helper)
  (merge-helper lst1 lst2 '() n))

(define (iter-merge-n lst1 lst2 n)
  (define (merge-helper lst1 lst2 res ctr)
    (cond
      ((null? lst1) (append res lst2))
      ((= ctr 0) (merge-helper lst2 lst1 res n))
      (else (merge-helper (cdr lst1) lst2 (append res (list (car lst1))) (- ctr 1)))))
  (trace merge-helper)
  (merge-helper lst1 lst2 '() n))

(define (my-super-merge-n lsts n)
  ;curr = huidige lijst
  ;rest = al de rest van de lijsten die nog verwerkt dienen te worden
  (define (merge-helper curr rest ctr)
    (cond
      ((and (null? curr) (null? rest)) '())
      ((null? rest) curr)
      ((null? curr) (merge-helper (car rest) (cdr rest) n))
      ((= ctr 0) (merge-helper (car rest) (append (cdr rest) (list curr)) n))
      (else (cons (car curr) (merge-helper (cdr curr) rest (- ctr 1))))))
  (trace merge-helper)
  (merge-helper (car lsts) (cdr lsts) n))

(define (super-merge-n lsts n)
  ;curr = huidige lijst
  ;rest = al de rest van de lijsten die nog verwerkt dienen te worden
  (define (merge-helper curr rest ctr)
    (cond
      ((and (null? curr) (null? rest)) '())
      ((null? curr) (merge-helper (car rest) (cdr rest) n))
      ((= ctr 0) (merge-helper (car rest) (append (cdr rest) (list curr)) n))
      (else (cons (car curr) (merge-helper (cdr curr) rest (- ctr 1))))))
  (if (null? lsts)
      '()
      (merge-helper (car lsts) (cdr lsts) n)))
      
    
  