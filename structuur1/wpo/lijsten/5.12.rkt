(#%require racket/trace)

(define (rec-accumulate-lists lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) 0)
    ((null? lst1) (rec-sum-lists lst2 lst1))
    (else (+ (car lst1) (rec-sum-lists (cdr lst1) lst2)))))

(define (rec-sum-lists lst1 lst2)
  (cond
    ;((and (null? lst1) (null? lst2)) '())
    ((null? lst1) lst2)
    ((null? lst2) lst1)
    (else (cons (+ (car lst1) (car lst2)) (rec-sum-lists (cdr lst1) (cdr lst2))))))

(trace rec-sum-lists)

(define (iter-sum-lists lst1 lst2)
  (define (iter lst1 lst2 res)
    (cond
      ((and (null? lst1) (null? lst2)) res)
      ((null? lst1) (append res lst2))
      ((null? lst2) (append res lst1))
      (else (iter (cdr lst1) (cdr lst2) (append res (list(+ (car lst1) (car lst2))))))))
  (trace iter)
  (iter lst1 lst2 '()))


;oplossing
(define (iter-sum-lists2 lst1 lst2)
  (define (iter lst1 lst2 res)
    (cond
      ((and (null? lst1) (null? lst2)) (reverse res))
      ((null? lst1) (append (reverse res) lst2))
      ((null? lst2) (append (reverse res) lst1))
      (else (iter (cdr lst1) (cdr lst2) (cons (+ (car lst1) (car lst2)) res)))))
  (trace iter)
  (iter lst1 lst2 '()))
