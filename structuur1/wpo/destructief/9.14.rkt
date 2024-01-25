(define (ontdubbel1! l)
  (let* ((curr-even '(()))
         (curr-odd '(()))
         (new-lst (cons curr-even curr-odd)))
    (define (iter curr-even curr-odd lst)
      (cond
        ((null? lst) (set-car! new-lst (cdr (car new-lst)))  (set-cdr! new-lst (cdr (cdr new-lst))) (set-cdr! curr-even '()) (set-cdr! curr-odd '()))
        ((even? (car lst))
         (set-cdr! curr-even lst)
         (iter lst curr-odd (cdr lst)))
        (else
         (set-cdr! curr-odd lst)
         (iter curr-even lst (cdr lst)))))
   (iter curr-even curr-odd l)
    new-lst))

(define (ontdubbel! l)

  (define (get-first f lst)
    (cond
      ((null? lst) '())
      ((f (cadr lst)) lst)
      (else (get-first f (cdr lst)))))
  
  (let* ((curr-even (if (even? (car l)) l (cdr l)))
         (curr-odd (if (odd? (car l)) l (cdr l)))
         (new-lst (cons curr-even curr-odd)))
    
    (define (iter curr-even curr-odd lst)
      (cond
        ((null? lst)(set-cdr! curr-even '()) (set-cdr! curr-odd '()))
        ((even? (car lst))
          (set-cdr! curr-even lst)
         (iter lst curr-odd (cdr lst)))
        (else
         (set-cdr! curr-odd lst)
         (iter curr-even lst (cdr lst)))))
   (iter curr-even curr-odd (cddr l))
    new-lst))


(define test '(1 2 3 4 5 6 7 8 9 10))

(ontdubbel! test)