(define (ontdubbel! l)
  (let ((new-lst (cons '() '())))
    (define (iter lst)
      (cond
        ((null? lst) '())
        ((even? (car lst)) (set-car! new-lst (cons (car lst) (car new-lst))) (iter (cdr lst)))
        (else (set-cdr! new-lst (cons (car lst) (cdr new-lst))) (iter (cdr lst)))))
    (iter l)
    (set-car! new-lst (reverse (car new-lst)))
    (set-cdr! new-lst (reverse (cdr new-lst)))
    new-lst))

(define (last l)
  (cond
    ((null? (cdr l)) l)
    (else (last (cdr l)))))

;NO EXTRA CONS CELLS
;Try:
; - Keep ref to last of new-lst
; - Point to node instead of creating new nodes to add
; - Draw list structure diagram
(define (ontdubbel2! l)
  (let ((new-lst (cons '() '())))
    (define (iter lst)
      (cond
        ((null? lst) '())
        ((even? (car lst))
         (if (null? (car new-lst))
             (set-car! new-lst lst)
             (set-cdr! (last (car new-lst)) lst))
         (iter (cdr lst)))
        (else (set-cdr! new-lst (cons (car lst) (cdr new-lst))) (iter (cdr lst)))))
    (iter l)
    (set-car! new-lst (reverse (car new-lst)))
    (set-cdr! new-lst (reverse (cdr new-lst)))
    new-lst))


(define test '(1 2 3 4 5 6 7 8 9 10))