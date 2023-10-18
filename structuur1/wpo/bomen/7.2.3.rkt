(define (atom? x)
    (not (pair? x)))
    
(define (leaf-count lst)
    (cond ((null? lst) 0)
           ((atom? lst) 1)
           (else (+ (leaf-count (car lst)) (leaf-count (cdr lst))))))


(define (depth lst)
  (cond
    ((null? lst) 0)
    ((atom? lst) 0)
    (else (max (+ 1 (depth (car lst)))
               (depth (cdr lst))))))

(define (depth-and-leaf-count lst)
    (cond
      ((null? lst) (cons 0 0))
      ((atom? lst) (cons 0 1))
     (else
      (let ((left (depth-and-leaf-count (car lst)))
            (right (depth-and-leaf-count (cdr lst))))
        (cons (max (+ 1 (car left))
                   (car right))
              (+ (cdr left) (cdr right)))))))