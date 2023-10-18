(define (atom? x)
    (not (pair? x)))
    
(define (leaf-count lst)
    (cond ((null? lst) 0)
           ((atom? lst) 1)
           (else (+ (leaf-count (car lst)) (leaf-count (cdr lst))))))

(define (depth-and-leaf-count lst)
  (define (helper lst res ctr)
    (cond
      ((null? lst) (cons res ctr))
      ((atom? lst) (cons res ctr))
      (else (helper (car lst) (+ res 1) (+ ctr 1)))))
  (if (null? lst)
      0
      (let ((left (helper (car lst) 0 0))
            (right (helper (cdr lst) 0 0)))
        (if (> (car left) (car right)) (cons (car left) (+ (cdr left) (cdr right))) (cons (car right) (+ (cdr left) (cdr right)))))))

