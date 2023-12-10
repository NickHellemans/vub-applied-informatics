(define (print-ring r)
  (define (aux l)
    (if (not (null? l))
        (cond ((eq? (cdr l) r) (display " ")
                               (display (car l))
                               (display "..."))
              (else (display " ")
                    (display (car l))
                    (aux (cdr l))))))
  (aux r))

(define (last-cell lst)
 (cond
 ((null? (cdr lst)) lst)
 (else (last-cell (cdr lst)))))

(define (make-ring n)
  (define (iter n res)
    (cond
      ((= n 0)
       (begin
         (let ((last (last-cell res)))
         (set-cdr! last (cons 0 res))
         res)))
      (else (iter (- n 1) (append res (list n))))))
  (iter n '()))

(define (make-ring-opl n)
  (define (help i prev)
    (if (> i n)
        prev
        (help (+ i 1)
              (cons i prev))))
  (let* ((last (cons 0 '()))
         (list (help 1 last)))
    (set-cdr! last list)
    list))

(define r (make-ring 3))
(print-ring r)