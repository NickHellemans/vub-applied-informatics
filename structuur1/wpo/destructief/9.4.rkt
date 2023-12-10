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

(define r (make-ring 3))
;(print-ring r)


(define (left-rotate ring)
  (cdr ring))

;(print-ring (left-rotate r))

(define (right-rotate ring)
  (define (aux l)
    (if (not (null? l))
        (cond
          ((eq? (cdr l) ring) (set! ring l) ring)
          (else (aux (cdr l))))))
  (aux ring))

(define (right-rotat-opl r)
  (define (aux l)
    (if (eq? (cdr l) r)
        l
        (aux (cdr l))))
  (aux r))

(print-ring (right-rotate r))