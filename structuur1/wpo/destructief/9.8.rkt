(define (count-pairs x)
  (let ((seen '()))
    (define (count x)
      (cond
        ((not (pair? x)) 0)
        ((memq x seen) 0)
          (else (set! seen (cons x seen))
                (+ 1 (count (car x))
                   (count (cdr x))))))
    (count x)))


(define ret7
  (let* ((last (cons 'c '()))
        (middle (cons last last)))
    (cons middle middle)))

(count-pairs ret7)