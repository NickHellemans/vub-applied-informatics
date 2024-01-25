(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(define ret3 '(a b c))

(count-pairs ret3)

(define ret4
  (let ((last (cons 'c '())))
    (cons last (cons 'b last))))

(count-pairs ret4)

(define ret7
  (let* ((last (cons 'c '()))
        (middle (cons last last)))
    (cons middle middle)))

(count-pairs ret7)

(define retInf
  (let* ((last (cons 'c '()))
         (first (cons 'a (cons 'b last))))
    (set-cdr! last first)
    first))


;(count-pairs retInf)