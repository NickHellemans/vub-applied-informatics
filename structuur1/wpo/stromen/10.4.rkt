(load "streams.rkt")

(define (fac n)
  (display "->") (display n) (newline)
  (if (= n 0)
	1
	(* n (fac (- n 1)))))


(define ds (cons-stream (fac 3) (cons-stream (fac 2) (cons-stream (fac 1) the-empty-stream))))

