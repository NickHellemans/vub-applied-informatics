(load "streams.rkt")

(define (accumulate-n op ne streams)
	(if (empty-stream? (head streams)) the-empty-stream
		(let ((heads (map-stream head streams))
			  (tails (map-stream tail streams)))
		(cons-stream (accumulate op ne heads)
		             (accumulate-n op ne tails)))))

(define (transpose matrix)
 (accumulate-n (lambda (x y) (cons-stream x y)) the-empty-stream matrix))

(define (print-m matrix)
	(display "[")
	(print-stream (head matrix)) (newline)
	(stream-for-each (lambda (x) (display " ")
						(print-stream x) (newline))
					 (tail matrix))
	(display "]\n"))

(define matrix
	(cons-stream (enumerate-interval 1 3)
				 (cons-stream (enumerate-interval 4 6)
							  (cons-stream (enumerate-interval 7 9)
								           (cons-stream (enumerate-interval 10 12)
														the-empty-stream)))))
(print-m matrix)

(print-m (transpose matrix))

