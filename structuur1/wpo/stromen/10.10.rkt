(load "streams.rkt")

(define matrix
	(cons-stream (enumerate-interval 1 3)
				 (cons-stream (enumerate-interval 4 6)
							  (cons-stream (enumerate-interval 7 9)
								           (cons-stream (enumerate-interval 10 12)
														the-empty-stream)))))

(print-stream matrix)


(define (accumulate-n op ne streams)
	(if (empty-stream? (head streams)) the-empty-stream
		(let ((heads (map-stream head streams))
			  (tails (map-stream tail streams)))
		(cons-stream (accumulate op ne heads)
		             (accumulate-n op ne tails)))))

(print-stream (accumulate-n + 0 matrix))

