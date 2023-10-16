(define (make-punt x y)
  (cons x y))

(define (x punt)
  (car punt))

(define (y punt)
  (cdr punt))

(define (make-segment start einde)
  (cons start einde))

(define (start-punt segment)
  (car segment))

(define (end-punt segment)
  (cdr segment))

(define (middelpunt segment)
  (let ((centerX (/ (+ (caar segment) (cadr segment)) 2))
        (centerY (/ (+ (cdar segment) (cddr segment)) 2)))
        (make-punt centerX centerY)))