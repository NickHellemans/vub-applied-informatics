(define (search f a b)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((mid (average a b)))
    (if (close-enough? a b)
        mid
        (let ((value (f mid)))
          (cond
            ((> value 0) (search f a mid))
            ((< value 0) (search f mid b))
            (else mid))))))

(define (average x y)
  (/ (+ x y ) 2.0))
(define (square x) (* x x))

(#%require racket/trace)

(define (fixpoint f)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.000001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
 ; (trace try)
  (try 1.0))

(define (sqrt0 x)
  (fixpoint (lambda (y) (/ x y))))

(define (sqrt1 x)
  (fixpoint
   (lambda (y)
     (/ (+ y (/ x y)) 2.0))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt2 x)
  (fixpoint
   (average-damp
    (lambda (y) (/ x y)))))

(define (cube-root1 x)
  (fixpoint (lambda (y) (/ x (square y)))))

(define (cube-root2 x)
  (fixpoint
   (average-damp
    (lambda (y) (/ x (square y))))))