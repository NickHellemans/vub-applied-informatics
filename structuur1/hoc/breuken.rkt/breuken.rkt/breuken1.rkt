(define (make-rat a b) 
   (cons a b))

(define (numer c) 
   (car c))

(define (denom c) 
   (cdr c))

(define (rat+ p q)
   (make-rat
      (+ (* (numer p) (denom q))
          (* (numer q) (denom p)))
      (* (denom p) (denom q))))

(define (rat- p q)
   (make-rat
      (- (* (numer p) (denom q))
          (* (numer q) (denom p)))
      (* (denom p) (denom q))))

(define (rat* p q)
   (make-rat
      (* (numer p) (numer q))
      (* (denom p) (denom q))))

(define (rat/ p q)
   (make-rat
      (* (numer p) (numer q))
      (* (denom p) (denom q))))