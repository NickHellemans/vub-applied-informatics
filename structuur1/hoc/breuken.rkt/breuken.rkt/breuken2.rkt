(define (make-rat a b)
   (let ((g (gcd a b)))
      (cons (/ a g) (/ b g))))

(define (numer c) 
   (car c))

(define (denom c) 
   (cdr c))

; Onderstaande code is een copy-paste van dezelfde code uit "breuken1"
; dank zij het feit dat deze code geen enkele cons/car/cdr gebruikte
; werken deze operaties automatisch OOK met gesimplificeerde breuken

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