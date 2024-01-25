#lang r7rs
(import (scheme base)
        (scheme write))
; vraag 7:
; 3e verbetering van quicksort, die terugschakelt naar insertion sort voor vectoren van lengte <= 10
(define (quicksort-new vector <<?)
  (define (insertion-sort-l-to-r l r) ; aangepaste insertion sort, die enkel van l tot r sorteert
    (define (>=? x y) (not (<<? x y)))
    (display "insertion sorting elements ") (display l) (display "-") (display r) (newline)
    (if (< l r)   
        (let outer-loop
          ((outer-idx (- r 1)))
          (let ((current (vector-ref vector outer-idx)))
            (vector-set! 
             vector 
             (let inner-loop
               ((inner-idx (+ 1 outer-idx)))
               (cond
                 ((or (>= inner-idx (+ r 1))
                      (>=? (vector-ref vector inner-idx)
                           current))
                  (- inner-idx 1))
                 (else
                  (vector-set! vector (- inner-idx 1) (vector-ref vector inner-idx))
                  (inner-loop (+ inner-idx 1)))))
             current)
            (if (> outer-idx l)
                (outer-loop (- outer-idx 1)))))))
  (define (swap i j) ; hulpfunctie
    (let ((keep (vector-ref vector i)))
      (vector-set! vector i (vector-ref vector j))
      (vector-set! vector j keep)))
  (define (shift-to-right i x) ; hulpfunctie
    (if (<<? (vector-ref vector i) x)
        (shift-to-right (+ i 1) x)
        i))
  (define (shift-to-left j x) ; hulpfunctie
    (if (<<? x (vector-ref vector j))
        (shift-to-left (- j 1) x)
        j))
  (define (partition pivot i j)
    (let ((shifted-i (shift-to-right i pivot))
          (shifted-j (shift-to-left j pivot)))
      (cond ((< shifted-i shifted-j)
             (swap shifted-i shifted-j)
             (partition pivot shifted-i (- shifted-j 1)))
            (else
             shifted-j))))
  (define (quicksort-main l r)
    (define k (- r l))
    (display "quicksorting elements ") (display l) (display "-") (display r) (newline)
    (cond ((> k 9) ; k is groot genoeg, roep quicksort opnieuw op
           (if (<<? (vector-ref vector r)
                    (vector-ref vector l))
               (swap l r))
           (let ((m (partition (vector-ref vector l) (+ l 1) (- r 1))))
             (swap l m)
             (quicksort-main l (- m 1))
             (quicksort-main (+ m 1) r)))
          ((> k 0) ; k is positief, maar niet groot genoeg, roep insertion sort op
           (insertion-sort-l-to-r l r))))
  (quicksort-main 0 (- (vector-length vector) 1))
  vector)
(display (quicksort-new (vector 8 3 2 1 8 3 2 1 0 9 5 7 4 6 0 9 5 7 8 3 2 1 0 9 5 7 4 6 4 6) <)) (newline)
