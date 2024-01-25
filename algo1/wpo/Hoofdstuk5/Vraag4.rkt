#lang r7rs
(import (scheme base)
        (scheme write))

; vraag 4: gebruik een index vector in plaats van de oorspronkelijke input vector aan te passen

; hulpfunctie: maakt een nieuwe index vector van lengte n
; bv. (make-index-vector 5) -> #(0 1 2 3 4)
(define (make-index-vector n)
  (define index-vector (make-vector n 0))
  (define (iter i)
    (if (< i n)
        (begin (vector-set! index-vector i i)
               (iter (+ i 1)))
        index-vector))
  (iter 0))

(define (selection-sort vector <<?) ; we passen NOOIT meer vector aan
   ; maak een nieuwe index vector aan
   (define index-vector (make-index-vector (vector-length vector)))
   ; aan swap hoeven we niets aan te passen, wel aan wat we als eerste parameter meegeven bij oproep
   (define (swap vector i j)
     (let ((keep (vector-ref vector i)))
       (vector-set! vector i (vector-ref vector j))
       (vector-set! vector j keep)))
   (let outer-loop
     ((outer-idx 0))
     (swap index-vector ; !!! gebruik voortaan de index vector
           outer-idx 
           (let inner-loop
             ((inner-idx (+ outer-idx 1))
              (smallest-idx outer-idx))
             (cond 
               ((>= inner-idx (vector-length vector))
                smallest-idx)
               ((<<? (vector-ref vector (vector-ref index-vector inner-idx)) ; !!! vraag aan de index vector waar het inner-idx'ste element zich bevindt
                     (vector-ref vector (vector-ref index-vector smallest-idx))) ; !!! vraag aan de index vector waar het smallest-idx'ste element zich bevindt
                (inner-loop (+ inner-idx 1) inner-idx))
               (else
                (inner-loop (+ inner-idx 1) smallest-idx)))))
     (if (< outer-idx (- (vector-length vector) 1))
       (outer-loop (+ outer-idx 1))
       index-vector)))

; test
(define v (vector 4 3 5 7 2 6))
(display "Vector v VOOR het uitvoeren van onze selection sort: ") (display v) (newline)
(define iv (selection-sort v <))
(display "Vector v NA   het uitvoeren van onze selection sort: ") (display v) (display " = NIET AANGEPAST !!!") (newline)
(display "We hebben echter de index vector iv teruggekregen:   ") (display iv) (newline)
(let loop ((i 0))
  (if (< i (vector-length iv))
      (begin (display "  -> het ") (display i)
             (display "'e element na sorteren bevindt zich in input vector v op positie ")
             (display (vector-ref iv i)) (display ", en is dus ")
             (display (vector-ref v (vector-ref iv i))) (newline)
             (loop (+ i 1)))))
