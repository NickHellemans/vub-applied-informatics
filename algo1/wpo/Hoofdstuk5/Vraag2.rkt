#lang r7rs
(import (scheme base)
        (scheme write)) ; display e.d.

; vraag 2: draai de richting van buitenste en binnenste loop om
;          (en doe in elke stap van beide iteraties een display van de vector)
(define (insertion-sort vector <<?)
   (define (>=? x y) (not (<<? x y))) ; We hebben >=? eigenlijk niet meer nodig hier.
   (let outer-loop ; buitenste loop
     ((outer-idx 1)) ; iteratievariabele
     (let ; begin body
         ((current (vector-ref vector outer-idx)))
       (display "outer ") (display outer-idx) (display "     ") (display vector) (newline) ; display
       (vector-set! 
        vector 
        (let inner-loop ; binnenste loop
          ((inner-idx (- outer-idx 1))) ; iteratievariabele
          (display "  inner ") (display inner-idx) (if (< inner-idx 0) (display "  ") (display "   ")) (display vector) (newline) ; display
          (cond ; begin body
            ((or (<= inner-idx -1)
                 (<<? (vector-ref vector inner-idx)
                      current))
             (+ inner-idx 1))
            (else
             (vector-set! vector (+ inner-idx 1) (vector-ref vector inner-idx))
             (inner-loop (- inner-idx 1)))))
        current)
       (if (< outer-idx (- (vector-length vector) 1))
         (outer-loop (+ outer-idx 1))))))

(define v (vector 7 9 4 8 3 0 1 2))
(insertion-sort v <)
(display v) (newline)
