#lang r5rs

(define (maak-matrix n m)
  (define rijen (make-vector n '()))
  (do ((rij 0 (+ rij 1)))
    ((= rij n) rijen)
    (vector-set! rijen rij (make-vector m 0))))

(define (rijen A)
  (vector-length A))

(define (kolommen A)
  (vector-length (vector-ref A 0)))

(define (ij? A i j)
  (vector-ref (vector-ref A i) j))

(define (ij! A i j v)
  (vector-set! (vector-ref A i) j v))

(define (T A)
  (define AT (maak-matrix (kolommen A) (rijen A)))
  (do ((i 0 (+ i 1)))
    ((= i (rijen A)))
    (do ((j 0 (+ j 1)))
      ((= j (kolommen A)))
      (ij! AT j i (ij? A i j))))
  AT)

(define (times A B)
  (define C (maak-matrix (rijen A) (kolommen B)))
  (do ((i 0 (+ i 1)))
    ((= i (rijen A)))
    (do ((j 0 (+ j 1)))
      ((= j (kolommen B)))
      (ij! C i j (let lus ((k 0)
                           (som 0))
                   (if (= k (kolommen A))
                       som
                       (lus (+ k 1) (+ som (* (ij? A i k) (ij? B k j)))))))))
  C)

(define myA (maak-matrix 3 2))
(define myB (maak-matrix 2 4))
(ij! myA 0 0 1)
(ij! myA 0 1 2)
(ij! myA 1 0 3)
(ij! myA 1 1 4)
(ij! myA 2 0 5)
(ij! myA 2 1 6)
(ij! myB 0 0 1)
(ij! myB 0 1 2)
(ij! myB 0 2 3)
(ij! myB 0 3 4)
(ij! myB 1 0 5)
(ij! myB 1 1 6)
(ij! myB 1 2 7)
(ij! myB 1 3 8)