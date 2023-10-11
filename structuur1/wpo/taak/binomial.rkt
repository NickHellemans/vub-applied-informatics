(#%require racket/trace)

(define (binom n k)
  (if (> k n)
      0
      ;Base cases:
      ;als k = 0 -> binom = 1
      ;als k = n -> binom = 1
      (cond
        ((= k 0) 1)
        ((= k n) 1)
        (else (+ (binom (- n 1) (- k 1)) (binom (- n 1) k))))))


; De binom procedure levert een recursief process op. Dit process heeft als eigenschappen
; dat het resultaat zal opgebouwd worden als we uit de recursie afdalen
;(trace binom)

(define (pascal-driehoek n)
  (define (binom-line x)
    (do ((i 0 (+ i 1)))
      ((> i x))
      (display (binom x i))
      (if (not (= i x))
      (display "\t"))))
    
  (do ((i 0 (+ i 1)))
    ((> i n))
    (binom-line i)
    (newline)))







  