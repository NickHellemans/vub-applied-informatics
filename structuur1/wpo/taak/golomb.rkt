(define (golomb n)
  (if (<= n 1)
      1
      (+ 1 (golomb(- n (golomb ( golomb (- n 1))))))))


(define (golomb-reeks n)
  (do ((i 1 (+ i 1)))
    ((> i n))
    (display (golomb i))
    (display " ")))


(define (golomb-reeks-rec n)
  (define (iter ctr)
    (cond
      ((= ctr n) (display (golomb ctr)))
      (else (display (golomb ctr)) (display " ") (iter (+ ctr 1)))))
  (iter 1))

(define (golomb-reeks-rec2 n)
  (cond
    ((= 1 n) (display (golomb n)) (display " "))
    (else (golomb-reeks-rec2 (- n 1)) (display (golomb n)) (display " "))))

(define (golomb-reeks-opl n)
  (define (iter ctr)
    (if (<= ctr n)
        (begin
          (display (golomb ctr))
          (display " ")
        (iter (+ ctr 1)))))
  (iter 1))