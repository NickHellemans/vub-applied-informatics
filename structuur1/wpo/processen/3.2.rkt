(define (rec-multiply a b)
  (if (= a 0)
      0
      (+ b (rec-multiply (- a 1) b))))

(define (iter-multiply a b)
  (do ((result 0 (+ b result))
       (counter a (- counter 1)))
    ((= counter 0) result)))

(define (iter-rec a b)
  (define (help count result)
    (if (= count 0)
        result
        (help (- count 1) (+ result b))))
        (help a 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(#%require racket/trace)

(define (rec-fast-multiply a b)
  (cond
    ((= a 0) 0)
    ((even? a) (+ (double (rec-fast-multiply (halve a) b))))
    (else (+ b (rec-fast-multiply (- a 1) b)))))

(define (rfm a b)
  (cond
    ((= a 0) 0)
    ((even? a) (rec-fast-multiply (halve a) (double b)))
    (else (+ b (rec-fast-multiply (- a 1) b)))))

(define (iter-fast-multiply a b)
  (define (iter-counter a b res)
    (cond
      ((= a 0) res)
      ((even? a) (iter-counter (halve a) b (+ res (* (halve a) b))))
      (else (iter-counter (- a 1) b (+ res b)))))
  (trace iter-counter)
  (iter-counter a b 0))

(define (fm2 a b)
  (define (iter a b acc)
    (cond ((zero? b) acc)
          ((even? b) (iter (double a) (halve b) acc))
          (else (iter a (- b 1) (+ a acc)))))
  (iter a b 0))