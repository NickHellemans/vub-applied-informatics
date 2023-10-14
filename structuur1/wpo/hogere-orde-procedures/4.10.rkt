(define (filtered-accumulate combiner filter? null-value term a next b)
  (define (iter a res)
    (let ((termedA (term a)))
      (cond
        ((> a b) res)
        ((filter? termedA) (iter (next a) (combiner termedA res)))
        (else (iter (next a) res)))))
  (iter a null-value))

(define (my-product-gcd n)
  (define (iter i res)
    (cond
      ((>= i n ) res)
      ((= (gcd i n) 1) (iter (+ i 1) (* i res)))
      (else (iter (+ i 1) res))))
  (iter 1 1))

(define (product-gcd n)
  (filtered-accumulate * (lambda (x) (= (gcd x n) 1)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))