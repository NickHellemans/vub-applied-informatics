(#%require racket/trace)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) (product factor (next a) next b))))

(trace product)
(define (iter-product2 factor a next b)
  (define (iter factor a next b res)
    (if (> a b)
        res
        (iter factor (next a) next b (* res (factor a)))))
  (iter factor a next b 1))

(define (iter-product factor a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (factor a)))))
  (iter a 1))

(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))