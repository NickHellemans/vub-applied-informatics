(define (accumulate combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner (term a) res))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (add a b)
  (accumulate + b (lambda (x) 1) (- a) (lambda (x) (+ x 1)) (- 1)))

(define (multiply a b)
  (accumulate + 0 (lambda (x) b) (- a) (lambda (x) (+ x 1)) (- 1)))

(#%require racket/trace)
(trace accumulate)