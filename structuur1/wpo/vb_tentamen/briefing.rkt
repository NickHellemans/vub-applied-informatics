(define (bereken n t)
  (if (= t 0)
      n
      (* 2 (bereken n (- t 1)))))

(define (bereken2 n t)
  (if (= t 0)
      n
      (bereken (* n 2) (- t 1))))
(#%require racket/trace)
(define (bereken4 f n p t)
  (define (iter n t res)
    (if (= t 0)
        res
        (iter (+ n (f n p)) (- t 1) (+ res (f n p)))))
  (trace iter)
  (+ n (iter n t 0)))

(define (bereken3 f n p t)
  (if (= t 0)
      0
      (+ (f n p) (bereken3 f (+ n (f n p)) p (- t 1)))))


(trace bereken3)
(trace bereken)