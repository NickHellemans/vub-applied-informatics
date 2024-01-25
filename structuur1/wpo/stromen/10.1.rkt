(load "streams.rkt")

(define (fac n)
  (define (iter n res)
    (cond
      ((= 0 n) res)
      (else (iter (- n 1) (* res n)))))
  (iter n 1))

(define (calc-e n)
  (accumulate + 0
              (map-stream (lambda (x) (/ 1 x))
                          (map-stream fac
                                      (enumerate-interval 0 n)))))


(define (sinus x n)
  (define (calc-term t)
    (cond
      ((odd? (/ (- t 1) 2)) (* -1 (/ (expt x t) (fac t))))
      (else (/ (expt x t) (fac t)))))
  
  (accumulate + 0
              (map-stream calc-term
               (streamfilter odd?
                             (enumerate-interval 0 (* n 2))))))