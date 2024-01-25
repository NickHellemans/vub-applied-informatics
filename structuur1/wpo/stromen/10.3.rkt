(load "streams.rkt")




(define (odd-sum-triples max)

  (define (filter-all lst)
    (and (filter-odd lst) (filter-small lst) (filter-sum lst)))
 
  (define (filter-odd lst)
  (if (and (odd? (car lst)) (odd? (cadr lst))) #t #f))

(define (filter-small lst)
  (if (and (< (car lst) max) (< (cadr lst) max)) #t #f))

(define (filter-sum lst)
  (= (+ (car lst) (cadr lst)) (caddr lst)))
  
  (streamfilter filter-all
                (flatten
                 (map-stream (lambda (i)
                               (map-stream (lambda (j) 
                                             (list i j (+ i j)))
                                           (enumerate-interval 1 max)))           
                             (enumerate-interval 1 max)))))
