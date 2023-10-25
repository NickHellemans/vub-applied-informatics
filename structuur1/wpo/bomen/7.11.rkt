

(define (boss tree)   (car tree))
(define (slaves tree) (cdr tree))

;7.11.1
(define (bazen-van tree p)
  (define (bazen-van tree path)
    (cond
      ((eq? p (boss tree)) path)
      (else (bazen-van-in (slaves tree) (cons (boss tree) path)))))
 
  (define (bazen-van-in trees path)
    (cond
      ((null? trees) #f)
      (else (or (bazen-van (car trees) path)
                (bazen-van-in (cdr trees) path)))))
  (bazen-van tree '()))

;7.11.2