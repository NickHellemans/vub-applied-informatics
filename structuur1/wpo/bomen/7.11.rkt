(define organigram
  '(directeur
    (hoofd-verkoop (verkoopsleider-vlaanderen)
                   (verkoopsleider-brussel))
    (hoofd-productie (hoofd-inkoop (bediende1)
                                   (bediende2)
                                   (bediende3))
                     (hoofd-fakturen))
    (hoofd-administratie (hoofd-personeel)
                         (hoofd-boekhouding))))


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

(define (find? name organigram)
 (cond
 ((eq? name (boss organigram)) #t)
 (else (find-in? name (slaves organigram)))))

(define (find-in? name organigram)
 (cond
 ((null? organigram) #f)
 (else (or (find? name (car organigram))
 (find-in? name (cdr organigram))))))

(define (hierarchisch? name1 name2 organigram)
 (cond
   ((eq? name1 (boss organigram)) (find? name2 organigram))
   (else (hierarchisch-in? name1 name2 (slaves organigram)))))

(define (hierarchisch-in? name1 name2 organigram)
  (cond
    ((null? organigram) #f)
    (else (or (hierarchisch? name1 name2 (car organigram))
              (hierarchisch-in? name1 name2 (cdr organigram))))))


;7.11.3
(define (atom? el) (not (pair? el)))
(define (flatten lst)
  (cond
    ((null? lst) '())
    ((atom? lst) (list lst))
    (else (append (flatten (car lst))
                  (flatten (cdr lst))))))

(define (collegas p tree)
  
  (define (bazen-van tree path)
    (cond
      ((eq? p (boss tree)) (append path (flatten (slaves tree)))) 
      (else (bazen-van-in (slaves tree) (cons (boss tree) path)))))
 
  (define (bazen-van-in trees path)
    (cond
      ((null? trees) #f)
      (else (or (bazen-van (car trees) path)
                (bazen-van-in (cdr trees) path)))))
   (bazen-van tree '()))


