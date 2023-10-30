(define familieboom '(jan (piet (frans (tom)
                                       (roel))
                                (mie))
                          (bram (inge (bert (ina)
                                            (ilse))
                                      (bart))
                                (iris))
                          (joost (els (ilse)))))

;7.17.1
(define (find? name organigram)
 (cond
 ((eq? name (boss organigram)) #t)
 (else (find-in? name (slaves organigram)))))

(define (find-in? name organigram)
 (cond
 ((null? organigram) #f)
 (else (or (find? name (car organigram))
 (find-in? name (cdr organigram))))))

(define (atom? el) (not (pair? el)))
(define (count-tree lst)
  (cond
    ((null? lst) 0)
    ((atom? lst) 1)
    (else (+ (count-tree (car lst))
                  (count-tree (cdr lst))))))

(define (verdeel-democratisch tree budget)
  (/ budget (- (count-tree tree) 1)))



;7.17.2

(define (budget tree lst)
  (define (budget-calc tree lst)
    (cond
      ((null? lst) 0)
      (else (+ (car lst) (budget-calc-in (cdr tree) (cdr lst))))))
  
  (define (budget-calc-in tree lst)
    (cond
      ((null? tree) 0)
      (else (+ (budget-calc (car tree) lst)
               (budget-calc-in (cdr tree) lst)))))
  (budget-calc-in (cdr tree) lst))


;7.17.3

(define (verdeel tree budget)
  (cond
    ((null? (cdr tree)) (list (list (car tree) budget)))
    (else (append (verdeel-in (cdr tree) (/ budget (length (cdr tree))))))))

(define (verdeel-in tree budget)
  (cond
    ((null? tree) '())
    (else (append (verdeel (car tree) budget)
                  (verdeel-in (cdr tree) budget)))))
  





    
    