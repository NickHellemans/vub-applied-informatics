(define (create-set) '())

(define (empty? set) (null? set))

(define (element-of? el set)
  (cond
    ((empty? set) #f)
    ((equal? el (car set)) #t)
    (else (element-of? el (cdr set)))))

(define (insert el set)
  (if (element-of? el set)
      set
      (cons el set)))

(define (delete el set)
  (cond
    ((empty? set) set)
    ((equal? el (car set)) (cdr set))
    (else (cons (car set) (delete el (cdr set))))))

(define (union set1 set2)
  (if (empty? set1)
      set2
      (insert (car set1)
              (union (cdr set1) set2))))

(define (intersection set1 set2)
  (cond
    ((or (empty? set1) (empty? set2))
     (create-set))
    ((element-of? (car set1) set2)
     (cons (car set1)
           (intersection (cdr set1) set2)))
    (else (intersection (cdr set1) set2))))