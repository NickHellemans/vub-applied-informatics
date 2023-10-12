(define (create-set) '())

(define (empty? set) (null? set))

(define (element-of? el set)
  (cond
    ((empty? set) #f)
    ((= el (car set)) #t)
    ((< el (car set)) #f)
    (else (element-of? el (cdr set)))))

(define (insert el set)
  (cond
    ((empty? set) (list el))
    ((= el (car set)) set)
    ((< el (car set)) (cons el set))
    (else (cons (car set)
                (insert el (cdr set))))))

(define (delete el set)
  (cond
    ((empty? set) set)
    ((= el (car set)) (cdr set))
    ((< el (car set)) set)
    (else (cons (car set)
                (delete el (cdr set))))))

; De originele versie van union en intersection werken nog
; Maar nu kunnen we er ook voor opteren om de abstractie 'te doorbreken'
; en te beseffen dat we op basis van een andere implementatie (van dezelfde abstractie)
; toch een snellere versie van union en intersection kunnen maken.

(define (union set1 set2)
  (cond
    ((empty? set1) set2)
    ((empty? set2) set1)
    ((= (car set1) (car set2))
     (cons (car set1)
           (union (cdr set1) (cdr set2))))
    ((< (car set1) (car set2))
     (cons (car set1)
           (union (cdr set1) set2)))
    (else
     (cons (car set2)
           (union set1 (cdr set2))))))
  
(define (intersection set1 set2)
  (cond
    ((or (empty? set1) (empty? set2))
     (create-set))
    ((= (car set1) (car set2))
     (cons (car set1)
           (intersection (cdr set1) (cdr set2))))
    ((< (car set1) (car set2))
     (intersection (cdr set1) set2))
    (else
     (intersection set1 (cdr set2)))))