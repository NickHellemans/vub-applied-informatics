(define boom
  '((blad (appel . golden))
    (blad (appel . granny))
    (((appel . golden) blad) blad (appel . cox))))

(define boom2
  '((blad (appel . granny))
    (blad (appel . golden))
    (((appel . golden) blad))))

(define result  '(golden granny golden cox))

(define (create-set) '())
(define (empty? set) (null? set))

(define (atom? x)
    (not (pair? x)))

(define (element-of? el set)
 (cond
 ((null? set) #f)
 ((equal? el (car set)) #t)
 (else (element-of? el (cdr set)))))

(define (insert el set)
 (if (element-of? el set)
 set
 (cons el set)))

(define (union set1 set2)
 (if (null? set1)
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

(define (all-apples boom)
    (cond ((null? boom) '())
          ((atom? boom) '())
          ((equal? (car boom) 'appel) (list (cdr boom)))
          (else (append (all-apples (car boom)) (all-apples (cdr boom))))))



(define (apple-types boom)
  (define (iter lst res)
    (if (null? lst) (reverse res)
        (iter (cdr lst) (insert (car lst) res))))

  (let ((apples (all-apples boom)))
    (display apples)
    (newline)
    (iter apples '())))

    