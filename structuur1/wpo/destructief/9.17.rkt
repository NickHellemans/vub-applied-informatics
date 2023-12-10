(define best1 '((ann (meiboomstraat 12 1820 Eppegem))
                (bert (populierendreef 7 1050 Brussel))
                (kurt (Mechelsesteenweg 50 1800 Vilvoorde))))
 
(define best2 '((bert (populierendreef 7 1050 Brussel))
                (jan (eikestraat 1 9000 Gent))
                (sofie (boerendreef 5  2800 Mechelen))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))
 
(define (element=? el1 el2)
  (equal? el1 el2))


(define (merge b1 b2)
  (let ((new-lst '()))
    
  (define (merge-helper ptr1 ptr2)
    (cond
      ((null? ptr1) (set! new-lst (cons new-lst ptr2)))
      ((null? ptr2) (set! new-lst (cons new-lst ptr1)))
      ((symbol<? (caar ptr1) (caar ptr2)) (set! new-lst (cons new-lst (car ptr1))) (merge-helper (cdr ptr1) ptr2))
      (else (set! new-lst (cons new-lst (car ptr2))) (merge-helper ptr1 (cdr ptr2)))))
    (merge-helper b1 b2)
    new-lst))