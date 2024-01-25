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

;Eigen kweek
(define (merge2 b1 b2)
  (let ((new-lst '())
        (last-node '()))
    
  (define (merge-helper ptr1 ptr2)
    (cond
      ((null? ptr1) (set-cdr! last-node ptr2))
      ((null? ptr2) (set-cdr! last-node ptr1))
      ((element=? (car ptr1) (car ptr2)) (set-cdr! last-node ptr1) (set! last-node (cdr last-node)) (merge-helper (cdr ptr1) (cdr ptr2)))
      ((symbol<? (caar ptr1) (caar ptr2))
       (cond ((null? new-lst)
              (set! new-lst ptr1)
              (set! last-node ptr1)
              (merge-helper (cdr ptr1) ptr2))
             (else
              (set-cdr! last-node ptr1) (set! last-node (cdr last-node))
              (merge-helper (cdr ptr1) ptr2))))
      (else (set-cdr! last-node ptr2) (set! last-node (cdr last-node)) (merge-helper ptr1 (cdr ptr2)))))
    (merge-helper b1 b2)
    new-lst))

;opl
(define (merge b1 b2)
  (define (merge-in curr r1 r2)
    (cond 
      ((null? r1) (set-cdr! curr r2))
      ((null? r2) (set-cdr! curr r1))
      ((element=? (car r1) (car r2))
       (set-cdr! curr r1)
       (merge-in r1 (cdr r1) (cdr r2)))
      ((symbol<? (caar r1) (caar r2))
       (set-cdr! curr r1)
       (merge-in r1 (cdr r1) r2))
      (else (set-cdr! curr r2)
            (merge-in r2 r1 (cdr r2)))))

  (let* ((curr (if (symbol<? (caar b1) (caar b2)) b1 b2))
         (r1 (cdr curr))
         (r2 (if (eq? curr b1) b2 b1)))
    (merge-in curr r1 r2)
    curr))


(merge best1 best2)
