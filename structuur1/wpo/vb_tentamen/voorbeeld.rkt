(#%require racket/trace)

;1
(define (opbrengst x c n)
    (if (= n 0)
        x
     (opbrengst (+ x (* (/ x 100.0) c)) c (- n 1))))

(define (opbrengst3 x c n)
  (define (helper x n)
    (if (= n 0)
        0
     (+ (* (/ x 100.0) c) (helper (+ x (* (/ x 100.0) c)) (- n 1)))))
  (+ x (helper x n)))

(define (opbrengst4 x c n)
    (define interest (+ 1 (* c 0.01)))
    (cond
        ((= n 0) x)
        (else (opbrengst (* x interest) c (- n 1)))
    )
)

(trace opbrengst)
(trace opbrengst3)
  
(define (opbrengst2 x c n)
 (define (iter x n res)
   (if (= n 0)
       res
       (iter (+ x (* (/ x 100.0) c)) (- n 1) (+  (* (/ x 100.0) c) res))))
  (iter x n x))



;;Niels

; Recursive
(define (opbrengstNR x c n)
    (define interest (+ 1 (* c 0.01)))
    (cond
        ((= n 0) x)
        (else (* (opbrengstNR x c (- n 1)) interest))
    )
)

(trace opbrengstNR)

; Iterative
(define (opbrengstNI x c n)
    (define interest (+ 1 (* c 0.01)))
    (cond
        ((= n 0) x)
        (else (opbrengstNI (* x interest) c (- n 1)))
    )
)
;2 a
(define (aantal-positief lst)
  (cond
    ((null? lst) 0)
    ((< (car lst) 0) (aantal-positief (cdr lst)))
    (else (+ 1 (aantal-positief (cdr lst))))))

;2 b
(define (aantal test lst)
  (cond
    ((null? lst) 0)
    ((test (car lst)) (+ 1 (aantal test (cdr lst))))
    (else (aantal test (cdr lst)))))

;2c
(define (aantal-positief2 lst)
  (aantal (lambda (x) (>= x 0)) lst))


;3 a

(define (schrap-3 lst)
  (define (helper lst ctr)
    (cond
      ((null? lst) '())
      ((= ctr 3) (helper (cdr lst) 1))
      (else (cons (car lst) (helper (cdr lst) (+ ctr 1))))))
  (helper lst 1))

;3b

(define (schrap-n n lst)
  (define (helper lst ctr)
    (cond
      ((null? lst) '())
      ((= ctr n) (helper (cdr lst) 1))
      (else (cons (car lst) (helper (cdr lst) (+ ctr 1))))))
  (helper lst 1))

;4

(define (controleer lijst test)
(if (null? lijst)
#t
(and (test (car lijst))
(controleer (cdr lijst) test))))


;5

;a
(define c 5)
(define (snelheid gewicht energie) (* c (/ energie gewicht)))

;b
(define (verdubbel-snelheid-1 gewicht energie)
  (let ((c (* 2 c)))
    (snelheid gewicht energie)))

;c

;(define (verdubbel-snelheid-2 gewicht energie)
;(let ((basisnelheid (snelheid gewicht energie))
      ;(eindsnelheid (* 2 basissnelheid)))
 ; eindsnelheid))


(define (bereken n t)
  (if (= t 0)
      n
      (+ (bereken (* 2 n) (- t 1)))))

(trace bereken)