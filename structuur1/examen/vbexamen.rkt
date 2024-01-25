
;1a
(define deelnemers '((123 jan) (456 famke) (789 nick)))
(define stortingen '((123 12.5) (789 12.5)))

(define (vind nummer deelnemers)
  (cond
    ((null? nummer) #f)
    ((null? deelnemers) #f)
    ((eq? nummer (caar deelnemers)) (car deelnemers))
    (else (vind nummer (cdr deelnemers)))))
  
(define (betaald deelnemers stortingen)
  (cond
    ((null? stortingen) '())
    (else
     (let ((deelnemer (vind (caar stortingen) deelnemers)))
      (cond
        ((eq? deelnemer #f) (betaald deelnemers (cdr stortingen)))
        (else (cons deelnemer (betaald deelnemers (cdr stortingen)))))))))
;1b

(define (betaald-iter deelnemers stortingen)
  (define (iter stortingen res)
    (cond
      ((null? stortingen) (reverse res))
      (else
       (let ((deelnemer (vind (caar stortingen) deelnemers)))
         (cond 
           ((eq? deelnemer #f) (iter (cdr stortingen) res))
           (else (iter (cdr stortingen) (cons deelnemer res))))))))
  (iter stortingen '()))

;1c
(define (select deelnemers keep? data)
    (define (iter data res)
      (cond
        ((null? data) (reverse res))
        (else
         (let ((el (vind (caar data) deelnemers)))
           (cond 
             ((not (keep? el (car data))) (iter (cdr data) res))
             (else (iter (cdr data) (cons el res))))))))
  (iter data '()))


;1d
(define resultaten '((123 15 2 3) (456 11 0 0) (789 4 5 10)))
(define (favorieten deelnemers resultaten)
  (define (keep el data)
    (if (not (eq? el #f))
    (> (cadr data) (caddr data)) #f))
  (select deelnemers keep resultaten))

;2a

(define (atom? x)
    (not (pair? x)))

(define deelboom '((bord O O #f X X #f X O X)
                   ((bord O O O X X #f X O X))
                   ((bord O O #f X X O X O X) ((bord O O X X X O X O X)))))


(define (count-veld veld)
  (define (iter veld res)
    (cond
      ((null? veld) res)
      (else
       (cond
         ((eq? (car veld) 'X) (iter (cdr veld) (cons (+ (car res) 1) (cdr res))))
         ((eq? (car veld) 'O) (iter (cdr veld) (cons (car res) (+ (cdr res) 1))))
         (else (iter (cdr veld) res))))))
  (if (not (eq? (car veld) 'bord))
      (cons 0 0)
      (iter (cdr veld) (cons 0 0))))
  
(define (combine-counted c1 c2)
  (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

(define (leaf-count lst)
    (cond
      ((null? lst) (cons 0 0))
      ((eq? (car lst) 'bord) (count-veld lst))
      (else (combine-counted (leaf-count (car lst)) (leaf-count (cdr lst))))))

;2b
(define (compare-depth r1 r2)
  (let ((newMin (if (< (car r1)  (car r2)) (car r1) (car r2)))
        (newMax (if (> (cdr r1) (cdr r2)) (cdr r1) (cdr r2))))
    (cons newMin newMax)))

(define (depth? bord res)
  (cond
    ((eq? (car bord) 'bord) res)
    (else (depth-in? (cdr bord) (cons (+ (car res)1) (+ (cdr res) 1))))))

(define (depth-in? borden res)
  (cond
    ((null? borden) res)
    (else (compare-depth (depth? (car borden) res)
               (depth-in? (cdr borden) res)))))

         

;3
(define deelnemers '((12345 jan)
                     (4566 els)
                     (7536 bart)
                     (7895 ann)))

(define getallen '(5 2))
;3a
(define (maak-ring deelnemers)
  (define (iter dn)
    (cond
      ((null? dn) dn)
      ((null? (cdr dn)) (set-cdr! dn deelnemers))
      (else (iter (cdr dn)))))
  (iter deelnemers)
  deelnemers)

(define deelnemers (maak-ring deelnemers))

(define (trekking! deelnemers getallen)
  (let ((winnaars '())
        (last-winner '()))
    
  (define (get-winnaar curr getal)
    (cond
      ((= getal 2) curr)
      (else (get-winnaar (cdr curr) (- getal 1)))))

    (define (trekking deelnemers getallen)
      (cond
        ((null? getallen)
         (set-cdr! last-winner '())
         winnaars)
        (else
           (let ((prev (get-winnaar deelnemers (car getallen))))
             (begin
               (if (null? winnaars)
                   (begin 
                     (set! last-winner (cdr prev))
                     (set! winnaars (cdr prev)))
                   (begin 
                     (set-cdr! last-winner (cdr prev))
                     (set! last-winner (cdr prev))))
               (set-cdr! prev (cddr prev))
               (trekking (cdr prev) (cdr getallen)))))))
    (trekking deelnemers getallen)))

;4

(define (lamp)
  (let ((toestand 'uit))

    (define (next!)
      (cond
        ((eq? toestand 'uit) (set! toestand 'aan))
        ((eq? toestand 'aan) (set! toestand 'knipperend))
        (else (set! toestand 'uit))))

    (define (get)
      toestand)
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'next) (next!))
        ((eq? msg 'get) (get))))
  dispatch))

(define (slinger n)
  (define (create-slinger n)
      (if (= n 0)
          '()
          (cons (lamp) (create-slinger (- n 1)))))
  
  (let ((lampen (create-slinger n)))

    (define (iter-slinger f lampen)
      (cond
        ((null? lampen))
        (else (f (car lampen)) (iter-slinger f (cdr lampen)))))
    
    (define (next!)
      (iter-slinger (lambda (x) (x 'next)) lampen))

    (define (print)
      (iter-slinger (lambda (x) (display "-") (display (x 'get)) (display "-")) lampen))

    (define (dispatch msg)
      (cond
        ((eq? msg 'next) (next!))
        ((eq? msg 'print) (print))))
    dispatch))






      
