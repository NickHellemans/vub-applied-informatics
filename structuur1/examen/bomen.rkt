(define (atom? x)
  (not (pair? x)))

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
(define (baas tree)
  (car tree))
(define (slaves tree)
  (cdr tree))

(define (find p tree)
  (cond
    ((eq? p (baas tree)) #t)
    (else (display "Baas: ") (display (baas tree))(newline)(find-in p (cdr tree)))))

(define (find-in p tree)
  (cond
    ((null? tree) #f)
    (else (or (find p (car tree))
              (find-in p (cdr tree))))))

(define (hierarchisch? p1 p2 tree)
  (define (hierarchisch tree)
    (cond
      ((eq? (baas tree) p1) (display (cddr tree))(find p2 tree))
      ((eq? (baas tree) p2) (display "Zoek ") (display p2) (display " in ") (display (slaves tree))(find p1 tree))
      (else (hierarchisch-in (slaves tree)))))
  
  (define (hierarchisch-in tree)
    (cond
      ((null? tree) #f)
      (else (or (hierarchisch (car tree))
                (hierarchisch-in (cdr tree))))))
  (hierarchisch tree))


(define (deep-flatten tree)
  (cond
    ((null? tree) '())
    ((atom? tree) (list tree))
    (else (append (deep-flatten (car tree))
                (deep-flatten (cdr tree))))))

(define (collegas p tree)
  (define (collegas? tree path)
    (cond
      ((eq? p (baas tree)) (append path (deep-flatten (slaves tree))))
      (else (collegas-in (slaves tree) (cons (baas tree) path)))))
  (define (collegas-in tree path)
    (cond
      ((null? tree) #f)
      (else (or (collegas? (car tree) path)
                (collegas-in (cdr tree) path)))))
  (collegas? tree '()))


(define familieboom '(jan (piet (frans (tom)
                                       (roel))
                                (mie))
                          (bram (inge (bert (ina)
                                            (ilse))
                                      (bart))
                                (iris))
                          (joost (els (ilse)))))

(define (familiehoofd fam) (car fam))
(define (kinderen fam) (cdr fam))
(define (laatste? fam)
  (null? (kinderen fam)))

(define (bereken-budget tree lst)
  (define (bereken tree lst)
    (cond
      ((null? lst) 0)
      (else (+ (car lst) (bereken-in (cdr tree) (cdr lst))))))
  (define (bereken-in tree lst)
    (cond
      ((null? tree) 0)
      (else (+ (bereken (car tree) lst)
               (bereken-in (cdr tree) lst)))))
  (bereken-in (kinderen tree) lst))

(define (verdeel-laatste tree budget)
  (define (verdeel tree budget)
    (cond
      ((laatste? tree) (list (cons (familiehoofd tree) budget)))
      (else (verdeel-in (kinderen tree) (/ budget (length (kinderen tree)))))))
  
  (define (verdeel-in tree budget)
    (cond
      ((null? tree) '())
      (else (append (verdeel (car tree) budget)
                    (verdeel-in (cdr tree) budget)))))
  (verdeel tree budget))