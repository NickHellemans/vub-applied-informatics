;1

(define transacties '(("WJ8" BE -100)
                      ("Q3D" NL -200)
                      ("YWB" US -3.1)
                      ("JQB" GB -142.2)
                      ("WZL" RU 7.3)
                      ("8NB" CH 1500)
                      ("TAB" RU 2070)))

(define (rekeningnr transactie)
  (car transactie))

(define (land transactie)
  (cadr transactie))

(define (hoeveelheid transactie)
  (caddr transactie))

;1a 
(define (bereken begin transacties)
  (cond
    ((null? transacties) '())
    (else
     (let ((nieuw-saldo (+ begin (hoeveelheid (car transacties)))))
       (cons nieuw-saldo (bereken nieuw-saldo (cdr transacties)))))))

;1b
(define (bereken-iter begin transacties)
  (define (iter begin transacties res)
    (cond
      ((null? transacties) res)
      (else
       (let ((nieuw-saldo (+ begin (hoeveelheid (car transacties)))))
         (iter nieuw-saldo (cdr transacties) (append res (list nieuw-saldo)))))))
  (iter begin transacties '()))

;1c

(define (map-met-staat bewerking begin lijst)
  (cond
    ((null? lijst) '())
    (else
     (let ((nieuw-begin (bewerking begin (car lijst))))
       (cons nieuw-begin (map-met-staat bewerking nieuw-begin (cdr lijst)))))))

;1d
(map-met-staat
 (lambda (staat el)
   (+ staat (hoeveelheid el)))
 700
 transacties)

;2
(define versiering '(((ster . 20) (bal . 10))
                     (((bal . 10) (bal . 10)) (ster . 20))
                     ((ster . 20) ((bal . 10) (ster . 20) (bal . 10)))))

(define (decor? el) (and (not (list? el)) (pair? el)))
(define weight cdr)

;2a
(define (count v)
  (cond ((null? v) 0)
        ((decor? v) (weight v))
        (else (+ (count (car v)) (count (cdr v))))))

(define (vorm-om v)
  (cond ((null? v) '())
        ((decor? v) (list v))
        ( else (cons (count v) (map vorm-om v)) )))

(define (vorm-om2 v)
  (cond
    ((null? v) '())
    ((decor? v) (count v))
    (else (+ (vorm-om2 (car v)) (vorm-om2 (cdr v))))))

versiering
(vorm-om versiering)
(vorm-om2 versiering)

;2b


;3

(define (set-hoeveelheid! transactie n)
  (set-car! (cddr transactie) (+ (hoeveelheid transactie) n)))

(define (change-transactions! transacties)
  (define (iter prev curr)
    (cond
      ((null? curr) (display "done"))
      ((eq? 'RU (land (car curr)))
       (begin
         (display "Changing:")
         (set-hoeveelheid! (car prev) (hoeveelheid (car curr)))
         (set-cdr! prev (cdr curr))
         (iter prev (cdr curr))))
      (else (iter curr (cdr curr)))))
  (iter '() transacties))
         

;4
;a
(define (maak-auto cap)
  (let ((capaciteit cap)
        (batterij 10)
        (station #f))

    (define (charge)
      batterij)

    (define (charge!)
      (if station
          (set! batterij 100)))

   (define (koppel! laadstation)
     (set! station laadstation))

    (define (ontkoppel!)
      (set! station #f))

    (define (dispatch msg)
      (cond
        ((eq? msg 'charge) (charge))
        ((eq? msg 'charge!) (charge!))
        ((eq? msg 'koppel!) koppel!)
        ((eq? msg 'ontkoppel!) (ontkoppel!))))

    dispatch))

;b
(define (maak-laadstation)
  (let ((elektriciteit 0)
        (auto #f))

    (define (withdraw! n)
      (set! elektriciteit (+ elektriciteit n)))

    (define (koppel! koppel-auto)
      (set! auto koppel-auto)
      ((auto 'koppel) dispatch))


    (define (ontkoppel!)
      (if auto
          (begin
            (auto 'ontkoppel!)
            (set! auto #f))))

    (define (vrij?)
      (if auto #f #t))

    (define (dispatch msg)
      (cond
        ((eq? msg 'withdraw) withdraw)
        ((eq? msg 'koppel!) koppel!)
        ((eq? msg 'ontkoppel) (ontkoppel!))
        ((eq? msg 'vrij?) (vrij?))))

    dispatch))

;c

(define (maak-laadpark n)

  (define (maak-stations n)
    (cond
      ((= 0 n) '())
      (else (cons (maak-laadstation) (maak-stations (- n 1))))))

  (let ((stations (maak-stations n))
        (capaciteit n)
        (count 0))

    (define (full?)
      (= count capaciteit))

    (define (get-next)
      (define (iter stations)
        (cond
          ((null? stations) #f)
          (((car stations) 'vrij?) (car stations))
          (else (iter (cdr stations)))))
      (iter stations))

    (define (enter! auto)
      (cond
        ((full?) #f)
        (else
         (begin
           (let ((free-station (get-next)))
             (set! count (+ count 1))
             (free-station 'koppel!) auto)))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'full?) (full?))
        ((eq? msg 'enter!) enter!)))
    
    dispatch))

;d

(define laadpark (maak-laadpark 2))
(define auto1 (maak-auto 50))
(define auto2 (maak-auto 50))
(define auto3 (maak-auto 50))

((laadpark 'enter!) auto1)
((laadpark 'enter!) auto2)
((laadpark 'enter!) auto3)

;5
(load "streams.rkt")
(define (raam data n)
  (define (split head-data data i)
    (if (or (empty-stream? data) (>= i n))
        (cons head-data data)
        (split (append-streams head-data
                               (cons-stream (head data)
                                            the-empty-stream))
               (tail data)
               (+ i 1))))
 
  (if (empty-stream? data)
      the-empty-stream
      (let ((temp (split the-empty-stream data 0)))
        (cons-stream (car temp) (raam (tail data) n)))))










  