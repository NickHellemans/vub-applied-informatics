(define (maak-laadstation)
  (let ((attached? #f)
        (electricity 0))

    (define (withdraw! n)
      (set! electricity (+ electricity n)))

    (define (koppel! car)
      (if (not attached?) (set! attached? car)))

    (define (ontkoppel!)
      (set! attached? #f))
    
    (define (vrij?)
      (not attached?))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'withdraw!) withdraw!)
        ((eq? msg 'koppel!) koppel!)
        ((eq? msg 'ontkoppel!) ontkoppel!)
        ((eq? msg 'vrij?) (vrij?))
        (else (display "error"))))

    dispatch))

(define (maak-auto capacity)
  (let ((percent 100)
        (station #f))

    (define (charge) percent)

    (define (charge!)
      (if station
          (begin 
            ((station 'withdraw!) (* capacity (/ (- 100 percent) 100)))
            (set! percent 100))))

    (define (koppel! load-station)
     (if (and (not station) (load-station 'vrij?))
         (begin 
           (set! station load-station)
           ((station 'koppel!) dispatch))))
    
    (define (ontkoppel!)
      (if station
          (begin
            ((station 'ontkoppel!))
            (set! station #f))))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'charge) (charge))
        ((eq? msg 'charge!) charge!)
        ((eq? msg 'koppel!) koppel!)
        ((eq? msg 'ontkoppel!) ontkoppel!)))

    dispatch))

(define (maak-laadpark n)
  (define (create-stations n)
    (if (= n 0)
        '()
        (cons (maak-laadstation) (create-stations (- n 1)))))

  (let ((stations (create-stations n)))

    (define (stations-empty? stations)
      (cond
        ((null? stations) #t)
        (((car stations) 'vrij?) #f)
        (else (stations-empty? (cdr stations)))))
    
    (define (koppel-auto stations auto)
      (cond
        ((null? stations) #f)
        (((car stations) 'vrij?) ((auto 'koppel!) (car stations)))
        (else (koppel-auto (cdr stations) auto))))

    (define (full?)
      (stations-empty? stations))

    (define (enter! auto)
      (cond
        ((full?) #f)
        (else (koppel-auto stations auto))))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'full?) (full?))
        ((eq? msg 'enter!) enter!)
        (else (display "error laadpark"))))

    dispatch))



(define laadpark (maak-laadpark 2))
(define auto1 (maak-auto 30))
(define auto2 (maak-auto 50))
(define auto3 (maak-auto 90))