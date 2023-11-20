(define (maak-laadstation)
  (let ((attached? #f)
        (electricity 0))

    (define (withdraw! n)
      (set! electricity (+ electricity n)))

    (define (koppel! car)
      (if (not attached?) (set! attached? car)))

    (define (ontkoppel!)
      (set! attached #f))
    
    (define (vrij?)
      (not attached))
    
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
            ((station 'withdraw!) (* capaciteit (/ (- 100 percent) 100)))
            (set! percent 100))))

    (define (koppel! load-station)
     (if (and (not station) (load-station 'vrij?))
         (begin 
           (set! station load-station)
           ((station 'koppel!) dispatch))))
    
    (define (ontkoppel!)
      (if station
          (begin
            ((station 'ontkoppel))
            (set! station #f))))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'charge) (charge))
        ((eq? msg 'charge!) charge!)
        ((eq? msg 'koppel!) koppel!)
        ((eq? msg 'ontkoppel!) ontkoppel!)))

    dispatch))

(define (maak-laadpark n)
  (define (create-stations x)
    (if (= x 0)
        '()
        (cons (maak-laadstation) (create-stations (- x 1)))))
  (let ((free-stations n)
        (stations (create-stations n)))
    
    (define (full?) ((= free-station 0)))

    (define (enter!)
      (cond
        ((full?) #f)))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'full?) (full?))
        ((eq? msg 'enter!) charge!)))

    dispatch))