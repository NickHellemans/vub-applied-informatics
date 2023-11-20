(define (make-counter initial)
  (define (increase!)
    (set! initial (+ initial 1)))
 
  (define (decrease!)
    (set! initial (- initial 1)))
 
  (define (dispatch m)
    (cond ((eq? m 'increase!) increase!)
          ((eq? m 'decrease!) decrease!)
          ((eq? m 'read) initial)
          (else (display "wrong message"))))
 
  dispatch)

(define (make-parking capacity1 capacity2)
  (let ((verdiep1 (make-counter capacity1))
        (verdiep2 (make-counter capacity2)))
    
    (define (full?)
      (and (= (verdiep1 'read) 0) (= (verdiep2 'read) 0)))

    (define (empty?)
      (and (= (verdiep1 'read) capacity1) (= (verdiep2 'read) capacity2)))

    (define (level)
      (if (= (verdiep1 'read) 0)
          2
          1))

    (define (car-enters!)
      (cond
        ((full?) #f)
        ((= (verdiep1 'read) 0) ((verdiep2 'decrease!)))
        (else ((verdiep1 'decrease!)))))

    (define (car-leaves!)
      (cond
        ((empty?) #f)
        ((= (verdiep2 'read) capacity2) ((verdiep1 'increase!)))
        (else ((verdiep2 'increase!)))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'full?) (full?))
        ((eq? msg 'empty?) (empty?))
        ((eq? msg 'level) (level))
        ((eq? msg 'car-enters!) car-enters!)
        ((eq? msg 'car-leaves!) car-leaves!)))
    
    dispatch))