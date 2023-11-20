#lang racket

(require (prefix-in bub: a-d/sorting/internal/comparative/bubble-sort))
(require (prefix-in ins: a-d/sorting/internal/comparative/insertion-sort))
(require (prefix-in sel: a-d/sorting/internal/comparative/selection-sort))
(require (prefix-in qck: a-d/sorting/internal/comparative/quicksort))
(require srfi/27); random numbers
(require srfi/19); dealing with time
(require plot)   ; plot libraries

; our own special-form/macro "time-taken" to profile an expression
; we get the number of nanoseconds the expression has taken
; this is: seconds * 10^9 + nanoseconds

(define-syntax time-taken
  (syntax-rules (expression)
    [(time-taken exp) (let* ([t1     (current-time)]
                             [val    exp]
                             [t2     (current-time)]
                             [passed (time-difference t2 t1)])
                        (+ (* (time-second passed) (expt 10 9))
                           (time-nanosecond passed)))]))

; generate a random vector of size n (with numbers in [0..1000])

;(define (generate-rnd-vec n)
;  (define vec (make-vector n))
;  (do ((i 0 (+ i 1)))
;    ((= i n) vec)
;    (vector-set! vec i (random-integer 1000))))

(define (generate-rnd-vec n)
  (build-vector n (lambda (i) (random-integer 1000))))

; take a vector of random values and sort it 4 times + calculate the time it took
; (make copies of the vector to make sure we sort the same vector 4 times)

(define (profile vec)
  (define vec2 (make-vector (vector-length vec))) ; copy 1
  (define vec3 (make-vector (vector-length vec))) ; copy 2
  (define vec4 (make-vector (vector-length vec))) ; copy 3
  (vector-copy! vec2 0 vec)
  (vector-copy! vec3 0 vec)
  (vector-copy! vec4 0 vec)
  (list (time-taken (bub:sort vec <))     ; chronometrise bubble sort
        (time-taken (ins:sort vec2 <))    ; chronometrise insertion sort
        (time-taken (sel:sort vec3 <))    ; chronometrise selection sort
        (time-taken (qck:sort vec4 <))))  ; chronometrise quicksort

; convert a list of qudruples into a quadruple of lists

(define (unzip list-of-quadruples)
  (if (null? list-of-quadruples)
      (list '() '() '() '())
      (let ((unzipped-cdr    (unzip (cdr list-of-quadruples)))
            (first-quadruple (car list-of-quadruples)))
        (list (cons (car first-quadruple)
                    (car unzipped-cdr))
              (cons (cadr first-quadruple)
                    (cadr unzipped-cdr))
              (cons (caddr first-quadruple)
                    (caddr unzipped-cdr))
              (cons (cadddr first-quadruple)
                    (cadddr unzipped-cdr))))))

; for all values L between low and high with 'step' size
; generate a random vector of length L (= low + step*idx)
; sort the vector 4 times

(define (experiment low high step)
  (define nr-of-vals (quotient (- high low) step)) ; number of points
  (unzip (build-list
          nr-of-vals
          (λ (idx)
            (profile (generate-rnd-vec (+ low (* idx step))))))))

; DO THE EXPERIMENTS

(define low  1000)
(define high 9000)
(define step 500)

(define x-axis (build-list (quotient (- high low) step)
                           (lambda (idx)
                             (+ low (* idx step)))))
(define experiments (experiment low high step))

(define bubble    (car    experiments))
(define insertion (cadr   experiments))
(define selection (caddr  experiments))
(define quick     (cadddr experiments))

(plot-new-window? #t)
(plot (list (lines (map vector x-axis bubble)
                   #:x-min low
                   #:x-max high
                   #:y-min 0
                   #:color "red")
            (lines (map vector x-axis insertion)
                   #:x-min low
                   #:x-max high
                   #:y-min 0
                   #:color "blue")
            (lines (map vector x-axis selection)
                   #:x-min low
                   #:x-max high
                   #:y-min 0
                   #:color "green")
            (lines (map vector x-axis quick)
                   #:x-min low
                   #:x-max high
                   #:y-min 0
                   #:color "black"))
      #:x-label "Grootte van de vector"
      #:y-label "Sorteertijd (in nanoseconden)")