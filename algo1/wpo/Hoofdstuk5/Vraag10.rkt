#lang r7rs
(import (scheme base)
        (scheme write)
        (a-d sorting internal comparative heapsort)    ; importeer heapsort   (niet stabiel)
;        (a-d sorting internal comparative bubble-sort) ;        OF bubblesort (stabiel)
        )
(define v (vector (cons "jan" "janssens")
                  (cons "oscar" "leemans")
                  (cons "marc" "janssens")
                  (cons "herman" "janssens")
                  (cons "jan" "leemans")
                  (cons "elza" "janssens")
                  (cons "edmond" "leemans")))
(display "input vector: ") (display v) (newline)
(sort v (lambda (el1 el2) (string<? (car el1) (car el2))))
(display "gesorteerd op voornaam: ") (display v) (newline) (newline)
; merk op dat jan leemans nu ineens voor jan janssens staat, terwijl ze dezelfde
; key hebben en jan janssens in de oorspronkelijke vector voor jan leemans stond
; -> het algoritme is dus niet stabiel!

; dit wil dus zeggen dat een eventuele vorige sortering op een andere key "stuk"
; wordt gemaakt!  als we de volgende code zouden uitvoeren met een stabiel
; sorteeralgoritme, dan zou de vector eerst op familienaam, en dan op voornaam
; gesorteerd worden.  omdat heapsort niet stabiel is lukt dat hier echter niet...
(sort v (lambda (el1 el2) (string<? (car el1) (car el2)))) ; sorteer eerst op voornaam
(sort v (lambda (el1 el2) (string<? (cdr el1) (cdr el2)))) ; ... en dan op familienaam
(display "gesorteerd op familienaam en dan op voornaam? ") (display v) (newline)
