#lang r7rs

;  Vraag 11: Gebruik radix sort voor het sorteren van een lijst van strings.
; ---------------------------------------------------------------------------

; -> Standaard werkt radix sort enkel voor sleutels die decimale getallen zijn.
; -> Als we elk van de sleutels van de lijst kunnen OMZETTEN naar een decimaal
;    getal, kunnen we radix sort dus wel gebruiken!
; -> De truuk is dus om elke string naar een uniek getal om te zetten.
; -> Daarvoor kunnen we een string beschouwen als een 26-delig getal:
;    "a" wordt omgezet naar 1, want 1 * 26^0 = 1
;    "b" wordt omgezet naar 2, want 2 * 26^0 = 2
;    ...  
;    "z" wordt omgezet naar 26, want 26 * 26^0 = 26
;    "aa" wordt omgezet naar 27, want 1 * 26^1 + 1 * 26^0 = 27
;    "ab" wordt omgezet naar 28, want 1 * 26^1 + 2 * 26^0 = 28
;    ...
;    "ba" wordt omgezet naar 78, want 2 * 26^1 + 1 * 26^0 = 78
; -> In deze aanpak zitten we nog met een klein probleem, namelijk dat kortere
;    strings altijd voor langere strings zouden komen te staan, bv. ("b" "ab"
;    "ba").  Bij de kortere strings moeten we dus eigenlijk rekening houden met
;    de lengte van de langst mogelijke string, want die bepaalt de significantie
;    van elk karakter van alle strings.
; -> Stel dat de langst mogelijke string 2 karakters lang is, dan zouden we de
;    voorgaande strings dus als volgt omzetten:
;    "a" wordt omgezet naar 26, want 1 * 26^1 = 26
;    "b" wordt omgezet naar 52, want 2 * 26^1 = 52
;    "z" wordt omgezet naar 676, want 26 * 26^1 = 676
;    "aa" wordt omgezet naar 27, want 1 * 26^1 + 1 * 26^0 = 27
;    "ab" wordt omgezet naar 28, want 1 * 26^1 + 2 * 26^0 = 28
;    "ba" wordt omgezet naar 78, want 2 * 26^1 + 1 * 26^0 = 78
; -> Nu wordt er dus wel correct gesorteerd.

(import (scheme base)
        (scheme write)
        (scheme inexact) ; for logarithm
        (a-d sorting internal non-comparative radix-sort))

; Deze hulpfunctie zet een karakter van #\a tot #\z om naar het overeenkomstige getal van 1 tot 26:
(define (char-to-number c)
  (let ((ascii (char->integer c)))
    (if (or (< ascii 97) (> ascii 122))
        (error "c is not a valid character" c)
        (- ascii 96))))

; Deze hulpfunctie zet een string s om naar het overeenkomstige decimaal getal
(define (string-to-number s k) ; k is de lengte van de langst mogelijke string
  (define (iter positie resultaat) ; itereer over alle karakters van de string, van links naar rechts
    (if (< positie (string-length s))
        (iter (+ positie 1)
              (+ resultaat ; voeg telkens iets toe aan het huidige resultaat
                 (* (char-to-number (string-ref s positie)) ; nl. de vermenigvuldiging van de getalwaarde van het huidige karakter
                    (expt 26 (- k 1 positie)))))            ; ... en de juiste 26-ste macht
        resultaat))
  (iter 0 0))

(define (radix-sort-for-string-keys slst key)
  ; 1. itereer over slst om te bepalen hoe lang de langste string is -> k
  (let ((k 0))
    (for-each (lambda (el) (set! k (max k (string-length (key el))))) slst)
    ; 2. bereken de grootst mogelijke key
    (let ((largestkey (expt 26 k)))
      ; 3. bereken hoeveel digits de grootst mogelijke key bevat -> d
      (let ((d (ceiling (log largestkey 10)))) ; de 10-de logaritme van largestkey, afgerond naar boven
        ; 4. roep tenslotte radix-sort op:
        (radix-sort slst
                    (lambda (el) (string-to-number (key el) k)) ; we geven een nieuwe key-functie mee aan radix-sort, die voor elke sleutel de overeenkomstige getalwaarde zal berekenen
                    d)))))

; test
(define l1 (list "hello" "world" "and" "goodday" "to" "all" "the" "rest" "of" "you"))
(display (radix-sort-for-string-keys l1 (lambda (el) el))) (newline)
(define l2 (list (cons "jan" "janssens")
                 (cons "marc" "janssens")
                 (cons "herman" "janssens")
                 (cons "jan" "leemans")
                 (cons "andre" "leemans")
                 (cons "edgar" "janssens")))
(display (radix-sort-for-string-keys l2 (lambda (el) (car el)))) (newline)
(display (radix-sort-for-string-keys l2 (lambda (el) (cdr el)))) (newline) ; hier merk je dat radix-sort stabiel is.  dat is overigens essentieel voor de werking van het algoritme!  (denk zelf eens na waarom...)
