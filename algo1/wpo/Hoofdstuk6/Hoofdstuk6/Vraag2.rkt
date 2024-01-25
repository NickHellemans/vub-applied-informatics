#lang r7rs
; Hoofdstuk 6: Bomen
; Vraag 2: Hoogte berekenen van een willekeurige boom

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-tree) bt:))


;; Deze procedure is wel een elegante manier om de *diepte* van een boom te berekenen
;; Dit concept is verschillend van de hoogte (verschil van 1)
(define (depth tree)
  (cond
    ; de lege boom heeft diepte 0
    ((bt:null-tree? tree) 0)
    ; een boom die niet leeg is, heeft een diepte die 1
    ; groter is dan de diepte van de diepste deelboom
    (else (+ 1 (max (depth (bt:left tree))
                    (depth (bt:right tree)))))))

(define (depth2 tree)
  (cond
    ((bt:null-tree? tree) 0)
    (else (max (+ 1 (depth (bt:left tree)))
                    (depth (bt:right tree))))))

(define (height tree)
  (- (depth tree) 1))

; testboom: 1
;          / \
;         2   6
;        / \
;       3   4
;            \
;             5
(define testboom
  (bt:new 1
          (bt:new 2
                  (bt:new 3
                          bt:null-tree
                          bt:null-tree)
                  (bt:new 4
                          bt:null-tree
                          (bt:new 5
                                  bt:null-tree
                                  bt:null-tree)))
          (bt:new 6
                  bt:null-tree
                  bt:null-tree)))
(display "testboom heeft diepte ") (display (depth testboom)) (newline)
(display "testboom heeft diepte2 ") (display (depth2 testboom)) (newline)
(display "testboom heeft hoogte ") (display (height testboom)) (newline)
