#lang r7rs
; Hoofdstuk 6: Bomen
; Vraag 3: Tellen van deelbomen in een willekeurige boom

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-tree) bt:))

(define (leaf? tree) ; hulpfunctie die bepaalt of een boom een blad is
  (and (bt:null-tree? (bt:left tree))
       (bt:null-tree? (bt:right tree))))

(define (count-nodes tree) ; hulpfunctie die het aantal knopen in een boom telt
  (cond
    ; de lege boom bevat 0 knopen
    ((bt:null-tree? tree) 0)
    ; een boom die niet leeg is, telt 1 knoop meer dan de som van zijn deelbomen
    (else (+ 1 (count-nodes (bt:left tree))
               (count-nodes (bt:right tree))))))

; hoofdfunctie:
; elke knoop onder de wortel van de boom is de wortel van een deelboom.
; we moeten dus gewoon het aantal knopen onder de wortel van de boom tellen.
(define (count-subtrees tree)
  (cond ((bt:null-tree? tree) 0) ; de lege boom bevat 0 subtrees
        ((leaf? tree) 0) ; een boom met maar 1 node bevat 0 subtrees
        (else (+ (count-nodes (bt:left tree))
                 (count-nodes (bt:right tree))))))

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
(display "testboom bevat ") (display (count-nodes testboom)) (display " knopen") (newline)
(display "testboom bevat ") (display (count-subtrees testboom)) (display " deelbomen") (newline)
