#lang r7rs
; Hoofdstuk 6: Bomen
; Vraag 1: Tellen van bladeren in een willekeurige boom

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-tree) bt:))

(define (leaf? tree) ; hulpfunctie die bepaalt of een boom een blad is
  (and (bt:null-tree? (bt:left tree))
       (bt:null-tree? (bt:right tree))))

(define (count-leaves tree)
  (cond
    ; de lege boom bevat 0 bladeren
    ((bt:null-tree? tree) 0)
    ; een boom zonder kinderen bevat 1 blad (= de wortel van de boom)
    ((leaf? tree) 1)
    ; de wortel van een boom mét kinderen is zelf geen blad,
    ; maar bevat (misschien) wel bladeren in zijn kinderen.
    ; we roepen onszelf dus recursief op op beide kinderen,
    ; en geven dan de som van beide resultaten terug.
    (else (+ (count-leaves (bt:left tree))
             (count-leaves (bt:right tree))))))



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
(display "testboom bevat ") (display (count-leaves testboom)) (display " bladeren") (newline)
