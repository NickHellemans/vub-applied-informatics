#lang r7rs
; Hoofdstuk 6: Bomen
; Vraag 11: AVL-boom met check

; Zie avl-tree-with-check.rkt

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-tree) bt:)
        (prefix (a-d tree binary-tree-algorithms) bta:))
;(prefix (Hoofdstuk6 avl-tree-with-check) avl:))




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


; testbst:  10
;          / \
;         5   15
;        / \
;       3   6
;            \
;             8
(define testbst
  (bt:new 10
          (bt:new 5
                  (bt:new 3
                          bt:null-tree
                          bt:null-tree)
                  (bt:new 6
                          bt:null-tree
                          (bt:new 8
                                  bt:null-tree
                                  bt:null-tree)))
          (bt:new 15
                  bt:null-tree
                  bt:null-tree)))

; testbst:  10
;          / \
;         5   15
;        / \
;       3   6
;            \
;             100
(define testbst-fout
  (bt:new 10
          (bt:new 5
                  (bt:new 3
                          bt:null-tree
                          bt:null-tree)
                  (bt:new 6
                          bt:null-tree
                          (bt:new 100
                                  bt:null-tree
                                  bt:null-tree)))
          (bt:new 15
                  bt:null-tree
                  bt:null-tree)))


; testavl:   10
;          /   \
;         5     15
;        / \
;       3   6
;            
(define testavl
  (bt:new 10
          (bt:new 5
                  (bt:new 3
                          bt:null-tree
                          bt:null-tree)
                  (bt:new 6
                          bt:null-tree
                          bt:null-tree))
          (bt:new 15
                  (bt:new 13
                          bt:null-tree
                          bt:null-tree)
                  (bt:new 17
                          bt:null-tree
                          bt:null-tree))))

; testavl:   10
;          /   \
;         5     15
;        / \      \
;       3   6      17
;                   \
;                    18
(define testavl-fout
  (bt:new 10
          (bt:new 5
                  (bt:new 3
                          bt:null-tree
                          bt:null-tree)
                  (bt:new 6
                          bt:null-tree
                          bt:null-tree))
          (bt:new 15
                  bt:null-tree
                  (bt:new 17
                          bt:null-tree
                          (bt:new 18
                                  bt:null-tree
                                  bt:null-tree)))))






(define (leaf? tree)
  (and (bt:null-tree? (bt:left tree))
       (bt:null-tree? (bt:right tree))))

; een eerste naieve versie die correct "testboom" gaan merken als een ongeldige BST,
; alsook "testbst" wordt correct gezien als correcte bst, maar niet "testbst-fout".
(define (bst?-naief tree)
  (cond ((bt:null-tree? tree) #t)
        ((leaf? tree) #t)
        (else
         (let ((curr-val (bt:value tree))
               (left (bt:left tree))
               (right (bt:right tree)))
           (cond ((bt:null-tree? left)
                  (and (< curr-val (bt:value right))
                       (bst?-naief right)))
                 ((bt:null-tree? right)
                  (and (< (bt:value left) curr-val)
                       (bst?-naief left)))
                 (else
                  (and (< (bt:value left) curr-val)
                       (< curr-val (bt:value right))
                       (bst?-naief left)
                       (bst?-naief right))))))))



; procedure geïmplementeerd a.d.h.v. binary tree algorithms
; Dit is een accumulator gelijkaardig aan een accumulate voor lijsten, maar deze gaat
; een binary tree "in-order" traversen en steeds de accumulator updaten.
(define (accumulate-tree tree init-acc proc)
  (define accumulator init-acc)
  (bta:in-order tree (lambda (val) (set! accumulator (proc accumulator val))))
  accumulator)

; Bereken de minimum (dus kleinste) value van een binary tree.
; deze procedure gaat ervan uit dat "tree" niet de null-tree is.
(define (min-value tree)
  (accumulate-tree tree (bt:value tree) min))

; Bereken de maximum (dus grootste) value van een binary tree.
; deze procedure gaat ervan uit dat "tree" niet de null-tree is.
(define (max-value tree)
  (accumulate-tree tree (bt:value tree) max))

; een procedure die het kleinste getal van een binary tree onthoudt.
; Dit toont hoe je de procedure zelf zou kunnen implementeren zonder
; gebruik te maken van de algoritmen om een binary tree te traversen
#;(define (min-value tree)
    (cond ((bt:null-tree? tree) #f)
          ((leaf? tree) (bt:value tree))
          (else
           (min (bt:value tree)
                (cond ((bt:null-tree? (bt:left tree))
                       (min-value (bt:right tree)))
                      ((bt:null-tree? (bt:right tree))
                       (min-value (bt:left tree)))
                      (else
                       (min (min-value (bt:left tree))
                            (min-value (bt:right tree)))))))))


; een procedure die het grootste getal van een binary tree onthoudt.
; Dit toont hoe je de procedure zelf zou kunnen implementeren zonder
; gebruik te maken van de algoritmen om een binary tree te traversen
#;(define (max-value tree)
    (cond ((bt:null-tree? tree) #f)
          ((leaf? tree) (bt:value tree))
          (else
           (max (bt:value tree)
                (cond ((bt:null-tree? (bt:left tree))
                       (max-value (bt:right tree)))
                      ((bt:null-tree? (bt:right tree))
                       (max-value (bt:left tree)))
                      (else
                       (max (max-value (bt:left tree))
                            (max-value (bt:right tree)))))))))


(define (bst? tree)
  (cond ((bt:null-tree? tree) #t)
        ((leaf? tree) #t)
        (else
         (let* ((curr-val (bt:value tree))
                (left (bt:left tree))
                (right (bt:right tree)))
           ; deze AND handelt het geval af waar het rechterkind of linkerkind leeg kunnen zijn.
           ; De procedure test eerst voor de aanwezigheid van hetkind, en indien het kind aanwezig is moet de bst-conditie gelden
           (and
            ; OF:
            ;  - het rechterkind is de null-tree (dat is OK)
            ;  - of de value van de huidige node moet kleiner zijn dan die van het rechterkind,
            ;    en de huidige value moet kleiner zijn dan de minimum value van de hele rechterboom,
            ;    en het rechterkind moet ook een geldige bst zijn
            (or (bt:null-tree? right)
                (and (< curr-val (bt:value right))
                     (< curr-val (min-value right))
                     (bst? right)))
            ; OF:
            ;  - het linkerkind is de null-tree (dat is OK)
            ;  - of de value van het linkerkind moet kleiner zijn dan die van de huidige node,
            ;    en de grootste value van de linkerboom moet kleiner zijn dan die van de huidige node,
            ;    en het linkerkind moet ook een geldige bst zijn
            (or (bt:null-tree? left)
                (and (< (bt:value left) curr-val)
                     (< (max-value left) curr-val)
                     (bst? left))))))))

(define (depth tree)
  (cond
    ((bt:null-tree? tree) 0)
    (else (+ 1 (max (depth (bt:left tree))
                    (depth (bt:right tree)))))))

(define (height tree)
  (- (depth tree) 1))

; deze procedure gaat enkel de AVL-conditie checken
(define (avl? tree)
  (or (bt:null-tree? tree)
      (let ((left-height (height (bt:left tree)))
            (right-height (height (bt:right tree))))
        (and (or (= left-height right-height)
                 (= left-height (+ 1 right-height))
                 (= (+ left-height 1) right-height))
             (avl? (bt:left tree))
             (avl? (bt:right tree))))))

(define (check? tree)
  (and (bst? tree)
       (avl? tree)))


(display "(bst?-naief testboom) => verwacht #f, gegeven: ") (display (bst?-naief testboom)) (newline)
(display "(bst?-naief testbst) => verwacht #t, gegeven: ") (display (bst?-naief testbst)) (newline)
(display "(bst?-naief testbst-fout) => verwacht #f, gegeven: ") (display (bst?-naief testbst-fout)) (newline)
(display "(bst?-naief testavl) => verwacht #t, gegeven: ") (display (bst?-naief testavl)) (newline)
(display "(bst?-naief testavl-fout) => verwacht #t, gegeven: ") (display (bst?-naief testavl-fout)) (newline)
(newline)
(display "(bst? testboom) => verwacht #f, gegeven: ") (display (bst? testboom)) (newline)
(display "(bst? testbst) => verwacht #t, gegeven: ") (display (bst? testbst)) (newline)
(display "(bst? testbst-fout) => verwacht #f, gegeven: ") (display (bst? testbst-fout)) (newline)
(display "(bst? testavl) => verwacht #t, gegeven: ") (display (bst? testavl)) (newline)
(display "(bst? testavl-fout) => verwacht #t, gegeven: ") (display (bst? testavl-fout)) (newline)
(newline)
(display "(avl? testboom) => verwacht #f, gegeven: ") (display (avl? testboom)) (newline)
(display "(avl? testbst) => verwacht #f, gegeven: ") (display (avl? testbst)) (newline)
(display "(avl? testbst-fout) => verwacht #f, gegeven: ") (display (avl? testbst-fout)) (newline)
(display "(avl? testavl) => verwacht #t, gegeven: ") (display (avl? testavl)) (newline)
(display "(avl? testavl-fout) => verwacht #f, gegeven: ") (display (avl? testavl-fout)) (newline)
(newline)
(display "(check? testboom) => verwacht #f, gegeven: ") (display (check? testboom)) (newline)
(display "(check? testbst) => verwacht #f, gegeven: ") (display (check? testbst)) (newline)
(display "(check? testbst-fout) => verwacht #f, gegeven: ") (display (check? testbst-fout)) (newline)
(display "(check? testavl) => verwacht #t, gegeven: ") (display (check? testavl)) (newline)
(display "(check? testavl-fout) => verwacht #f, gegeven: ") (display (check? testavl-fout)) (newline)
