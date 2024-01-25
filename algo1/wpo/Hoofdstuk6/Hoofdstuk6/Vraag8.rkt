#lang r7rs
; Hoofdstuk 6: Bomen
; Vraag 8: Binaire zoekboom met current

; Zie binary-search-tree-with-current.rkt of binary-search-tree-with-current-stack.rkt

(import (scheme base)
        (scheme write)
        (prefix (Hoofdstuk6 binary-search-tree-with-current-stack) bst:))

(define mybst (bst:new = <))
(bst:insert! mybst 3)
(bst:insert! mybst 7)
(bst:insert! mybst 1)
(bst:insert! mybst 2)
(bst:insert! mybst 6)
(bst:insert! mybst 9)
(bst:insert! mybst 4)

(define (foreach bst fun) ; dankzij onze current kunnen we een foreach schrijven zonder dat we de interne representatie van bomen hoeven te kennen
  (bst:set-current-to-first! bst) ; zet de current op de eerste positie
  (let loop () ; we itereren over elke positie
    (if (bst:has-current? bst) ; we doen enkel iets indien we nog een (geldige) current hebben
        (begin (fun (bst:get-current bst)) ; voer fun uit op de current
               (bst:set-current-to-next! bst) ; ga een positie verder
               (loop))))) ; (recursieve oproep)

(foreach mybst (lambda (elt)
                 (display "we bekijken momenteel element ")
                 (display elt)
                 (newline)))
