#lang r7rs
; Hoofdstuk 6: Bomen
; Vraag 7: Symmetrisch verwijderen in de BST implementatie

; Zie binary-search-tree-symmetrical.rkt

(import (scheme base)
        (scheme write)
        (prefix (Hoofdstuk6 binary-search-tree-symmetrical) bst:))

(define mybst (bst:new = <))
(bst:insert! mybst 5)
(bst:insert! mybst 3)
(bst:insert! mybst 4)
(bst:insert! mybst 7)
(bst:insert! mybst 6)
(bst:insert! mybst 8)
(bst:insert! mybst 9)

(bst:delete-symmetrical! mybst 7)

(display (bst:find mybst 7))
