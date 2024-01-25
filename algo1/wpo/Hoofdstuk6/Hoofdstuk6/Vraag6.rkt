#lang r7rs

; Hoofdstuk 6: Bomen
; Vraag 6: Tekenen van binaire zoekbomen

(import (scheme base)
        (scheme write)
        (prefix (a-d tree binary-search-tree) bst:))

; bst1:   bst2:   bst3:   bst4:   bst5:
; 
; 1       1       1       1       1
;  \       \       \       \       \
;   2       2       3       4       4
;    \       \     / \     /       /
;     3       4   2   4   2       3
;      \     /             \     /
;       4   3               3   2

(define bst1 (bst:new = <))
(bst:insert! bst1 1)
(bst:insert! bst1 2)
(bst:insert! bst1 3)
(bst:insert! bst1 4)

(define bst2 (bst:new = <))
(bst:insert! bst2 1)
(bst:insert! bst2 2)
(bst:insert! bst2 4)
(bst:insert! bst2 3)

(define bst3 (bst:new = <))
(bst:insert! bst3 1)
(bst:insert! bst3 3)
(bst:insert! bst3 2)
(bst:insert! bst3 4) ; (volgorde 1, 3, 4, 2 zou hetzelfde resultaat geven)

(define bst4 (bst:new = <))
(bst:insert! bst4 1)
(bst:insert! bst4 4)
(bst:insert! bst4 2)
(bst:insert! bst4 3)

(define bst5 (bst:new = <))
(bst:insert! bst5 1)
(bst:insert! bst5 4)
(bst:insert! bst5 3)
(bst:insert! bst5 2)

; bst6:   bst7:
; 
;   2       2
;  / \     / \
; 1   3   1   4
;      \     /
;       4   3

(define bst6 (bst:new = <))
(bst:insert! bst6 2)
(bst:insert! bst6 1)
(bst:insert! bst6 3)
(bst:insert! bst6 4) ; (volgorde 2, 3, 1, 4 zou hetzelfde resultaat geven,
                     ;  net als volgorde 2, 3, 4, 1)

(define bst7 (bst:new = <))
(bst:insert! bst7 2)
(bst:insert! bst7 1)
(bst:insert! bst7 4)
(bst:insert! bst7 3) ; (volgorde 2, 4, 1, 3 zou hetzelfde resultaat geven,
                     ;  net als volgorde 2, 4, 3, 1)

; bst8:   bst9:
; 
;   3       3
;  / \     / \
; 1   4   2   4
;  \     /
;   2   1

; bst10:  bst11:  bst12:  bst13:  bst14:
; 
;   4       4       4       4       4
;  /       /       /       /       /
; 1       1       2       3       3
;  \       \     / \     /       /
;   2       3   1   3   2       1
;    \     /           /         \
;     3   2           1           2
