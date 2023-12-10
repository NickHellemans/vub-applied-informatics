#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme inexact) ; for logarithm
        (prefix (Hoofdstuk4 dequeue-list) deq:)
        (prefix (a-d heap standard) heap:))

; Lineaire ADT's
; --------------

; Oefening 6.
; Cf. dequeue-circular-vector.rkt en dequeue-list.rkt
; MERK OP: bij het dequeue ADT dat positionele lijsten gebruikt, is de head de laatste positie en de rear de eerste positie
; in de onderliggende positionele lijst!
; Hierdoor lijkt de queue in omgekeerde volgorde te zitten, maar de respectievelijke enqueue! en serve! procedures halen
; de elementen correct uit de dequeue 
(define deq (deq:new))
(deq:enqueue! deq 1) (display "    ") (deq:show deq) (newline)
(deq:enqueue! deq 2) (display "    ") (deq:show deq) (newline)
(deq:enqueue-at-head! deq 3) (display "    ") (deq:show deq) (newline)
(deq:enqueue-at-head! deq 4) (display "    ") (deq:show deq) (newline)
(deq:enqueue! deq 5) (display "    ") (deq:show deq) (newline)
(deq:enqueue-at-head! deq 6) (display "    ") (deq:show deq) (newline)      ; resultaat  inhoud dequeue (plist)
(display (deq:serve! deq)) (display "   ") (deq:show deq) (newline)         ; 6          5 2 1 3 4
(display (deq:serve! deq)) (display "   ") (deq:show deq) (newline)         ; 4          5 2 1 3
(display (deq:serve-at-rear! deq)) (display "   ") (deq:show deq) (newline) ; 5          2 1 3
(display (deq:serve-at-rear! deq)) (display "   ") (deq:show deq) (newline) ; 2          1 3
(display (deq:peek deq)) (display "   ") (deq:show deq) (newline)           ; 3          1 3
(display (deq:peek-at-rear deq)) (display "   ") (deq:show deq) (newline)   ; 1          1 3

; Oefening 8.
; Consider the heap created using the expression (from-scheme-vector (vector 25 2 17 20 84 5 7 12) <).
; -> De vector in het voorbeeld kan als volledige boom voorgesteld worden:
;
;            25
;          /    \
;         2      17
;        /\      /\
;      20  84   5   7
;      /
;    12
;
; Deze vector voldoet niet aan de heap-voorwaarde, namelijk dat elk element op positie i kleiner is dan de elementen op posities 2*i en 2*i+1.
; Daarom zal from-scheme-vector de vector "heapificeren", d.w.z. stap voor stap de heap doorlopen tot de heap-voorwaarde voor alle elementen geldt.
; from-scheme-vector begint onderaan de heap (omdat daar de heap-voorwaarde het makkelijkst te herstellen is),
; en werkt meerbepaald van positie length/2 (het laatste element dat een kind heeft) naar 1 (de root van de heap) toe.
; In elke stap zal sift-down op het huidige element worden opgeroepen.
; sift-down werkt van het hudige element naar beneden toe, en verwisselt op elk niveau van de heap het huidige element met zijn kleinste kind.
; Het resultaat van (from-scheme-vector (vector 25 2 17 20 84 5 7 12) <) is dus:
;
;             2
;          /    \
;        12      5
;        /\      /\
;      20  84  17   7
;      /
;    25
;
;    Of in vector-representatie: #(2 12 5 20 84 17 7 25)

(define heap (heap:from-scheme-vector (vector 25 2 17 20 84 5 7 12) <))

; a) What is the parent of the element sitting at index 3?
; -> Het element op positie 1, want dat is de parent van de elementen op posities 2*1 en 2*1+1.

; b) Which element in the heap does not have a parent?
; -> Alleen de root van de heap (het element op positie 1) heeft geen parent.

; c) Which element in the heap does not have a left child?
; -> De elementen op positie i > length/2.
;    Which element only has a left child?
; -> Het element op positie 4.

; d) What is the height of the heap? Implement an operation height that returns the height of any heap. Give the procedural type of the operation.
(define (height heap)
  (floor (log (heap:length heap) 2)))
(display (height heap)) (newline)
; -> Het procedurele type van height is (heap -> number).

; e) Is the following statement true or false? “The value sitting at the root of a subheap of a heap is always the smallest element of all values contained by that subheap.”
; -> true

; Oefening 9.
; What can you say about the location of the greatest element of a heap?
; -> Het grootste element bevindt zich op een positie i > length/2 (in de boomvoorstelling is dit een blad van de boom).

; Oefening 10.
; Assume you have an empty heap with comparator <. Using insert!, we add the elements 5, 2, 3, 1, 2, 1 in that order. Draw every phase of the heap during the construction. Now remove two elements from the heap and redraw the result. In all phases of the exercise, draw the heap as a complete binary tree and draw the underlying vector as well.
; -> insert! voegt steeds het element op de eerste vrije plaats in de heap toe, en voert vervolgens sift-up uit.  sift-up werkt van onderaan naar de root van de heap toe, en gaat telkens het huidige element verwisselen met zijn ouder als de ouder groter is dan het huidige element.

(define heap2 (heap:new 6 <))
; -> (Dit geeft een lege heap terug.)

(heap:insert! heap2 5)
; -> 5

(heap:insert! heap2 2)
; ->   2
;     /
;    5

(heap:insert! heap2 3)
; ->   2
;     / \
;    5   3

(heap:insert! heap2 1)
; ->     1
;       / \
;      2   3
;     /
;    5

(heap:insert! heap2 2)
; ->     1
;       / \
;      2   3
;     / \
;    5   2

(heap:insert! heap2 1)
; ->      1
;       /   \
;      2     1
;     / \   /
;    5   2 3

(heap:delete! heap2)
; ->      1
;       /   \
;      2     3
;     / \   
;    5   2 

(heap:delete! heap2)
; ->      2
;       /   \
;      2     3
;     /    
;    5    