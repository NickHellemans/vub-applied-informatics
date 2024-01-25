#lang r7rs
(import (scheme base)
        (scheme write)
        (scheme cxr) ; zie oef. 1-c
        (prefix (a-d positional-list adt) plist:)
        (prefix (a-d pattern-matching quicksearch) pmatch:))

; Lineaire datastructuren
; -----------------------

; Oefening 1.
; a)
(define exercise-a (list 'exercise-a
                         3
                         eq?
                         (vector 5 6 3 0 0 0 0 0 0 0 0 0)))
(display "exercise a: ")
(display exercise-a)
(newline)
; -> 'exercise-a is een tag (cf. de complex, fraction en disk ADT's uit hoofdstuk 1)
; -> 3 is het huidige aantal elementen (en dus ook de eerste vrije positie)
; -> eq? is de gelijkheidsfunctie (die bijvoorbeeld door find zal gebruikt worden
;    om na te gaan of een bepaald element gelijk is aan het gezochte element)
; -> de vector is de onderliggende datastructuur


; b)
(define exercise-b (list 'exercise-b
                         4
                         (vector 6 -2 9 -1 -7 0 10 -5 1)))
(display "exercise b: ")
(display exercise-b)
(newline)
; -> 'exercise-b is een tag
; -> 4 is wellicht het aantal negatieve getallen in de vector
; -> de vector is de onderliggende datastructuur

; c)
(define underlying-list (list -5 -8 -1 6 2 0 7))
(define exercise-c (list 'exercise-c
                         (cddr (cddddr underlying-list)) ; cddddr zit in de cxr library
                         3
                         underlying-list))
(display "exercise c: ")
(display exercise-c)
(newline)
; -> 'exercise-c is een tag
; -> (cddr (cddddr underlying-list) is de cons-cel die het laatste element van de enhanced list bevat
; -> 3 is het huidige aantal negatieve elementen in de lijst
; -> underlying-list is de cons-cel die het eerste element van de lijst bevat

; Alternatieve manier
(define last-element (cons 7 '()))
(define underlying-list2 (append (list -5 -8 -1 6 2 0)
                                 last-element))

(define exercise-c2 (list 'exercise-c2
                          last-element
                          3
                          underlying-list2))
(display "exercise c (alternative): ")
(display exercise-c2)
(newline)


; Oefening 2.
; Hulpfunctie: Toont de inhoud van een positionele lijst
(define (display-plist p)
  (display "positional list elements are: ")
  (plist:for-each p (lambda (element) (display element) (display ", ")))
  (newline))

; a)
(define l (plist:new eq?))
(display-plist l)
(plist:add-before! l "and")
(display-plist l)
(plist:add-after! l "me")
(display-plist l)
(plist:add-before! l "to" (plist:last l))
(display-plist l)
(plist:add-after! l "goodday" (plist:first l))
(display-plist l)
(plist:add-before! l "hello")
(display-plist l)
(plist:add-after! l "world" (plist:first l))
(display-plist l)

; b)
(define (count-words-containing-e l)
  (define result 0)
  (plist:for-each l (lambda (element) 
                      (if (pmatch:match element "e") 
                          (set! result (+ result 1)))))
  result)
(display "(count-words-containing-e l)    -> ") (display (count-words-containing-e l)) (newline)

; Oefening 3.
(define (pair-eq? p1 p2)
  (eq? (cdr p1) (cdr p2)))
  
(define m (plist:map l
                     (lambda (element)
                       (cons element (string-length element)))
                     pair-eq?))
(display-plist m)
(display "(plist:find m (cons \"\" 7))      -> ")(display (plist:find m (cons "" 7))) (newline)

; Oefening 4.
; In de map a-d/positional-list zit het bestand `single-linked`.
; Daar dien je de implementatie van de procedures make-list-node etc. aan te passen zodat 
; een node wordt voorgesteld door een vector van lengte 2.


(define (make-list-node x y) (vector x y))
(define list-node-val (lambda (vector-node) (vector-ref vector-node 0)))
(define list-node-val! (lambda (vector-node value) (vector-set! vector-node 0 value)))
(define list-node-next (lambda (vector-node) (vector-ref vector-node 1)))
(define list-node-next! (lambda (vector-node value) (vector-set! vector-node 1 value)))


; Oefening 5.
(define (accumulate p null combiner)
  (if (plist:empty? p)
      null
      (let loop
        ((current-pos (plist:first p))
         (current-res null))
        (let ((new-res (combiner current-res (plist:peek p current-pos))))
          (if (plist:has-next? p current-pos)
              (loop (plist:next p current-pos) new-res)
              new-res)))))

(define (accumulate-my p null combiner)
  (cond
    ((plist:empty? p) null)
    (else
     (let loop
       ((curr (plist:first p))
        (res null))
       (let ((new-res (combiner res (plist:peek p curr))))
         (if (plist:has-next? p curr)
             (loop (plist:next p curr) new-res)
             new-res))))))

(define n (plist:from-scheme-list '(1 2 3 4 5) =))
(display "(accumulate n 0 +)              -> ") (display (accumulate-my n 0 +)) (newline)
(define o (plist:from-scheme-list '("a" "b" "c") eq?))
(display "(accumulate o \"\" string-append) -> ") (display (accumulate-my o "" string-append)) (newline)

; Oefening 6.
(define (intersection p1 p2)
  (define result (plist:new eq?))
  (if (or (plist:empty? p1)
          (plist:empty? p2))
      result
      (let loop
        ((current-pos (plist:first p1)))
        (let ((current-val (plist:peek p1 current-pos)))
          (if (plist:find p2 current-val)
              (plist:add-after! result current-val))
          (if (plist:has-next? p1 current-pos)
              (loop (plist:next p1 current-pos))
              result)))))

(define (my-intersection p1 p2)
  (define res (plist:new eq?))
  (cond
    ((or (plist:empty? p1) (plist:empty? p2)) res)
    (else
     (let loop
       ((curr (plist:first p1)))
       (let ((curr-val (plist:peek p1 curr)))
         (cond
           ((plist:find p2 curr-val) (plist:add-after! res curr-val)))
         (if (plist:has-next? p1 curr)
             (loop (plist:next p1 curr))
             res))))))
(define p (plist:from-scheme-list '(2 4 6 8 10 12 14) eq?))
(define q (plist:from-scheme-list '(3 6 9 12 15 18 21) eq?))
(display "(intersection p q) -> ")(display-plist (my-intersection p q))

; Oefening 8.
; Voor deze oefening moet je eigenlijk de (a-d sorted-list vectorial) library 
; aanpassen (zie vectorial-ternarysearch.rkt).  
; De volgende functie toont de algemene werking van ternary search voor gesorteerde (naakte) vectoren.
(define (find sorted-vector key)
  (define length (vector-length sorted-vector))
  (let ternary-search
    ((left 0)
     (right (- length 1)))
    (if (<= left right)
        (let ((point-a (+ left (quotient (- right left) 3)))
              (point-b (- right (quotient (- right left) 3))))
          (cond
            ((= (vector-ref sorted-vector point-a) key) point-a)
            ((= (vector-ref sorted-vector point-b) key) point-b)
            ((< (vector-ref sorted-vector point-b) key) (ternary-search (+ point-b 1) right))
            ((< (vector-ref sorted-vector point-a) key) (ternary-search (+ point-a 1) (- point-b 1)))
            (else (ternary-search left (- point-a 1)))))
        -1)))
(display "(find (vector 1 2 3 4 5 6 7 8 9 10) 8)  -> ") (display (find (vector 1 2 3 4 5 6 7 8 9 10) 8)) (newline)
(display "(find (vector 1 2 3 4 5 6 7 8 9 10) 12) -> ") (display (find (vector 1 2 3 4 5 6 7 8 9 10) 12)) (newline)
(display "(find (vector 1 2 3 4 5 6 7 8 9 10) 3)  -> ") (display (find (vector 1 2 3 4 5 6 7 8 9 10) 3)) (newline)

; Oefening 10
; Zie ring-improved.rkt
