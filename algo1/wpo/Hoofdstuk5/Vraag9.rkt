#lang r7rs
(import (scheme base)
        (scheme write))

; eerste hulpfunctie: splitst een lijst in twee helften
(define (split lst)
    (define l (length lst))
    ; iteratief proces dat eerste l/2 elementen van lst in een nieuwe lijst (= linkerhelft) steekt,
    ; en die nieuwe lijst vervolgens samen met de rest (= rechterhelft) teruggeeft
    (define (iter teller linkerhelft rechterhelft)
      (if (< teller (/ l 2))
          (iter (+ teller 1)
                (cons (car rechterhelft) linkerhelft) ; omdat we vooraan cons'en wordt de linkerhelft omgedraaid maar dit is geen probleem!
                (cdr rechterhelft))
          (cons linkerhelft rechterhelft)))
    (iter 0 '() lst))

; test
(define l '(4 6 2 8 5 3 7 1 9))
(display "Volledige lijst: ") (display l) (newline)
(display "Linker helft: ") (display (car (split l))) (newline) ; merk op dat de linkerhelft in omgekeerde volgorde staat
(display "Rechter helft: ") (display (cdr (split l))) (newline) (newline)

; tweede hulpfunctie: voegt twee gesorteerde lijsten samen tot één gesorteerde lijst
(define (merge lst1 lst2 <<?) ; lst1 en lst2 zijn allebei al gesorteerd
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (let ((car1 (car lst1))
                    (car2 (car lst2)))
                (if (<<? car1 car2) ; vergelijk telkens het eerste (= kleinste) element van beide lijsten
                    (cons car1 (merge (cdr lst1) lst2 <<?))
                    (cons car2 (merge lst1 (cdr lst2) <<?)))))))

; test
(define l1 '(1 3 5 7 8 9))
(define l2 '(2 4 6))
(display "Eerste lijst: ") (display l1) (newline)
(display "Tweede lijst: ") (display l2) (newline)
(display "Samengevoegde lijst: ") (display (merge l1 l2 <)) (newline) (newline)

; de uiteindelijke merge sort
(define (merge-sort lst <<?)
  (if (> (length lst) 1)
      (let*
          ; 1. splits de lijst in 2 helften
          ;    = SPLITSFUNCTIE schrijven
          ((helften (split lst))
           (linkerhelft (car helften))
           (rechterhelft (cdr helften)))
        ; 2. roep onszelf recursief op voor elk van beide helften
        ;    -> we krijgen twee gesorteerde lijsten terug
        ; 3. voeg die twee gesorteerde lijsten samen
        ;    = SAMENVOEGFUNCTIE schrijven
        (merge (merge-sort linkerhelft <<?)
               (merge-sort rechterhelft <<?)
               <<?))
      lst)) ; als de lijst slechts 1 element bevat, geef dat dan gewoonweg terug

; test
(display "Gesorteerde lijst: ") (display (merge-sort l <))
