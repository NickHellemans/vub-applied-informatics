#lang r7rs
(import (scheme base)
        (scheme write)
        (prefix (a-d stack vectorial) stack:)
        (prefix (a-d queue list) queue:)
        (prefix (a-d heap standard) heap:))

; Lineaire ADT's
; --------------

; Oefening 1.
; Implement a procedure postfix-eval that evaluates a Scheme list representing expressions in postfix notation, e.g.:
;    (postfix-eval (list 5 6 +))       ->    11
;    (postfix-eval (list 5 6 + 7 -)    ->     4
; -> Ter herinnering: postfix-notatie is de notatie waarbij operaties na de operanden worden geschreven.  In infix-notatie zouden de twee voorbeeld-expressies als 5 + 6 en  5 + 6 - 7 geschreven worden, in prefix-notatie als - 5 6 en - + 5 6 7.  Expressies in postfix-notatie zijn makkelijk te evalueren door gebruik van een stack: je doorloopt de expressie, en als je een operand tegenkomt, voeg je deze toe aan de stack.  Als je een operator tegenkomt, haal je alle operanden van de stack, voer je de operator uit op deze operanden, en voeg je het resultaat toe aan de stack.  Wanneer je op het einde van de expressie gekomen bent, bevindt het resultaat zich op de stack.  Voor de tweede voorbeeld-expressie ziet dit proces er ongeveer als volgt uit:
;
;    stack (top = rechts)    expressie    actie
;    --------------------    ---------    -----
;                            5 6 + 7 -    5 is een operand, en wordt dus aan de stack toegevoegd
;    5                       6 + 7 -      6 is een operand, en wordt dus aan de stack toegevoegd
;    5 6                     + 7 -        + is een operator, dus we halen 6 en 5 van de stack, voeren de operator uit op deze operanden, en voegen het resultaat 11 toe aan de stack
;    11                      7 -          7 is een operand, en wordt dus aan de stack toegevoegd
;    11 7                    -            - is een operator, dus we halen 7 en 11 van de stack, voeren de operator uit op deze operanden (OPGELET MET DE VOLGORDE!), en voegen het resultaat 4 toe aan de stack
;    4                                    we zijn op het einde van de expressie gekomen, en geven dus de top van de stack als resultaat terug
;

; -> Eerste mogelijke oplossing, die een named let gebruikt om over de elementen van de expressie te itereren:
(define (postfix-eval expression)
  (define stack (stack:new))
  (let loop
    ((current expression)) ; current verwijst steeds naar de cons-cel die het huidige element bevat
    (if (null? current) ; als current gelijk is aan de lege lijst, zijn we op het einde van de expressie gekomen, en geven we de top van de stack als resultaat terug
        (stack:pop! stack)
        (let ((element (car current))) ; anders bekijken we het huidige element
          (if (number? element) ; als het huidige element een getal is, voegen we het gewoon toe aan de stack
              (stack:push! stack element)
              (let ((operand2 (stack:pop! stack)) ; anders is het huidige element een operator, en halen we de twee operanden van de stack
                    (operand1 (stack:pop! stack)))
                (stack:push! stack (element operand1 operand2)))) ; vervolgens voeren we de operator uit op de operanden, en voegen het resultaat toe aan de stack
          (loop (cdr current)))))) ; tenslotte roepen we de named let op met als nieuwe current de cdr van de huidige current
(display "(postfix-eval (list 5 6 + 7 -))     ->    ") (display (postfix-eval (list 5 6 + 7 -))) (newline)

; -> Tweede mogelijke oplossing, die een iteratieve hulpprocedure gebruikt om over de elementen van de expressie te itereren:
(define (postfix-eval2 expression)
  (define stack (stack:new))
  (define (loop current)
    (if (null? current) ; de body van de hulpprocedure is dezelfde als die van de named let in de vorige mogelijke oplossing
        (stack:pop! stack)
        (let ((element (car current)))
          (if (number? element)
              (stack:push! stack element)
              (let ((operand2 (stack:pop! stack))
                    (operand1 (stack:pop! stack)))
                (stack:push! stack (element operand1 operand2))))
          (loop (cdr current)))))
  (loop expression))
(display "(postfix-eval2 (list 5 6 + 7 -))    ->    ") (display (postfix-eval2 (list 5 6 + 7 -))) (newline)

; -> Derde mogelijke oplossing, waarbij operatoren een VARIABEL aantal operanden kunnen hebben:
(define (get-all-elements-as-list! stack) ; deze hulpprocedure verwijdert alle elementen van de stack, en geeft deze in omgekeerde volgorde terug als een Scheme lijst
  (let loop
    ((lst '()))
    (if (stack:empty? stack)
        lst
        (loop (cons (stack:pop! stack) lst)))))

(define (postfix-eval3 expression)
  (define stack (stack:new))
  (let loop
    ((current expression))
    (if (null? current)
        (stack:pop! stack)
        (let ((element (car current)))
          (if (number? element)
              (stack:push! stack element)
              (stack:push! stack (apply element (get-all-elements-as-list! stack)))) ; dit is de enige lijn die verschilt t.o.v. de eerste mogelijke oplossing
          (loop (cdr current))))))

(display "(postfix-eval3 (list 5 6 + 7 -))    ->    ") (display (postfix-eval3 (list 5 6 + 7 -))) (newline)
; -> Er zijn uiteraard nog veel meer mogelijke oplossingen.

; Oefening 2.

(define (opening-tag? symbol) ; true als het eerste karakter < is, het tweede karakter niet / is, en het laatste karakter > is
  (define s (symbol->string symbol))
  (and (equal? (string-ref s 0) #\<)
       (not (equal? (string-ref s 1)#\/))
       (equal? (string-ref s (- (string-length s) 1)) #\>)))
(display "(opening-tag? '<html>)      ->    ") (display (opening-tag? '<html>)) (newline)
(display "(opening-tag? '</html>)     ->    ") (display (opening-tag? '</html>)) (newline)

(define (closing-tag? symbol) ; true als het eerste karakter < is, het tweede karakter / is, en het laatste karakter > is
  (define s (symbol->string symbol))
  (and (equal? (string-ref s 0) #\<)
       (equal? (string-ref s 1)#\/)
       (equal? (string-ref s (- (string-length s) 1)) #\>)))
(display "(closing-tag? '<html>)      ->    ") (display (closing-tag? '<html>)) (newline)
(display "(closing-tag? '</html>)     ->    ") (display (closing-tag? '</html>)) (newline)

(define (matches? symbol-a symbol-b) ; true als het eerste symbool een opening parenthesis is, het tweede symbool een closing parenthesis is, en de "inhoud" van het eerste symbool gelijk is aan die van het tweede symbool
  (define s-a (symbol->string symbol-a))
  (define s-b (symbol->string symbol-b))
  (and (opening-tag? symbol-a)
       (closing-tag? symbol-b)
       (equal? (substring s-a 1 (- (string-length s-a) 1))
               (substring s-b 2 (- (string-length s-b) 1)))))
(display "(matches? '<html> '</html>))        ->    ") (display (matches? '<html> '</html>)) (newline)
(display "(matches? '<html> '</body>))        ->    ") (display (matches? '<html> '</body>)) (newline)

(define (valid? lst)
  (define stack (stack:new))
  (let loop
    ((current lst)) ; current verwijst steeds naar de cons-cel die het huidige element bevat
    (if (null? current) ; als current gelijk is aan de lege lijst, zijn we op het einde van de lijst gekomen, en geven we #t terug
        (stack:empty? stack)
        (let ((element (car current))) ; anders bekijken we het huidige element
          (if (opening-tag? element) ; als het huidige element een opening parenthesis is, voegen we het gewoon toe aan de stack
              (begin (stack:push! stack element)
                     (loop (cdr current)))
              (and (matches? (stack:pop! stack) element) ; anders is het huidige element een closing parenthesis, en controleren we of deze matcht met de top van de stack; door de lazy evaluatie van and wordt er hier onmiddellijk #f teruggegeven als dit niet zo is
                   (loop (cdr current))))))))
(display "(valid? '(<html> <head> <title> </title> </head> <body> <p> </p> </body> </html>))    ->    ") (display (valid? '(<html> <head> <title> </title> </head> <body> <p> </p> </body> </html>))) (newline)
; De uitvoering van dit voorbeeld kan dan als volgt worden voorgesteld (elke lijn is een nieuwe iteratie van "loop"):
;
;    stack (top = rechts)     lijst                                                                     actie
;    --------------------     -----                                                                     -----
;                             <html> <head> <title> </title> </head> <body> <p> <p> </body> </html>     <html> is een opening parenthesis, en wordt dus aan de stack toegevoegd
;    <html>                   <head> <title> </title> </head> <body> <p> <p> </body> </html>            <head> is een opening parenthesis, en wordt dus aan de stack toegevoegd
;    <html> <head>            <title> </title> </head> <body> <p> <p> </body> </html>                   <title> is een opening parenthesis, en wordt dus aan de stack toegevoegd
;    <html> <head> <title>    </title> </head> <body> <p> <p> </body> </html>                           </title> is een closing parenthesis, dus we controleren of deze matcht met de top van de stack (ja)
;    <html> <head>            </head> <body> <p> <p> </body> </html>                                    </head> is een closing parenthesis, dus we controleren of deze matcht met de top van de stack (ja)
;    <html>                   <body> <p> <p> </body> </html>                                            <body> is een opening parenthesis, en wordt dus aan de stack toegevoegd
;    <html> <body>            <p> <p> </body> </html>                                                   <p> is een opening parenthesis, en wordt dus aan de stack toegevoegd
;    <html> <body> <p>        <p> </body> </html>                                                       <p> is een opening parenthesis, en wordt dus aan de stack toegevoegd
;    <html> <body> <p> <p>    </body> </html>                                                           </body> is een closing parenthesis, dus we controleren of deze matcht met de top van de stack (NEEN); door de lazy evaluatie van and wordt er onmiddellijk #f teruggegeven
(display "(valid? '(<html> <head> <title> </title> </head> <body> <p> <p> </body> </html>))     ->    ") (display (valid? '(<html> <head> <title> </title> </head> <body> <p> <p> </body> </html>))) (newline)

; Oefening 3.
(define (josephus lst m)
  (define queue (queue:new))
  (let prepare-queue ; in een eerste stap voegen we alle elementen van de lijst toe aan de queue
    ((current lst))
    (when (not (null? current))
      (queue:enqueue! queue (car current))
      (prepare-queue (cdr current))))
  (let process-queue ; in een tweede stap lopen we door de queue tot deze leeg is
    ((i 1))
    (let ((element (queue:serve! queue))) ; we halen het eerste element uit de queue
      (cond ((queue:empty? queue) element) ; als de queue dan leeg is, geven we het element als resultaat terug
            ((= i m) (process-queue 1)) ; als i gelijk is aan m, gaan we gewoon verder (waardoor het element uit de queue verdwijnt), met i gelijk aan 1
            (else (queue:enqueue! queue element) ; anders voegen we het element terug toe aan de queue vooraleer we verdergaan, met i gelijk aan i + 1
                  (process-queue (+ i 1)))))))
;    queue (head = links)    i    actie
;    --------------------    -    -----
;    a b c d e               1    i is niet gelijk aan m, dus voeg a toe op het einde van de queue
;    b c d e a               2    i is niet gelijk aan m, dus voeg b toe op het einde van de queue
;    c d e a b               3    i is gelijk aan m, dus voeg c NIET toe op het einde van de queue en zet i terug op 1
;    d e a b                 1    i is niet gelijk aan m, dus voeg d toe op het einde van de queue
;    e a b d                 2    i is niet gelijk aan m, dus voeg e toe op het einde van de queue
;    a b d e                 3    i is gelijk aan m, dus voeg a NIET toe op het einde van de queue en zet i terug op 1
;    b d e                   1    i is niet gelijk aan m, dus voeg b toe op het einde van de queue
;    d e b                   2    i is niet gelijk aan m, dus voeg d toe op het einde van de queue
;    e b d                   3    i is gelijk aan m, dus voeg e NIET toe op het einde van de queue en zet i terug op 1
;    b d                     1    i is niet gelijk aan m, dus voeg b toe op het einde van de queue
;    d b                     2    i is niet gelijk aan m, dus voeg d toe op het einde van de queue
;    b d                     3    i is gelijk aan m, dus voeg b NIET toe op het einde van de queue en zet i terug op 1
;    d                       1    er is nog maar één element in de queue, dus we geven dit element als resultaat terug
(display "(josephus '(a b c d e) 3)      ->    ") (display (josephus '(a b c d e) 3)) (newline)

; Oefening 4.
; a) Stack
;    Stel dat we een stack d.m.v. een positionele lijst met vector-implementatie implementeren, dan kunnen we steeds efficiënt elementen toevoegen en verwijderen OP HET EINDE van de positionele lijst (niet in het begin!).  In dat geval is de performantie van stack:push! O(1), want de performantie van plist:attach-last! is O(1).  De performantie van stack:pop! is O(1), want de performantie van plist:last, plist:peek en plist:detach-last! is O(1).
;    Stel dat we een stack d.m.v. een positionele lijst met enkel-gelinkte-lijst-implementatie implementeren, dan kunnen we steeds efficiënt elementen toevoegen en verwijderen IN HET BEGIN van de positionele lijst (niet op het einde!).  In dat geval is de performantie van stack:push! O(1) want de performantie van plist:attach-first! is O(1).  De performantie van stack:pop! is O(1), want de performantie van plist:first, plist:peek en plist:detach-first! is O(1).
;    Voor een stack die geïmplementeerd wordt d.m.v. een positionele lijst met dubbel-gelinkte-lijst-implementatie geldt hetzelfde als voor de enkel-gelinkte-lijst-implementatie: we kunnen steeds efficiënt elementen toevoegen en verwijderen in het begin van de positionele lijst, maar niet op het einde.
;    Stel dat we een stack d.m.v. een positionele lijst met verbeterde-dubbel-gelinkte-lijst-implementatie implementeren, dan kunnen we steeds efficiënt elementen toevoegen en verwijderen aan beide uiteinden van de positionele lijst.
; b) Queue
;    In tegenstelling tot bij een stack, gebeurt bij een queue het toevoegen en verwijderen niet aan hetzelfde uiteinde van de positionele lijst.  Enkel de verbeterde-dubbel-gelinkte-lijst-implementatie ondersteunt efficiënt (O(1)) toevoegen of verwijderen aan beide uiteinden, bij de andere implementaties zal ofwel enqueue!, ofwel serve! in O(n) gebeuren.
