#lang r7rs
(import (scheme base)
        (scheme write))

; Strings en Patroonherkenning
; ============================

; Oefening 1.
; -----------

; Determine the range of ASCII values that corresponds to the characters #\0 to \#9, #\a to #\z, and #\A to #\Z.

; -> De functie char->integer zet een karakter om naar zijn ASCII-waarde, bv.:

(display "De ASCII-waarde van het karakter #\\0 is ") (display (char->integer #\0)) (newline)
(display "De ASCII-waarde van het karakter #\\1 is ") (display (char->integer #\1)) (newline)
(display "De ASCII-waarde van het karakter #\\9 is ") (display (char->integer #\9)) (newline)
; -> De ASCII-waarde van #\0 tot \#9 is 48 tot 57.

(newline)
(display "De ASCII-waarde van het karakter #\\a is ") (display (char->integer #\a)) (newline)
(display "De ASCII-waarde van het karakter #\\b is ") (display (char->integer #\b)) (newline)
(display "De ASCII-waarde van het karakter #\\z is ") (display (char->integer #\z)) (newline)
; -> De ASCII-waarde van #\a tot \#z is 97 tot 122.

(newline)
(display "De ASCII-waarde van het karakter #\\A is ") (display (char->integer #\A)) (newline)
(display "De ASCII-waarde van het karakter #\\B is ") (display (char->integer #\B)) (newline)
(display "De ASCII-waarde van het karakter #\\Z is ") (display (char->integer #\Z)) (newline)
; -> De ASCII-waarde van #\A tot \#Z is 65 tot 90.

; Extra oefening.
; Define a function that takes the ASCII value of a number as its input, and returns the corresponding number, e.g.:
;    (ascii-value->number 48)    ->    0
;    (ascii-value->number 49)    ->    1
;    (ascii-value->number 97)    ->    ERROR: Input is not the ASCII value of a number: 97
(define (ascii-value->number ascii-value)
  (if (or (< ascii-value 48)
          (> ascii-value 57))
      (error "Input is not the ASCII value of a number" ascii-value)
      (- ascii-value 48)))
(newline)
(display "(ascii-value->number 48) -> ") (display (ascii-value->number 48)) (newline)
(display "(ascii-value->number 49) -> ") (display (ascii-value->number 49)) (newline)
; (display "(ascii-value->number 97) -> ") (display (ascii-value->number 97)) (newline)

; Oefening 2.
; -----------

; Write a procedure of type (string -> number) that converts a string containing 
; any combination of numeric characters (i.e., characters between #\0 and #\9) 
; to the corresponding number, e.g.:
;    (my-string->number "1234")    ->    1234
(define (my-string->number s)
  (define (iter position result)
    (if (= position (string-length s))
        result
        (iter (+ position 1)
              (+ (* result 10) 
                 (ascii-value->number (char->integer (string-ref s position)))))))
  (iter 0 0))
; -> Deze functie itereert over de verschillende karakters van s, 
; te beginnen bij het minst significante karakter, dus van rechts naar links.  
; In elke stap wordt het huidige resultaat verhoogd met het product van de 
; getalwaarde van het huidige karakter en de huidige macht van 10, die 
; oorspronkelijk 1 is en steeds met 10 vermenigvuldigd zal worden.
(newline)
(display "(my-string->number \"1234\") -> ") (display (my-string->number "1234")) (newline)

; Determine Omega(f(n)) for your algorithm, when n is the length of the string.
; -> Omega(n), want er zijn evenveel iteraties als karakters.

; Determine Omega(f(n)) for your algorithm, when n is the value of the number.
; -> Omega(log(n)), want een getal n bestaat uit ceiling(log10(n+1)) cijfers.

; Oefening 3.
; -----------

; Consider the text t = "helterskelter" and the pattern "elter".  
; Consider the fragments v = "helter" and w = "ter".  
; Fill in the blanks:

; -> v is a prefix of t.
; -> w is a suffix of t.
; -> The length of w is 3.  This is also denoted as |w| = 3.
; -> v is a proper prefix of t, because v is not equal to the empty string or to t.

; Oefening 4.
; -----------

; Consider the string "Hello".  Enumerate all prefixes, all suffixes, all proper prefixes and all proper suffixes.

; -> Prefixes: "", "H", "He", "Hel", "Hell", "Hello"
; -> Suffixes: "", "o", "lo", "llo", "ello", "Hello"
; -> Proper prefixes: "H", "He", "Hel", "Hell", d.i. alle prefixen die niet gelijk zijn aan de lege string of aan de string zelf.
; -> Proper suffixes: "o", "lo", "llo", "ello", d.i. alle suffixen due niet gelijk zijn aan de lege string of aan de string zelf.

; Oefening 5.
; Write the procedure type for the match procedures discussed in this chapter.

; -> De match procedures hebben twee parameters, een string t en een string p, 
; en geven ofwel een positie binnen de string t terug, ofwel false.  
; Het proceduretype is dus (string string -> number U {#f}).

; Oefening 6.
; -----------

(define (match t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (let loop
    ((i-t 0)
     (i-p 0)
     (shifts '())) ; Bouw een lijst op van gevonden shifts
    (cond
      ((> i-p (- n-p 1)) 
       (loop (+ i-t 1) 0 (cons i-t shifts))) ; We hebben een shift gevonden; voeg deze toe aan de lijst
      ((> i-t (- n-t n-p))
       (reverse shifts)) ; We kunnen geen shift meer vinden; geef de lijst terug
      ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
       (loop i-t (+ i-p 1) shifts))
      (else
       (loop (+ i-t 1) 0 shifts)))))

(newline)
(display "(match \"abracadabra\" \"bra\") -> ") (display (match "abracadabra" "bra")) (newline)


; Alternatieve oplossing zonder een extra variabele "shifts" toe te voegen aan de let
(define (match-rec t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (let loop
    ((i-t 0)
     (i-p 0))
    (cond
      ((> i-p (- n-p 1))
       (cons i-t (loop (+ i-t 1) 0))) ;; aangepast
      ((> i-t (- n-t n-p))
       '()) ; aangepast
      ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
       (loop i-t (+ i-p 1)))
      (else
       (loop (+ i-t 1) 0)))))


; Oefening 7.
; -----------

; Versie 1
; -> De volgende procedure is gebaseerd op de oplossing voor oefening 6.  
; De procedure neemt een extra parameter 'wildcard-char', die aangeeft door welk karakter een wildcard wordt voorgesteld.  
; Elk voorkomen van zo'n karakter in het patroon zal precies één willekeurig karakter matchen.
(define (wildcard-match t p wildcard-char)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (let loop
    ((i-t 0)
     (i-p 0)
     (shifts '()))
    (cond
      ((> i-p (- n-p 1))
       (loop (+ i-t 1) 0 (cons i-t shifts)))
      ((> i-t (- n-t n-p))
       (reverse shifts))
      ((or (eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
           (eq? (string-ref p i-p) wildcard-char)) ; Als het huidige karakter van het patroon de wildcard is, gaan we ook verder
       (loop i-t (+ i-p 1) shifts))
      (else
       (loop (+ i-t 1) 0 shifts)))))

(newline)
(display "(wildcard-match \"abrecidobru\" \".br.\" #\\.) -> ") (display (wildcard-match "abrecidobru" ".br." #\.)) (newline)


; Versie 2
; Een oplossing waarbij een wildcard 0 of meerdere karakters mag voorstellen, bijvoorbeeld
; patroon = "ba*ab", de wildcard (het sterretje) vervangt meerdere karakters die mogen 'geskipt' worden
; text = "ba-xyz-ab" (match, want "-xyz-" mag worden overgeslagen
; Voor de gemakkelijkheid gaan we ervan uit dat het input patroon al is opgesplitst in een lijst van patronen die "na elkaar" gevonden moeten worden
;  dus patroon "ba*ab" gaan we voorstellen als de input lijst van patronen (list "ba" "ab")
(define (wildcard-brute-force-match text patterns)
  
  ;; Dit is de originele match procedure van het brute-force algoritme,
  ; maar met een extra functie parameter "start-pos" waarmee we kunnen aangeven vanaf welk karakter in de text het matchen moet beginnen
  ; Normaal begon dit altijd van 0, maar nu kunnen we het matchen laten beginnen vanaf eender welk karakter in de tekst.
  ; Dit kunnen we gebruiken om stukken tekst over te slagen waarin we al een match hebben gevonden van een deel van het patroon.
  (define (match-single text pattern start-pos)
    (define n-t (string-length text))
    (define n-p (string-length pattern))
    (let loop
      ((i-t start-pos)
       (i-p 0))
      (cond
        ((> i-p (- n-p 1))
         i-t)
        ((> i-t (- n-t n-p))
         #f)
        ((eq? (string-ref text (+ i-t i-p)) (string-ref pattern i-p))
         (loop i-t (+ i-p 1)))
        (else
         (loop (+ i-t 1) 0)))))

  ;; nieuwe "iter" procedure om over de lijst van patronen te loopen, om deze één voor één terug te vinden in de tekst (van links naar rechts)
  ; argumenten zijn:
  ;  - remaining-patterns: lijst van patronen die steeds gaat krimpen naargelang we deze gaan zoeken (van begin tot eind) in de tekst
  ;  - result: het resultaat dat we gaan opbouwen
  ;            in het oorspronkelijke brute-force algoritme is het resultaat de index in de tekst waar het patroon begint,
  ;            maar in deze functie gaan we een lijst van resultaten opbouwen, namelijk die index voor elk stukje van het patroon in de input lijst in variabele "patterns"
  ;  - text-starting-pos: dit argument geeft aan vanaf welke positie in de tekst we voor het 'volgende stukje van het patroon' beginnen te zoeken
  (define (iter remaining-patterns result text-starting-pos)
    (if (null? remaining-patterns) ; als de lijst leeg is, geef resultaat terug
        (reverse result) ; reverse omdat indexen altijd vooraan eraan geconst worden, dus de index van het laatste stukje van het patroon zal anders vooraan in de lijst staan
        (let* ((current-pattern (car remaining-patterns)) ; volgende patroon om in de tekst te zoeken
               (pattern-idx (match-single text current-pattern text-starting-pos))) ; dat patroon wordt gevonden op "pattern-idx" in de tekst (of #f indien niet gevonden)
          (if pattern-idx ; als het patroon gevonden is...
              (iter (cdr remaining-patterns) ; zoek het volgende patroon
                    (cons pattern-idx result) ; hou het resultaat bij waar het huidige patroon gevonden is
                    (+ pattern-idx (string-length current-pattern))) ; begin pas te zoeken naar het volgende stukje van het patroon _achter_ het huidige patroon in de tekst, want patronen kunnen niet overlappen

              #f))))
  (iter patterns '() 0))

; Versie 3
; -> De oplossing waarbij elk voorkomen van een wildcard karakter in het patroon nul of meer willekeurige karakters zal matchen,
; kan je zien als een procedure die meerdere "deelpatronen" probeert te matchen, 
; met tussen elk deelpatroon nul of meer willekeurige karakters, tot alle patronen gematched zijn.
(define (wildcard-match-two text . patterns)
  (define n-t (string-length text))
  (define minimum-length-pattern (apply + (map string-length patterns)))
  (define shifts '())
  (define (match-characters i-t i-p patterns-to-check)
    (if (null? patterns-to-check)
        i-t ; dit is de positie van het karakter in de text waar het laatste karakter van het laatste patroon in patterns mee overeenkomt
        (if (> i-t (- n-t 1))
            #f
            (if (eq? (string-ref text i-t)
                     (string-ref (car patterns-to-check) i-p))
                (if (= i-p (- (string-length (car patterns-to-check)) 1))
                    (match-characters (+ 1 i-t) 0 (cdr patterns-to-check))
                    (match-characters (+ 1 i-t) (+ 1 i-p) patterns-to-check))
                (if (< (length patterns-to-check) (length patterns))
                    (match-characters (+ 1 i-t) 0 patterns-to-check) ; een wildcard, ga een positie verder in de tekst
                    #f)))))
  (define idx #f) ; nodig in de de derde tak van de conditional
  (define (loop-text i-t)
    (cond ((> i-t (- n-t minimum-length-pattern))
           (reverse shifts))
          ((let ((end-idx (match-characters i-t 0 patterns)))
             (set! idx end-idx)
             (number? end-idx))
           (set! shifts (cons (cons i-t idx) shifts)) ; idx want parameter end-idx is niet meer beschikbaar
           (loop-text (+ 1 i-t)))
          (else
           (loop-text (+ 1 i-t)))))
  (loop-text 0))

(newline)
(display "(wildcard-match-two \"abracadabrabra\" \"ab\" \"ab\") -> ") 
(display (wildcard-match-two "abracadabrabra" "ab" "ab"))
(newline)

; Oefening 8.
; -----------

; Enkele voorbeelden vanuit de Nederlandse taal
; (bron: http://home.wxs.nl/~avdw3b/herhaal.html)

; berber
; blabla
; bonbon
; cancan
; couscous
; dodo
; genegene
; haha
; hèhè
; hihi
; inging
; jojo
; kankan
; kerker
; mama
; papa
; renderende
; tamtam
; tenten

; Oefening 9.
; -----------

; In het verwerken van het patroon in het KMP algoritme, ga je je patroon aflopen
; en op zoek gaan naar "herhalingen" in dat patroon.

; Voor iedere positie i-p in je patroon ga je nagaan in de voorgaande karakters
; (d.i. de substring van karakters gaande van positie 0 t.e.m i-p - 1) of je een 
; prefix vindt dat ook als suffix terug te vinden is in diezelfde substring.
; Je probeert die zo groot mogelijk te maken.

; We gaan dus stelselmatig een vector ter grootte van de lengte van het patroon 
; opvullen met de lengtes van de grootste gevonden prefixen dat ook suffixen 
; zijn in die substrings.

; Maak hiervoor reeds een vector aan waarin we standaard nullen plaatsen.

; Voor het patroon "helter" wordt dit dus een vector van 6 groot:
;    h e l t e r
; #( 0 0 0 0 0 0 )

; Je loopt met een index i-p (of handmatig met je vinger) alle karakters af van het patroon,
; en kijkt telkens terug naar de karakters "links" van je index, 
; Omdat je pas van "herhaling" kunt gaan spreken vanaf dat we reeds 2 karakters
; in de substring zouden hebben, beginnen we deze procedure dan ook maar te doen 
; vanaf positie 2 in het patroon (= het derde karakter!), dus beginnen bij i-p=2.

; Voor de string "helter" beginnen we de dus bij het karakter l .
; De linker-substring die we daar hebben is dus "he" (onderlijnd in het voorbeeld hieronder).

;    h e l t e r
;    ___ ^
;        |
;       i-p
;
; #( 0 0 0 0 0 0 )
;        _
;
; Deze heeft geen prefix dat ook een suffix is, want h != e.
; Dus zetten we een nul in de vector op positie i-p = 2 (ook onderlijnd).

; Nu schuif je op met je index naar het volgende karakter, i-p wordt dus 3 en de substring wordt "hel".
;    h e l t e r
;    _____ ^
;          |
;         i-p
;
; #( 0 0 0 0 0 0 )
;          _
;
; Ook hier geen prefixen dat ook suffixen zijn in deze substring,
; want "h" != "l" en "he" != "el"  
; Dus zetten we weer een nul in de vector op positie i-p = 3.

; Dit blijf je dus verder doen tot en met het laatste karakter van je patroon.
; Je zal ondertussen waarschijnlijk al gemerkt hebben dat we in het patroon "helter"
; geen herhalingen van het gewenste kaliber kunnen vinden en bekomen dus een vector met nullen:
;    h e l t e r
;    _________ ^
;              |
;             i-p
;
; #( 0 0 0 0 0 0 )
;              _

; Om af te sluiten schrijf je (per conventie) in de allereerste positie van je tabel het getal -1.

; -> de sigma-table wordt dus #( -1 0 0 0 0 0 )

; Ter controle kan je een display toevoegen in de compute-failure-function en deze eens oproepen.
; Zie hieronder: 
(define (compute-failure-function p)
  (define n-p (string-length p))
  (define sigma-table (make-vector n-p 0))
  (let loop
    ((i-p 2)
     (k 0))
    (when (< i-p n-p)
      (cond
        ((eq? (string-ref p k) 
              (string-ref p (- i-p 1)))
         (vector-set! sigma-table i-p (+ k 1))
         (loop (+ i-p 1) (+ k 1)))
        ((> k 0)
         (loop i-p (vector-ref sigma-table k)))
        (else ; k=0
         (vector-set! sigma-table i-p 0)
         (loop (+ i-p 1) k))))
    (vector-set! sigma-table 0 -1)
    (display sigma-table) ; display toegevoegd hier
    (newline) ; newline toegevoegd hier
    (lambda (q)
      (vector-ref sigma-table q))))

(newline)
(display "De sigma-table voor (compute-failure-function \"helter\") is: ")
(compute-failure-function "helter")
(newline)

; Oefening 10.
; ------------

; De procedure computer-failure-function neemt een patroon als invoer en gaat hiervoor 
; een tabel opstellen in de vorm van een vector. Het patroon heeft het datatype string.
; De laatste expressie in de body van de procedure is de volgende:
;
; (lambda (q)
;     (vector-ref sigma-table q))
; 
; Lambda is een anonieme procedure en deze lambda wordt dus teruggegeven als resultaat van de procedure.
; Dit betekent dat compute-failure-function een hogere-orde procedure is!

; De lambda-expressie heeft 1 argument, q, die gebruikt wordt als index om in een vector op te zoeken.
; Vermits de vector, sigma-table, getallen bewaart, zal een oproep van deze lambda een getal opleveren.
; Het proceduretype van deze lambda is dus (number -> number).

; Het proceduretype van computer-failure-function wordt hierdoor dus:
; (string -> (number -> number) )


; Oefening 11.
; ------------

; We hernemen de procedure match uit de a-d folder (a-d pattern-matching kmp) en plaatsen
; een aantal oproepen van display in de body van de named-let om de waarden van de parameters i-t en i-p te printen.
; Hierdoor kan je de waarden zien van deze indexen in iedere iteratie van loop.
; Je kan het patroon in iedere iteratie oplijnen tov de tekst door voor het patroon de juiste hoeveelheid spaties ervoor te printen.
; Die hoeveelheid komt in iedere iteratie overeen met de waarde van i-t.

(define (kmp-match t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (define sigma (compute-failure-function p)) 
  (let loop
    ((i-t 0)
     (i-p 0))

    (display "i-t = ")
    (display i-t)
    (newline)
    (display "i-p = ")
    (display i-p)
    (newline)
    (display "Alignment t: ")
    (display t)
    (newline)
    (display "Alignment p: ")
    (display (string-append (make-string i-t #\space) p)) ; om het patroon op te lijnen tov de tekst.
    (newline)
    (display "--------")
    (newline)

    (cond 
      ((> i-p (- n-p 1))
       i-t)
      ((> i-t (- n-t n-p))
       #f)
      ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
       (loop i-t (+ i-p 1)))
      (else
       (loop (+ i-t (- i-p (sigma i-p))) (if (> i-p 0)
                                             (sigma i-p)
                                             0))))))

(define P "ABCDABD")
(define T "ABC ABCDAB ABCDABCDABDE")
(kmp-match T P)

; Oefening 12.
; ------------

; QuickSearch heeft veel werk indien het patroon mismatch op het laatste karakter,
; én de alignment gebeurt met het allereerste karakter van dat patroon.
; Op die manier zijn de shifts die het algoritme maakt minimaal en het algoritme
; begint telkens het patroon vanaf het begin te vergelijken met de tekst.

; Een voorbeeld waarbij dit zich voordoet is:
; tekst: "bbbbbbbbbbbbbab"
; patroon: "bab"

; We hernemen de procedures 'compute-shift-function' en 'match' uit de a-d folder (a-d pattern-matching quicksearch) en plaatsen
; een aantal oproepen van display in de body van de named-let van 'match' om de waarden van de parameters i-t en i-p te printen.
; Hierdoor kan je de waarden zien van deze indexen in iedere iteratie van loop.
; Je kan het patroon in iedere iteratie oplijnen tov de tekst door voor het patroon de juiste hoeveelheid spaties ervoor te printen.
; Die hoeveelheid komt in iedere iteratie overeen met de waarde van i-t.

(define (compute-shift-function p)
  (define n-p (string-length p))
  (define min-ascii (char->integer (string-ref p 0)))
  (define max-ascii min-ascii)
   
  (define (create-table index)
    (if (< index n-p)
        (begin
          (set! min-ascii (min min-ascii (char->integer (string-ref p index))))
          (set! max-ascii (max max-ascii (char->integer (string-ref p index))))
          (create-table (+ index 1)))
        (make-vector (- max-ascii min-ascii -1) (+ n-p 1)))) 
   
  (define (fill-table index)
    (if (< index n-p)
        (let ((ascii (char->integer (string-ref p index))))
          (vector-set! shift-table (- ascii min-ascii) (- n-p index))
          (fill-table (+ index 1)))))
   
  (define shift-table (create-table 0))
  (fill-table 0)
  (lambda (c)
    (let ((ascii (char->integer c)))
      (if (>= max-ascii ascii min-ascii)
          (vector-ref shift-table (- ascii min-ascii))
          (+ n-p 1)))))
 
(define (qs-match t p)
  (define n-t (string-length t))
  (define n-p (string-length p))
  (define shift (compute-shift-function p))
  (let loop
    ((i-t 0)
     (i-p 0))
    
    (display "i-t = ")
    (display i-t)
    (newline)
    (display "i-p = ")
    (display i-p)
    (newline)
    (display "Alignment t: ")
    (display t)
    (newline)
    (display "Alignment p: ")
    (display (string-append (make-string i-t #\space) p)) ; om het patroon op te lijnen tov de tekst.
    (newline)
    (display "--------")
    (newline)

    (cond 
      ((> i-p (- n-p 1))
       i-t)
      ((> i-t (- n-t n-p))
       #f)
      ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
       (loop i-t (+ i-p 1)))
      (else
       (let ((c-t (string-ref t (modulo (+ i-t n-p) n-t))))
         (loop (+ i-t (shift c-t)) 0))))))

(set! T "bbbbbbbbbbbbbab")
(set! P "bab")
(newline)
(display "Outline QuickSearch:")
(newline)
(qs-match T P)