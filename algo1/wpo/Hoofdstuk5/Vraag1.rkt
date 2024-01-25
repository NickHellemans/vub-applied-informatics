#lang r7rs
(import (scheme base)
        (scheme write))

;; Bubble Sort on ordinary Scheme lists

; Versie 1: sentinel + swappen met set-cdr!
(define (swap previous-cons-cell)
  (let* ((cons-cell-a (cdr previous-cons-cell)) ; vraag een verwijzing naar cons-cel A
         (cons-cell-b (cdr cons-cell-a)) ; vraag een verwijzing naar cons-cel B
         (next-cons-cell (cdr cons-cell-b))) ; vraag een verwijzing naar de cons-cel volgend op B
    (set-cdr! previous-cons-cell cons-cell-b) ; zorg dat de vorige cons-cel naar cons-cel B           verwijst
    (set-cdr! cons-cell-b cons-cell-a) ;                 cons-cel B         naar cons-cel A
    (set-cdr! cons-cell-a next-cons-cell)) ;             cons-cel A         naar de volgende cons-cel
  )
(define (bubble-sort lst <<?)
  (let ((sentinel-lst (cons 'sentinel lst)))
    (let outer-loop ; buitenste lus: outer-idx gaat van n-1 naar 0
      ((outer-idx (- (length sentinel-lst) 1)))
      (if (>= outer-idx 0)
          ; binnenste lus: doorloop de lijst van in het begin tot je outer-idx cons-cellen hebt bekeken
          (let inner-loop
            ((prev-cons-cell sentinel-lst))
            (if (not (null? (cddr prev-cons-cell)))
                (let ((cons-cell-a (cdr prev-cons-cell))
                      (cons-cell-b (cddr prev-cons-cell)))
                  ; bekijken = kijken of het huidige elt < het volgende elt,
                  (if (<<? (car cons-cell-b) (car cons-cell-a))
                      (swap prev-cons-cell)) ; zoniet: SWAP!
                  (inner-loop (cdr prev-cons-cell)))
                (outer-loop (- outer-idx 1))))
          (cdr sentinel-lst)))))

(define l (list 5 4 3 2 1))
(display l) (newline)
(display (bubble-sort l <)) (newline)


; Versie 2: zonder sentinel + swappen met set-car!
(define (bubble-sort-2 lst <<?)
  (define (bubble-swap cons-cell-a cons-cell-b)
    (let ((keep (car cons-cell-a)))
      (set-car! cons-cell-a (car cons-cell-b))
      (set-car! cons-cell-b keep)
      #t))
  (let outer-loop ; buitenste lus: unsorted-idx gaat van n-2 naar 0
    ((unsorted-idx (- (length lst) 2)))
    (if (>= unsorted-idx 0)
        ; binnenste lus: doorloop de lijst van in het begin tot je unsorted-idx cons-cellen hebt bekeken
        (if (let inner-loop
              ((inner-idx 0)
               (has-changed? #f)
               (curr-cons lst)) ;; TOEGEVOEGD: we houden een verwijzing bij naar de huidige positie (= cons-cel)
              (if (> inner-idx unsorted-idx)
                  has-changed?
                  (let ((next-cons (cdr curr-cons)))
                    (inner-loop (+ inner-idx 1)
                                (if (<<? (car next-cons) ; kijken of het huidige elt < het volgende elt,
                                         (car curr-cons))
                                    (bubble-swap curr-cons next-cons) ; zoniet: SWAP!
                                    has-changed?)
                                next-cons))))
            (outer-loop (- unsorted-idx 1)))))
  lst)

(define k (list 5 4 3 2 1))
(display k) (newline)
(display (bubble-sort-2 k <)) (newline)
