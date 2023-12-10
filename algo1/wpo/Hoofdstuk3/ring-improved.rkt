#lang r7rs
(define-library
  (ring-improved)
  (export ring? length new from-scheme-list add-after! add-before! shift-forward! shift-backward! delete! update! peek display)
  (import (except (scheme base) length)
          (prefix (scheme write) io:))
  (begin
    (define-record-type ring
      (make-ring c l)
      ring?
      (c current current!)
      (l length length!)) ; Hou ook de lengte van de ring bij
 
    (define (new)
      (make-ring '() 0)) ;  De lengte van een lege ring is 0
 
    (define (make-ring-node val next prev) (vector val next prev)) ; Hou ook de prev van de node bij
    (define (ring-node-val node) (vector-ref node 0))
    (define (ring-node-val! node val) (vector-set! node 0 val))
    (define (ring-node-next node) (vector-ref node 1))
    (define (ring-node-next! node next) (vector-set! node 1 next))
    (define (ring-node-prev node) (vector-ref node 2)) ; Geef de prev van de node terug
    (define (ring-node-prev! node prev) (vector-set! node 2 prev)) ; Verander de prev van de node
 
    (define (from-scheme-list slst)
      (let loop
        ((scml slst)
         (ring (new)))
        (if (null? scml)
            ring
            (loop (cdr scml) (add-after! ring (car scml))))))
 
    (define (add-after! ring val)
      (define curr (current ring))
      (define node (make-ring-node val '() '()))
      ; Zet de next van de nieuwe node goed:
      (ring-node-next! node 
                       (if (null? curr)
                           node
                           (ring-node-next curr)))
      ; Zet de prev van de nieuwe node goed:
      (ring-node-prev! node
                       (if (null? curr)
                           node
                           curr))
      ; Zet de next van de vorige node goed:
      (if (not (null? curr))
          (ring-node-next! curr node))
      ; Zet de prev van de volgende node goed:
      (if (not (null? curr))
          (ring-node-prev! (ring-node-next node) node))
      (length! ring (+ (length ring) 1)) ; Verhoog de lengte met 1
      (current! ring node) 
      ring)
 
    (define (add-before! ring val)
      (define curr (current ring))
      (define node (make-ring-node val curr '())) ; Zet de next van de nieuwe node goed
      ; Zet de prev van de nieuwe node goed:
      (ring-node-prev! node
                       (if (null? curr)
                           node
                           (ring-node-prev curr)))
      ; Zet de next van de vorige node goed:
      (ring-node-next!
       (if (null? curr)
           node
           (ring-node-prev curr)) ; Gebruik de nieuwe ring-node-prev procedure i.p.v. de oude iter-to-previous
       node)
      ; Zet de prev van de volgende node goed:
      (if (not (null? curr))
          (ring-node-prev! curr node))
      (length! ring (+ (length ring) 1)) ; Verhoog de lengte met 1
      (current! ring node)
      ring)
 
    (define (shift-forward! ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (shift-forward!)" ring))
      (current! ring (ring-node-next curr))
      ring)
 
    (define (shift-backward! ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (shift-backward!)" ring)
          (current! ring (ring-node-prev curr))) ; Gebruik de nieuwe ring-node-prev procedure i.p.v. de oude iter-to-previous
      ring)
 
    (define (delete! ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (delete!)" ring))
      ; Zet de next van de vorige node goed:
      (ring-node-next!
       (ring-node-prev curr) ; Gebruik de nieuwe ring-node-prev procedure i.p.v. de oude iter-to-previous
       (ring-node-next curr))
      ; Zet de prev van de volgende node goed:
      (ring-node-prev!
       (ring-node-next curr)
       (ring-node-prev curr))
      (length! ring (- (length ring) 1)) ; Verlaag de lengte met 1
      (if (eq? curr (ring-node-next curr))
          (current! ring '())
          (current! ring (ring-node-next curr)))
      ring)
 
    (define (update! ring val)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (update!)"ring)
          (ring-node-val! curr val)))
 
    (define (peek ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (peek)" ring)
          (ring-node-val curr)))
 
    ; Toon alle elementen in de ring vanaf het huidige element:
    (define (display ring)
      (define l (length ring))
      (let loop ((i 0))
        (if (< i l)
            (begin (io:display (peek ring)) (io:display ", ") (shift-forward! ring) (loop (+ i 1)))))
      (newline))
 
    (define myring (from-scheme-list (list 1 2 3 4)))
    (display myring)
    (add-before! myring 5)
    (display myring)
    (add-after! myring 6)
    (display myring)
    (shift-forward! myring)
    (display myring)
    (delete! myring)
    (display myring)))
