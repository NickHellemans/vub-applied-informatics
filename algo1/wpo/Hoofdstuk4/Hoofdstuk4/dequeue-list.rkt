#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*     Double-Ended Queues (Positional List Implementation)        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2018  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (dequeue) ;; naam gewijzigd
  (export new queue? enqueue! serve! peek full? empty?
          enqueue-at-head! serve-at-rear! peek-at-rear show) ; nieuwe procedures toegevoegd in de export
  (import (prefix (a-d positional-list adt)  plist:) ; augmented-double-linked versie vereist indien je alles O(1) wilt krijgen
          (except (scheme base) length map for-each )
          (scheme write))
  (begin
   
    (define-record-type queue
      (make p)
      queue?
      (p plist))
 
    (define (new)
      (make (plist:new eq?)))
 
    (define (enqueue! q val)
      (define plst (plist q))
      (if (full? q)
          (error "full queue (enqueue!)" q)
          (plist:add-before! plst val)))
 
    (define (peek q)
      (define plst (plist q))
      (if (= (plist:length plst) 0)
          (error "empty queue (peek)" q))
      (plist:peek plst (plist:last plst)))
 
    (define (serve! q)
      (define plst (plist q))
      (define last-position (plist:last plst))
      (if (plist:empty? plst)
          (error "queue empty (pop)" q))
      (let ((val (plist:peek plst last-position)))
        (plist:delete! plst last-position)
        val))
 
    (define (empty? q)
      (plist:empty? (plist q)))
 
    (define (full? q)
      (plist:full? (plist q)))

    ; Nieuwe procedures beginnen hier

    (define (enqueue-at-head! q val)
      (define plst (plist q))
      (if (full? q)
          (error "full queue (enqueue-at-head!)" q)
          (plist:add-after! plst val)))

    (define (serve-at-rear! q)
      (define plst (plist q))
      (define first-position (plist:first plst))
      (if (plist:empty? plst)
          (error "queue empty (serve-at-rear!)" q))
      (let ((val (plist:peek plst first-position)))
        (plist:delete! plst first-position)
        val))

    (define (peek-at-rear q)
      (define plst (plist q))
      (if (= (plist:length plst) 0)
          (error "empty queue (peek-at-rear)" q))
      (plist:peek plst (plist:first plst)))

    (define (show q)
      (plist:for-each (plist q) (lambda (elt) (display elt) (display " "))))

    ))