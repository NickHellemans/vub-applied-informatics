#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Double-Ended Queues (Circular Vector)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (dequeue) ;; naam gewijzigd
  (export new queue? enqueue! serve! peek full? empty?
          enqueue-at-head! serve-at-rear! peek-at-rear show) ; nieuwe procedures toegevoegd in de export
  (import (scheme base)
          (scheme write))
  (begin
   
    (define default-size 5)
    (define-record-type queue
      (make s h r)
      queue?
      (s storage)
      (h head head!)
      (r rear rear!))
 
    (define (new)
      (make (make-vector default-size) 0 0))
 
    (define (empty? q)
      (= (head q)
         (rear q)))
 
    (define (full? q)
      (= (remainder (+ (rear q) 1) default-size)
         (head q)))
 
    (define (enqueue! q val)
      (if (full? q)
          (error "full queue (enqueue!)" q))
      (let ((new-rear (remainder (+ (rear q) 1) default-size)))
        (vector-set! (storage q) (rear q) val)
        (rear! q new-rear))
      q)
 
    (define (peek q)
      (if (empty? q)
          (error "empty queue (peek)" q))
      (vector-ref (storage q) (head q)))
 
    (define (serve! q)
      (if (empty? q)
          (error "empty queue (peek)" q))
      (let ((result (vector-ref (storage q) (head q))))
        (head! q (remainder (+ (head q) 1) default-size))
        result))

    ; Toegevoede procedures beginnen hier
    
    (define (enqueue-at-head! q val)
      (if (full? q)
          (error "full queue (enqueue-at-head!)" q))
      (let ((new-head (modulo (- (head q) 1) default-size)))
        (vector-set! (storage q) new-head val)
        (head! q new-head))
      q)

    (define (serve-at-rear! q)
      (if (empty? q)
          (error "empty queue (serve-at-rear!)" q))
      (let* ((new-rear (modulo (- (rear q) 1) default-size))
             (result (vector-ref (storage q) new-rear)))
        (rear! q new-rear)
        result))
    
    (define (peek-at-rear q)
      (if (empty? q)
          (error "empty queue (peek-at-rear)" q))
      (vector-ref (storage q) (modulo (- (rear q) 1) default-size)))
    
    (define (show q)
      (let loop ((i (head q)))
        (if (not (= i (rear q)))
            (begin (display (vector-ref (storage q) i)) (display " ") (loop (modulo (+ i 1) default-size))))))))