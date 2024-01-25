#lang r7rs

(import (except (scheme base) map)
        (scheme write))

;  CHAPTER 1: INTRODUCTION
; -------------------------

;  Section 1.1: Terminology
; --------------------------

; Exercise 1

; Specify the procedural type of the following built-in Scheme procedures: cons,
; car, cdr, vector-ref, vector-set!, member.  You can use the following data
; types: any, pair, vector, number, boolean and void.  You can also use
; singleton sets such as {#f}.
; -> The procedural type of cons        is (any any -> pair)
; -> The procedural type of car         is (pair -> any)
; -> The procedural type of cdr         is (pair -> any)
; -> The procedural type of vector-ref  is (vector number -> any)
; -> The procedural type of vector-set! is (vector number any -> ∅)
; -> The procedural type of member      is (any pair -> pair U {#f})

; Exercise 2

; Specify the procedural type of the following higher-order procedures.  You can
; use the same data types as in the previous exercise.

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))
;
; -> The procedural type of (map f l)
;             is ((any -> any) pair -> pair)

(define (sum term next a b)
  (if (< a b)
      (+ (term a)
         (sum term next (next a) b))
      0))
;
; -> The procedural type of (sum term next a b)
;             is ((number -> number) (number -> number) number number -> number)

(define (compose f g)
  (lambda (x)
    (f (g x))))
;
; -> The procedural type of (compose f g)
;             is ((any -> any) (any -> any) -> (any -> any))

;  Section 1.3: Performance
; --------------------------

; Exercise 6

(define (last-of-list lst)
  (cond ((null? lst)
         '())
        ((null? (cdr lst))
         (car lst))
        (else
         (last-of-list (cdr lst)))))

(define (last-of-vector vctr)
  (let ((length (vector-length vctr)))
    (if (< length 1)
        '()
        (vector-ref vctr (- length 1)))))

; Worst case performance:
; - last-of-list: 
;   O(n), where n is the size of the list
;   We have to walk until the end of the list to obtain the last element.
; - last-of-vector:
;   O(1)
;   Since vectors have a fixed length, we can use the length of the vector to
;   access the last element in O(1).
; Best case performance:
; - last-of-list: 
;   O(n)
;   For any arbitrary input of size n, you'll always have to pass through the whole list.
;   Stating that this could be O(1) when the list has size 1 or 0 is therefore incorrect.
; - last-of-vector:
;   O(1)
;   Cf. worst case performance.
; Average case performance:
; - last-of-list:
;   O(n)
;   Cf. worst case performance.
; - last-of-vector:
;   O(1)
;   Cf. worst case performance.

; Exercise 7 - Iterative implementation of length-of-list

(define (length-of-list lst)
  (define (iter-length count rest-of-lst)
    (cond ((null? rest-of-lst)
         count)
        (else
         (iter-length (+ count 1) (cdr rest-of-lst)))))
  (iter-length 0 lst))

(define (length-of-list-rec lst)
  (if (null? lst)
      0
      (+ 1 (length-of-list (cdr lst)))))

(define (length-of-vector vctr)
  (vector-length vctr))

; Worst case performance:
; - length-of-list: 
;   O(n), where n is the size of the list
;   We have to walk until the end of the list to count all the elements.
; - length-of-vector:
;   O(1)
;   Since vectors have a fixed length, we can simply obtain the length of the
;   vector in O(1).
; Best case performance:
; - length-of-list: 
;   O(n)
;   For any arbitrary input of size n, you'll always have to pass through the whole list.
;   Stating that this could be O(1) when the list has size 1 or 0 is therefore incorrect.
; - length-of-vector:
;   O(1)
;   Cf. worst case performance.
; Average case performance:
; - length-of-list:
;   O(n)
;   Cf. worst case performance.
; - length-of-vector:
;   O(1)
;   Cf. worst case performance.

; Exercise 8
(define (all-but-first-n l n)
  (define (iterate current counter)
    (if (or (= counter 0)
            (null? current))
        current
        (iterate (cdr current) (- counter 1))))
  (iterate l n))

; Exercise 9

; subtract: O(m)
; zip:      O(min(length(l1), length(l2)))

; Exercise 10

; i-to-j:     O(j)
; sum:        i * (i-to-j i i) = i * O(i) = O(i^2)
; all-i-to-j: (sum n) = O(n^2)
