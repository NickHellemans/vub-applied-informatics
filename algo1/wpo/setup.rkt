#lang r7rs

;Import libaries
(import (scheme base)
        (scheme write))

;Define libaries
 (define-library ()
   (export f)
   (import (scheme base)
           (scheme write))
   (begin
     (define (f x) x)))

;Import rename for name clashes
 (import (prefix (scheme base) base:)
         (a-d pattern-matching brute-force))
