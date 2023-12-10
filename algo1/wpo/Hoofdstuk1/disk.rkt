#lang r7rs

(define-library 
  (disk)
  (export new disk? centre radius color new-point point? x-coordinate y-coordinate)
  (import (scheme base)
          (scheme cxr))
 
  (begin
    (define disk-tag 'disk)
 
    (define (new centre radius color)
      (list disk-tag centre radius color))
 
    (define (disk? any)
      (and (pair? any)
           (eq? (car any) disk-tag)))
 
    (define (centre disk)
      (cadr disk))
 
    (define (radius disk)
      (caddr disk))
 
    (define (color disk)
      (cadddr disk))
 
    (define point-tag 'point)
 
    (define (new-point x y)
      (list point-tag x y))
 
    (define (point? any)
      (and (pair? any)
           (eq? (car any) point-tag)))
 
    (define (x-coordinate point)
      (cadr point))
 
    (define (y-coordinate point)
      (caddr point))
    ))
