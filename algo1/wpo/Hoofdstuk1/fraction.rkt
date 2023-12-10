#lang r7rs

; ADT fraction
;
;   new         (number number -> fraction)
;   fraction?   (any -> boolean)
;   numerator   (fraction -> number)
;   denominator (fraction -> number)
;   +           (fraction fraction -> fraction)
;   -           (fraction fraction -> fraction)
;   *           (fraction fraction -> fraction)
;   /           (fraction fraction -> fraction)
;   display     (fraction -> void)

(define-library 
  (fraction)
  (export new fraction? numerator denominator + - * / display)
  (import 
   (prefix (scheme write) io:)   ; use a prefix for all functions in a library...
   (rename (scheme base)         ; or rename some functions in a library...
           (numerator base:numerator)
           (denominator base:denominator)
           (+ base:+)
           (* base:*)
           (/ base:/)
           (- base:-))
   (scheme cxr)) ; CxR library only exports procedures for various "car" and "cdr" combinations (such as "caddr" which we use)
  (begin
    (define fraction-tag 'fraction)

    ;; original new that does not convert rationals to reduced form
    ;(define (new num den)
    ;  (list fraction-tag num den))

    (define (gcd-euclides a b)
      (if (= b 0)
          a
          (gcd-euclides b (modulo a b))))

    ; new that converts rationals to reduced form
    (define (new num den)
      (let ((gcd (gcd-euclides num den)))
        (list fraction-tag (base:/ num gcd) (base:/ den gcd))))

    (define (fraction? any)
      (and (pair? any)
           (eq? (car any) fraction-tag)))

    (define (numerator f)
      (cadr f))

    (define (denominator f)
      (caddr f))

    (define (+ f1 f2)
      (new (base:+ (base:* (numerator f1) (denominator f2)) (base:* (numerator f2) (denominator f1))) 
           (base:* (denominator f1) (denominator f2))))
 
    (define (- f1 f2)
      (new (base:- (base:* (numerator f1) (denominator f2)) (base:* (numerator f2) (denominator f1))) 
           (base:* (denominator f1) (denominator f2))))
 
    (define (* f1 f2)
      (let ((numerator-mul (base:* (numerator f1) (numerator f2)))
            (denominator-mul (base:* (denominator f1) (denominator f2))))
        (new numerator-mul denominator-mul)))
 
    (define (/ f1 f2)
      (let ((numerator-div (base:* (numerator f1) (denominator f2)))
            (denominator-div (base:* (denominator f1) (numerator f2))))
        (new numerator-div denominator-div)))
 
    (define (display f)
      (io:display (numerator f))
      (io:display " / ")
      (io:display (denominator f))
      (io:display "\n"))
    ))
