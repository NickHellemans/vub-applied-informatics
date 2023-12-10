#lang r7rs

(define-library ()
  (export maak-samenvatting)
  (import (scheme base)
          (only (racket base) equal-hash-code))

  (begin
    (define (maak-samenvatting t a b)
      (unless (string? t)
        (error "Het eerste argument van maak-samenvatting moet een value zijn van type string." t))
      
      (let ((n-t (string-length t)))
        (unless (and (number? a)
                     (number? b)
                     (>= a 0)
                     (<= a n-t)
                     (>= b a)
                     (<= b n-t))
          (error (string-append "Ongeldige regio in de string. Gelieve indices te geven binnen het interval [0, " (number->string n-t) "].") t a b)))
      
      (equal-hash-code (substring t a b)))))