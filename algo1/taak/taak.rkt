#lang r7rs

(define-library ()
  (export match)
  (import (scheme base)
          (scheme write)
          (taak samenvatting))

  (begin
     (define (match t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
      (define p-samenvatting (maak-samenvatting p 0 n-p))
      (let loop
        ((i-t 0)
         (i-p 0))
        (cond
          ((> i-p (- n-p 1))
           i-t)
          ((> i-t (- n-t n-p))
           #f)
          ((not (eq? (maak-samenvatting t i-t (+ i-t n-p)) p-samenvatting))
           (loop (+ i-t 1) 0))
          ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
           (loop i-t (+ i-p 1)))
          (else
           (loop (+ i-t 1) 0)))))

    ; voorbeeld oproepen van "maak-samenvatting"
    (display (maak-samenvatting "aaab" 0 4))
    (newline)
    (display (maak-samenvatting "aaaaaaaaab" 0 4))
    (newline)
    (display (maak-samenvatting "aaaaaaaaab" 6 10))
    (newline)

    ))