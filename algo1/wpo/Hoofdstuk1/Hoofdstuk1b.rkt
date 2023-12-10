#lang r7rs
(import (scheme base)
        (scheme write)
        (prefix (Hoofdstuk1 fraction) fraction:)
        (prefix (Hoofdstuk1 disk) disk:)
        (scheme inexact)) ; inexact maths library, used for sqrt procedure

; Abstractie
; ----------

; VANAF NU GEBRUIKEN WE DE R7RS VARIANT VAN SCHEME.
; ZIE R7RS.RKT VOOR EEN UITGEBREIDE INTRODUCTIE OVER HET GEBRUIK VAN DEZE VARIANT!

; Oefening 3.
; Analogous to the complex ADT, let's define the fraction ADT.

; First, formulate the ADT itself, i.e., specify all procedures along with their procedure type.
; -> Zie fraction.rkt.

; Second, implement the ADT in the procedural style as a Scheme library.
; -> Zie fraction.rkt.

; Third, write a procedure = that uses the ADT in order to verify whether or not to fractions are equal.  You are not allowed to add = to your library.
; -> Mogelijke oplossing:
(define (fraction:= f1 f2)
  (= (/ (fraction:numerator f1) (fraction:denominator f1))
     (/ (fraction:numerator f2) (fraction:denominator f2))))
; -> Andere mogelijke oplossing:
;    (define (fraction:= f1 f2)
;      (let ((difference (fraction:- f1 f2)))
;        (= (fraction:numerator difference) 0)))

(define f1 (fraction:new 1 2)) (display f1) (newline)
(define f2 (fraction:new 2 4)) (display f2) (newline)
(define f3 (fraction:new 3 4)) (display f3) (newline)

(display (fraction:= f1 f1)) (newline)
(display (fraction:= f1 f2)) (newline)
(display (fraction:= f1 f3)) (newline)

; Fourth, reimplement the constructor such that rationals are always represented in reduced form.  Does this reimplementation affect your code for =?
; -> Zie fraction.rkt.  Deze herimplementatie beïnvloedt onze code voor = niet.

; Oefening 4.
; The software company KidSoft is creating a drawing program for children between 8 and 12 years old.
; One of the features of the program consists of creating colorful disks.
; We can think of a disk as a circle that is filled with a certain color.
; The circle can be thought of as a center (represented by two numbers that correspond to 2D-coordinates) and a radius.

; First, formulate the ADT disk.  Implement the constructor and the accessors in the procedural style.
; -> Zie disk.rkt.

; Second, implement (external to the ADT library) the following additional operations: concentric?, same-size?, same-color?, identical?.
(define (concentric? disk1 disk2)
  (point:= (disk:centre disk1) (disk:centre disk2)))

(define (point:= p1 p2)
  (and (= (disk:x-coordinate p1) (disk:x-coordinate p2))
       (= (disk:y-coordinate p1) (disk:y-coordinate p2))))

(define (same-size? disk1 disk2)
  (= (disk:radius disk1) (disk:radius disk2)))

(define (same-color? disk1 disk2)
  (equal? (disk:color disk1) (disk:color disk2)))

(define (identical? disk1 disk2)
  (and (concentric? disk1 disk2)
       (same-size? disk1 disk2)
       (same-color? disk1 disk2)))

; Third, implement the additional operations: subdisk?, intersects?, touches?.
(define (subdisk? disk1 disk2)
  (<= (distance (disk:centre disk1) (disk:centre disk2))
      (- (disk:radius disk2) (disk:radius disk1))))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (disk:x-coordinate p1) (disk:x-coordinate p2)))
           (sqr (- (disk:y-coordinate p1) (disk:y-coordinate p2))))))

(define (sqr x)
  (* x x))

(define (intersects? disk1 disk2)
  (and (< (distance (disk:centre disk1) (disk:centre disk2))
          (+ (disk:radius disk1) (disk:radius disk2)))
       (not (subdisk? disk1 disk2))
       (not (subdisk? disk2 disk1))))

(define (touches? disk1 disk2)
  (or (= (distance (disk:centre disk1) (disk:centre disk2))
         (+ (disk:radius disk1) (disk:radius disk2)))
      (= (distance (disk:centre disk1) (disk:centre disk2))
         (- (disk:radius disk1) (disk:radius disk2)))))

; Oefening 5.
; Consider the ADT dictionary<K,V> and suppose that we want to use an implementation of the ADT in the following applications.  Formally specify K and V for all cases.

; A dictionary Dutch-English that maps a Dutch word onto its only translation in English.
; -> dictionary<string,string>

; A dictionary Dutch-English that maps a Dutch word onto a series of possible translations in English.
; -> dictionary<string,pair>

; A list of students that associates a student's name with the number of credits he (or she) still has to collect in order to get a bachelor's degree.
; -> dictionary<string,number>

; A list of students that associates a student's name with the fact whether or not the student is male.
; -> dictionary<string,boolean>

; A list of students that associates a student with his or her study program.  The study program is a mapping that associates course names with the mark obtained by the student for that particular course.
; -> dictionary<student,dictionary<string,number>>
