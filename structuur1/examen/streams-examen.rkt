(load "streams.rkt")

(define (incr n) (if (>= n 12) 1 (+ n 1)))


(define integers (cons-stream 1 (map-stream incr integers)))
 
(define integers (cons-stream 1
                            (map-stream incr integers)))


(define (cut stream)
  (define (split head-stream rest e)
    (cond
      ((empty-stream? rest) (cons head-stream the-empty-stream))
      ((= e (head rest)) (split (cons-stream e head-stream) (tail rest) e))
      (else (cons head-stream rest))))
  (cond
    ((empty-stream? stream) the-empty-stream)
    (else
     (let ((tmp (split (head stream) (tail stream) (head stream))))
       (cons-stream (car tmp) (cut (cdr tmp)))))))

(define (merge s1 s2)
  (cond
    ((empty-stream? s1) s2)
    ((empty-stream? s2) s1)
    ((> (head s1) (head s2)) (merge s2 s1))
    (else (cons-stream (head s1) (merge (tail s1) s2)))))

(define (raam data n)
  (define (split head-stream rest n)
    (cond
      ((empty-stream? rest) (cons head-stream the-empty-stream))
      ((= n 1) (cons head-stream rest))
      (else (split (cons-stream (head rest) head-stream) (tail rest) (- n 1)))))

  (cond
    ((empty-stream? data) the-empty-stream)
    (else
     (let ((tmp (split the-empty-stream data n)))
       (cons-stream (car tmp) (raam (tail tmp) n))))))



(define (prune stream n)
  (define (helper stream ctr flag)
    (cond
      ((empty-stream? stream) the-empty-stream)
      ((= ctr 0) (helper stream n (not flag)))
      (flag (cons-stream (head stream) (helper (tail stream) (- ctr 1) flag)))
      (else (helper (tail stream) (- ctr 1) flag))))
  (helper stream n #t))


(define (split stream n)
  (define (splitter head-stream rest ctr)
    (cond
      ((empty-stream? rest) (cons head-stream the-empty-stream))
      ((= ctr 0) (cons head-stream rest))
      (else (splitter (cons-stream (head rest) head-stream) (tail rest) (- ctr 1)))))
  
  (cond
    ((empty-stream? stream) the-empty-stream)
    (else
     (let ((tmp (splitter the-empty-stream stream n)))
       (cons-stream (car tmp) (split (cdr tmp) n))))))


(define (daggemiddelden stream)
  (map-stream (lambda (x) (/ x 12))
  (map-stream
   (lambda (x) (accumulate + 0 x))
    (split (prune stream 12) 12))))

(define temperaturen (enumerate-interval 1 48))
(print-stream (daggemiddelden temperaturen))

(define (print-m matrix)
  (display "[")
  (print-stream (head matrix)) (newline)
  (stream-for-each (lambda (x) (display " ")
                     (print-stream x) (newline))
                   (tail matrix))
  (display "]\n"))