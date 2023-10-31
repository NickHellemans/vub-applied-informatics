;1
(define (aantal n)
  (if (= n 0)
      0
      (+ n (aantal (- n 1)))))

(define (aantal-iter n)
  (define (iter n res)
    (cond
      ((= n 0) res)
      (else (iter (- n 1) (+ n res)))))
  (iter n 0))

;2
(define (tel lst a b)
  (cond
    ((null? lst) 0)
    ((and (> (car lst) a) (< (car lst) b)) (+ 1 (tel (cdr lst) a b)))
    (else (tel (cdr lst) a b))))


(define (tel-pred lst pred)
  (cond
    ((null? lst) 0)
    ((pred (car lst)) (+ 1 (tel-pred (cdr lst) pred)))
    (else (tel-pred (cdr lst) pred))))

(define (tel-with-pred lst a b)
  (tel-pred lst (lambda (x) (and (> x a) (< x b)))))

;3
(define (vorm-koppels lst)
  (if (null? lst)
      '()
  (cons (list (car lst) (cadr lst)) (vorm-koppels (cddr lst)))))

(define (vorm-lijstjes lst n)
  (define (car-n lst n)
    (cond
      ((null? lst) '())
      ((= n 0) '())
      (else (cons (car lst) (car-n (cdr lst) (- n 1))))))
  (define (cdr-n lst n)
    (cond
      ((null? lst) '())
      ((= n 0) lst)
      (else (cdr-n (cdr lst) (- n 1)))))
  (if (null? lst)
      '()
      ;List moet hier eigenlijk niet (zorgt voor extra haakjes), wel gedaan op tentamen
      (cons (list (car-n lst n)) (vorm-lijstjes (cdr-n lst n) n))))

;4
(define (f x y)
    (let (
            (x (+ x 1))
            (y (+ x 2))
        )
        (+ x y (let* (
                (x (+ x 3))
                (x (+ x 4))
                (y (+ x 5))
            )
            (* x y)
        ))
    )
)
;(f 10 20) = 437 --> Op tentamen 434, kleine rekenfout ergens wss met een 3 te vergeten bij hoofdrekenen

;5
;Benaderingsmethode: midden van een lijnstuk vinden 