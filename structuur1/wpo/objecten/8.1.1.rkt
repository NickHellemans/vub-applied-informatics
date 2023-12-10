(define flip
  (let ((state 0))
    (lambda ()
    (if (= state 0)
        (set! state 1)
        (set! state 0))
    state)))

(define (make-flip)
    (let ((state 0))
    (lambda ()
    (if (= state 0)
        (set! state 1)
        (set! state 0))
    state)))

(define flip (make-flip))
(define flap1 (flip))
(define (flap2) (flip))
(define flap3 flip)
(define (flap4) flip)