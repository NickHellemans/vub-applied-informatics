(define (make-random m a seed)
  (let ((next seed))
    (define (generate)
      (set! next (modulo (* next a) m))
      (exact->inexact (/ next m)))
    
    (define (reset! new-seed)
      (set! next new-seed))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'generate) generate)
        ((eq? msg 'reset) reset!)))
    dispatch))
