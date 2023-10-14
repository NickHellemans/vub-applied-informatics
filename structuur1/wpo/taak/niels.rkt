(#%require racket/trace)
(define (golomb n)
    (if 
        (= n 1) 
        1
        (+ 1 (golomb (- n (golomb (golomb (- n 1))))))
    )
)

(define (golomb-reeks n)
    (define (helper counter) 
        (display (golomb counter))
        (iter (+ counter 1))
    )

    (define (iter counter)
        (if 
            (<= counter n)
            (helper counter)
        )
    )

    (iter 1)
)
