(define (parent tree)   ...)
(define (children tree) ...)
 
(define (tree-proc tree ...)
  (cond ((test-parent (parent tree)) ...)
        (else (tree-proc-in (children tree) ...))))
 
(define (tree-proc-in lst ...)
  (cond ((null? lst) ...)
        (else (combine-res (tree-proc (car lst) ...)
                           (tree-proc-in (cdr lst) ...)))))

