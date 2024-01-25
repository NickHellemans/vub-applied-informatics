(define (cut stream)
  (define (split head-stream stream e)
    (if (empty-stream? stream)
        (cons head-stream the-empty-stream)
        (if (not (= e (head stream)))
            (cons head-stream stream)
            (split (cons-stream e head-stream) (tail stream) e))))
 
  (if (empty-stream? stream)
      the-empty-stream
      (let ((temp (split the-empty-stream stream (head stream))))
        (cons-stream (car temp) (cut (cdr temp))))))