(define VUBOrganigram
  '(VUB (academisch (rectoraat)
                    (faculteiten
                     (rechten (bachelor (ba-rechten)
                                        (ba-criminologie))
                              (master (ma-rechten)
                                      (ma-criminologie)))
                     (economie)
                     (wetenschappen (bachelor (ba-wiskunde)
                                              (ba-fysica)
                                              (ba-cw))
                                    (master (ma-wiskunde)
                                            (ma-fysica)
                                            (ma-cw)))))
        (administratief (personeel) (financien))))

(define (display-n n d)
  (cond ((> n 0) (display d)
                 (display-n (- n 1) d))))
 
(define (print-lijn aantalblanco tekst)
  (display-n aantalblanco " ")
  (display tekst)
  (newline))

;7.13.1
(define (print-vanaf organigram label)
  (define (find-label label organigram)
    (cond
      ((eq? label (car organigram)) (print-organigram organigram -1))
      (else (find-label-in label (cdr organigram)))))
  
  (define (find-label-in label organigram)
    (cond
      ((null? organigram) #f)
      (else (or (find-label label (car organigram))
                (find-label-in label (cdr organigram))))))
  
  (define (atom? el) (not (pair? el)))
  (define (print-organigram organigram ctr)
    (cond
      ((null? organigram) (display ""))
      ((atom? organigram) (print-lijn ctr organigram))
      (else (print-organigram (car organigram) (+ ctr 1))
            (print-organigram (cdr organigram) ctr))))
  (find-label label organigram))

  