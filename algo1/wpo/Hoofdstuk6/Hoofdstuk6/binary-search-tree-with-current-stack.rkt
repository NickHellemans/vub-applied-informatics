#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Binary Search Trees                        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (binary-search-tree-with-current)
  (export new bst? find insert! delete! empty? full? root
          set-current-to-first! set-current-to-next! get-current has-current?) ;; NIEUWE PROCEDURES
  (import (prefix (a-d tree binary-tree) tree:)
          (scheme base)
          (scheme write)
          (prefix (a-d stack linked) stack:)) ;; TOEGEVOEGD: we gebruiken een stack om een pad in de BST te onthouden.
  (begin 
    (define-record-type bst
      (make r e l c) ;; AANGEPAST: extra parameter
      bst?
      (r root root!)
      (e equality)
      (l lesser)
      (c current current!)) ;; TOEGEVOEGD

    (define (new ==? <<?)
      (make tree:null-tree ==? <<? (stack:new) )) ;; AANGEPAST: initialiseer de current met een lege stack
 
    (define (find bst key)
      (define <<? (lesser bst))
      (define ==? (equality bst))
      (let find-key 
        ((node (root bst)))
        (if (tree:null-tree? node)
            #f
            (let 
                ((node-value (tree:value node)))
              (cond 
                ((==? node-value key)
                 node-value)
                ((<<? node-value key)
                 (find-key (tree:right node)))
                ((<<? key node-value)
                 (find-key (tree:left node))))))))
 
    (define (insert! bst val)
      (define <<? (lesser bst))
      (let insert-iter 
        ((parent tree:null-tree)
         (child! (lambda (ignore child) (root! bst child)))
         (child (root bst)))
        (cond 
          ((tree:null-tree? child)
           (child! parent 
                   (tree:new val 
                             tree:null-tree 
                             tree:null-tree)))
          ((<<? (tree:value child) val)
           (insert-iter child tree:right! 
                        (tree:right child)))
          ((<<? val (tree:value child))
           (insert-iter child tree:left!
                        (tree:left child)))
          (else 
           (tree:value! child val)))))
 
    (define (delete! bst val)
      (define <<? (lesser bst))
      (define ==? (equality bst))
      (define (find-leftmost deleted parent child! child)
        (if (tree:null-tree? (tree:left child))
            (begin 
              (tree:value! deleted (tree:value child))
              (child! parent (tree:right child)))
            (find-leftmost deleted child 
                           tree:left!
                           (tree:left child))))
      (define (delete-node parent child! child)
        (cond 
          ((tree:null-tree? (tree:left child))
           (child! parent (tree:right child)))
          ((tree:null-tree? (tree:right child))
           (child! parent (tree:left child)))
          (else
           (find-leftmost child
                          child 
                          tree:right! 
                          (tree:right child)))))
      (let find-node
        ((parent tree:null-tree)
         (child! (lambda (ignore child) (root! bst child)))
         (child (root bst)))
        (cond 
          ((tree:null-tree? child)
           #f)
          ((==? (tree:value child) val)
           (delete-node parent child! child)
           (tree:value child))
          ((<<? (tree:value child) val)
           (find-node child tree:right! (tree:right child)))
          ((<<? val (tree:value child))
           (find-node child tree:left! (tree:left child))))))
 
    (define (empty? bst)
      (tree:null-tree? (root bst)))
 
    (define (full? bst)
      #f)

    ;;; Nieuwe procedures

    (define (has-current? bst)
      (not (stack:empty? (current bst))))
    
    (define (loop-down bst) ; overgenomen uit de implementatie van iterative-in-order (zie binary-tree-algorithms.rkt)
      (define stack (current bst))
      (let ((node (stack:top stack)))
        (when (not (tree:null-tree? (tree:left node))) ;; AANGEPAST: when-expressie ipv if-expressie (geen alternative tak meer)
          (stack:push! stack (tree:left node))
          (loop-down bst))))

    (define (set-current-to-first! bst) ; Gebaseerd op deel van iterative-in-order (zie binary-tree-algorithms.rkt)
      ;; Eerst, er voor zorgen dat de current "leeg" is
      (current! bst (stack:new))
      
      ;; Vervolgens, zo links mogelijk afdalen vanaf de root, onthou het pad dat je bent afgedaald in de current
      (when (not (tree:null-tree? (root bst))) ;; Check of er elementen zijn
        (stack:push! (current bst) (root bst)) 
        (loop-down bst)))

    (define (set-current-to-next! bst) ; Gebaseerd deel van iterative-in-order (zie binary-tree-algorithms.rkt)
      (define stack (current bst))
      (define (loop-up)
        (let ((node (stack:pop! stack)))                   ;; De huidige node waar we ons bevinden wordt van de stack gehaald
          (if (not (tree:null-tree? (tree:right node)))    ;; Kunnen we rechts afdalen?
              (begin (stack:push! stack (tree:right node)) ;; Push dan het rechterkind op stack
                     (loop-down bst))                      ;; Daal in die deelboom zo links mogelijk af
              (if (and (not (stack:empty? stack))          ;; Indien we niet rechts kunnen afdalen, klim verder naar boven tot we zien dat we niet uit een rechterdeelboom komen 
                       (not (eq? node (tree:left (stack:top stack))))) ;; stack:top bevat nu de parent-knoop omdat node weggepopt is van de stack
                  (loop-up)))))
      (if (has-current? bst)
          (loop-up)
          (error "The BST does not have a current!")))

    (define (get-current bst) ; geeft de waarde van de huidige knoop terug
      (if (has-current? bst)
          (tree:value (stack:top (current bst)))
          (error "The BST does not have a current!")))))