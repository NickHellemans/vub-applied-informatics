#lang r7rs

(define-library
  (avl-tree)
  (export new bst? null-tree null-tree?
          empty? full? insert! delete! find
          left left! right right! balance balance! value value!
          root root! check backtracking-check)
  (import (scheme base)
          (scheme write))
  (begin
 
    (define balanced 'balanced)
    (define Lhigh 'Lhigh)
    (define Rhigh 'Rhigh)
 
    (define-record-type AVL-node
      (make-AVL-node v l r b)
      AVL-node?
      (v value value!)
      (l left left!)
      (r right right!)
      (b balance balance!))
 
    (define null-tree '())
 
    (define (null-tree? node)
      (eq? node null-tree))
 
    (define-record-type bst
      (make r e l)
      bst?
      (r root root!)
      (e equality)
      (l lesser))
 
    (define (new ==? <<?)
      (make null-tree ==? <<?))
 
    (define (single-rotate-left! black)
      (define white (right black))
      (define tree (left white))
      (right! black tree)
      (left! white black)
      white)
    
    (define (single-rotate-right! black)
      (define white (left black))
      (define tree (right white))
      (left! black tree)
      (right! white black)
      white)
    
    (define (double-rotate-left-then-right! black)
      (define white (left black))
      (left! black (single-rotate-left! white))
      (single-rotate-right! black))
    
    (define (double-rotate-right-then-left! black)
      (define white (right black))
      (right! black (single-rotate-right! white))
      (single-rotate-left! black))
 
    (define (single-rotate-left-update! black white)
      (cond ((eq? (balance white) Rhigh)
             (balance! black balanced)
             (balance! white balanced))
            (else
             (balance! black Rhigh)
             (balance! white Lhigh))))
 
    (define (single-rotate-right-update! black white)
      (cond ((eq? (balance white) Lhigh)
             (balance! black balanced)
             (balance! white balanced))
            (else
             (balance! black Lhigh)
             (balance! white Rhigh))))
 
    (define (double-rotate-right-then-left-update! black white grey)
      (cond ((eq? (balance grey) Lhigh)
             (balance! white Rhigh)
             (balance! black balanced)
             (balance! grey balanced))
            ((eq? (balance grey) balanced)
             (balance! white balanced)
             (balance! black balanced)
             (balance! grey balanced))
            (else
             (balance! white balanced)
             (balance! black Lhigh)
             (balance! grey balanced))))
 
    (define (double-rotate-left-then-right-update! black white grey)
      (cond ((eq? (balance grey) Rhigh)
             (balance! white Lhigh)
             (balance! black balanced)
             (balance! grey balanced))
            ((eq? (balance grey) balanced)
             (balance! white balanced)
             (balance! black balanced)
             (balance! grey balanced))
            (else
             (balance! white balanced)
             (balance! black Rhigh)
             (balance! grey balanced))))
 
    (define (check-after-insert-left parent child! black)
      (cond 
        ((eq? (balance black) Rhigh)
         (balance! black balanced)
         #f)
        ((eq? (balance black) balanced)
         (balance! black Lhigh)
         #t)
        (else ; child already was left-high
         (let* ((white (left black))
                (grey  (right white))) 
           (if (eq? (balance white) Lhigh)
               (begin
                 (child! parent (single-rotate-right! black))
                 (single-rotate-right-update! black white))
               (begin 
                 (child! parent (double-rotate-left-then-right! black))
                 (double-rotate-left-then-right-update! black white grey)))
           #f))))
 
    (define (check-after-insert-right parent child! black)
      (cond
        ((eq? (balance black) Lhigh)
         (balance! black balanced)
         #f)
        ((eq? (balance black) balanced)
         (balance! black Rhigh)
         #t)
        (else ; child already was right-high
         (let* ((white (right black))
                (grey  (left white)))
           (if (eq? (balance white) Rhigh)
               (begin
                 (child! parent (single-rotate-left! black))
                 (single-rotate-left-update! black white))
               (begin 
                 (child! parent (double-rotate-right-then-left! black))
                 (double-rotate-right-then-left-update! black white grey)))
           #f))))
 
    (define (check-after-delete-left parent child! black)
      (cond 
        ((eq? (balance black) Lhigh)
         (balance! black balanced) 
         #t)
        ((eq? (balance black) balanced)
         (balance! black Rhigh) 
         #f)
        (else ; right-high
         (let* ((white (right black)) 
                (white-bal (balance white))
                (grey (left white)))
           (if (or (eq? white-bal balanced)
                   (eq? white-bal Rhigh))
               (begin 
                 (child! parent (single-rotate-left! black))
                 (single-rotate-left-update! black white))
               (begin
                 (child! parent (double-rotate-right-then-left! black))
                 (double-rotate-right-then-left-update! black white grey)))
           (not (eq? white-bal balanced))))))
 
    (define (check-after-delete-right parent child! black)
      (cond 
        ((eq? (balance black) Rhigh)
         (balance! black balanced)
         #t)
        ((eq? (balance black) balanced)
         (balance! black Lhigh) 
         #f)
        (else
         (let* ((white (left black)) 
                (white-bal (balance white))
                (grey (right white)))
           (if (or (eq? white-bal Lhigh)
                   (eq? white-bal balanced))
               (begin
                 (child! parent (single-rotate-right! black))
                 (single-rotate-right-update! black white))
               (begin
                 (child! parent (double-rotate-left-then-right! black))
                 (double-rotate-left-then-right-update! black white grey)))
           (not (eq? white-bal balanced))))))
 
    (define (empty? avl)
      (null-tree? (root avl)))
 
    (define (full? avl)
      #f)
 
    (define (insert! avl val)
      (define <<? (lesser avl))
      (define ==? (equality avl))
   
      (let insert-rec
        ((parent null-tree)
         (child! (lambda (ignore child) (root! avl child)))
         (child (root avl)))
        (cond
          ((null-tree? child)
           (child! parent (make-AVL-node val null-tree null-tree balanced))
           #t)
          ((<<? (value child) val)
           (if (insert-rec child right! (right child))
               (check-after-insert-right parent child! child)
               #f))
          ((<<? val (value child))
           (if (insert-rec child left! (left child))
               (check-after-insert-left parent child! child) 
               #f))
          (else  ; value = (AVL-node-value node)
           (value! child val)
           #f)))
      avl)
 
    (define (find avl key)
      (define <<? (lesser avl))
      (define ==? (equality avl))
      (let find-value 
        ((node (root avl)))
        (if (null-tree? node)
            #f
            (let ((node-val (value node)))
              (cond
                ((==? (value node) key)
                 node-val)
                ((<<? key node-val)
                 (find-value (left node)))
                ((<<? node-val key)
                 (find-value (right node)))
                (else
                 #f))))))
 
    (define (delete! avl val)
      (define ==? (equality avl))
      (define <<? (lesser avl))
   
      (define (find-leftmost deleted parent child! child)
        (if (null-tree? (left child))
            (begin
              (value! deleted (value child))
              (child! parent (right child))
              #t)
            (if (find-leftmost deleted child left! (left child))
                (check-after-delete-left parent child! child)
                #f)))
   
      (define (delete-node parent child! child)
        (cond 
          ((null-tree? (left child))
           (child! parent (right child))
           #t)
          ((null-tree? (right child))
           (child! parent (left child))
           #t)
          (else
           (if (find-leftmost child child right! (right child))
               (check-after-delete-right parent child! child)
               #f))))
   
      (let find-node
        ((parent null-tree)
         (child! (lambda (ignore child) (root! avl child)))
         (child (root avl)))
        (cond
          ((null-tree? child)
           #f)
          ((==? (value child) val)
           (delete-node parent child! child))
          ((<<? (value child) val)
           (if (find-node child right! (right child))
               (check-after-delete-right parent child! child)
               #f))
          ((<<? val (value child))
           (if (find-node child  left! (left child))
               (check-after-delete-left parent child! child)
               #f))))
      avl)
 
    ;; Exercise 11
 
    ; Variant 1: Simplest to program, but descends down the tree several times
 
    (define (min-tree avltree)
      (define (leftmost node)
        (if (null-tree? (left node))
            (value node)
            (leftmost (left node))))
      (leftmost (root avltree)))
 
    (define (max-tree avltree)
      (define (rightmost node)
        (if (null-tree? (right node))
            (value node)
            (rightmost (right node))))
      (rightmost (root avltree)))
 
    (define (check-values avltree) ; Checks the BST condition
      (let ((minAVLtree (min-tree avltree))
            (maxAVLtree (max-tree avltree)))
        (define (checktree node)
          (if (null-tree? node)
              #t
              (let* ((left-check (checktree (left node)))
                     (right-check (checktree (right node)))
                     (Lv (if (not (null-tree? (left node)))
                             (value (left node))
                             minAVLtree))
                     (Rv (if (not (null-tree? (right node)))
                             (value (right node))
                             maxAVLtree)))
                (and (<= Lv (value node))
                     (<= (value node) Rv)
                     left-check
                     right-check))))
        (checktree (root avltree))))
 
    (define (check-heights avltree) ; Checks the AVL condition.  Actually, we should check the balance information as well...
      (define (height node)
        (cond ((null-tree? node) ; The node is the empty tree
               0)
              ((and (null-tree? (left node))
                    (null-tree? (right node))) ; The node is a leaf
               1)
              (else
               (+ 1 (max (height (left node))
                         (height (right node)))))))
      (define (check-node-heights node)
        (or (null-tree? node)
            (let ((left-height (height (left node)))
                  (right-height (height (right node))))
              (and (or (= left-height right-height)
                       (= left-height (- right-height 1))
                       (= left-height (+ right-height 1)))
                   (check-node-heights (left node))
                   (check-node-heights (right node))))))
      (check-node-heights (root avltree)))
 
    (define (check avltree)
      (and (check-values avltree)
           (check-heights avltree)))
 
    ; Variant 2: More difficult to program, but only descends down the tree a single time.
    ; The BST condition, AVL condition, and balance information are checked while backtracking upwards.
 
    (define (backtracking-check avl-tree)
      (define == (equality avl-tree))
      (define << (lesser avl-tree))
      (define-record-type loop-result
        (make-loop-result v s l h c)
        loop-result?
        (v loop-result-value)
        (s loop-result-smallest)
        (l loop-result-largest)
        (h loop-result-height)
        (c loop-result-check))
      (define (leaf? node)
        (and (not (null-tree? node))
             (null-tree? (left node))
             (null-tree? (right node))))
      (define (check-avl-condition-and-balance-information node left-height right-height)
        (or (and (eq? (balance node) balanced) (= left-height right-height))
            (and (eq? (balance node) Lhigh) (= left-height (+ right-height 1)))
            (and (eq? (balance node) Rhigh) (= left-height (- right-height 1)))))
      (define (loop node)
        (display "Recursion is currently entering ") (display (value node)) (newline)
        (if (leaf? node)
            (begin (display "Recursion is currently leaving ") (display (value node)) (newline)
                   (make-loop-result (value node) (value node) (value node) 1 (eq? (balance node) balanced))) ; For leafs, the check only returns false if the balance information is incorrect
            (cond ((null-tree? (left node))
                   (let ((right-result (loop (right node)))) ; The recursion will descend down the tree, the body of this let will be executed while backtracking upwards
                     (display "Recursion is currently leaving ") (display (value node)) (newline)
                     (make-loop-result (value node)
                                       (value node)
                                       (loop-result-largest right-result)
                                       (+ 1 (loop-result-height right-result))
                                       (and (loop-result-check right-result) ; Check whether the check succeeded for our children
                                            (<< (value node) (loop-result-smallest right-result)) ; Check the BST condition
                                            (= 1 (loop-result-height right-result)) ; Check the AVL condition
                                            (eq? (balance node) Rhigh))))) ; Check the balance information
                  ((null-tree? (right node))
                   (let ((left-result (loop (left node)))) ; The recursion will descend down the tree, the body of this let will be executed while backtracking upwards
                     (display "Recursion is currently leaving ") (display (value node)) (newline)
                     (make-loop-result (value node)
                                       (loop-result-smallest left-result)
                                       (value node)
                                       (+ 1 (loop-result-height left-result))
                                       (and (loop-result-check left-result) ; Check whether the check succeeded for our children
                                            (<< (loop-result-largest left-result) (value node)) ; Check the BST condition
                                            (= 1 (loop-result-height left-result)) ; Check the AVL condition
                                            (eq? (balance node) Lhigh))))) ; Check the balance information
                  (else
                   (let ((left-result (loop (left node)))
                         (right-result (loop (right node)))) ; The recursion will descend down the tree, the body of this let will be executed while backtracking upwards
                     (display "Recursion is currently leaving ") (display (value node)) (newline)
                     (make-loop-result (value node)
                                       (loop-result-smallest left-result)
                                       (loop-result-largest right-result)
                                       (+ 1 (max (loop-result-height left-result) (loop-result-height right-result)))
                                       (and (loop-result-check left-result) (loop-result-check right-result) ; Check whether the check succeeded for our children
                                            (<< (loop-result-largest left-result) (value node)) (<< (value node) (loop-result-smallest right-result)) ; Check the BST condition
                                            (check-avl-condition-and-balance-information node (loop-result-height left-result) (loop-result-height right-result))))))))) ; Check the AVL condition and balance information
      (loop-result-check (loop (root avl-tree))))))



;; checks ingebouwd in deze aanpassing van het AVL ADT 
(define my-avl-tree (new = <))
(insert! my-avl-tree 1)
(insert! my-avl-tree 2)
(insert! my-avl-tree 3)
(insert! my-avl-tree 4)
(insert! my-avl-tree 5)
(insert! my-avl-tree 6)
(insert! my-avl-tree 7)
(insert! my-avl-tree 8)
(insert! my-avl-tree 0)
(display my-avl-tree) (newline) (newline)
(display (check my-avl-tree)) (newline) (newline)
(display (backtracking-check my-avl-tree)) (newline)
