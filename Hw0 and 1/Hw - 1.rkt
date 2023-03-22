; Hansi Seitaj, CMPSC 460, Hw 1
#lang plait

; Tree data structure
(define-type Tree
    (leaf [val : Number])
    (node [val : Number]
          [left : Tree]
          [right : Tree]))

; sum method returns the sum of all the values 
(define (sum [t : Tree]) : Number
  (type-case Tree t
    [(leaf n) n]
    [(node v l r) (+ v
                     (+ (sum l)
                        (sum r)))]))

(test (sum (node 5 (leaf 6) (leaf 7)))
      18)
(test (sum (leaf 0))
      0)
(test (sum (leaf -6)) 
      -6)


; negetes the values of a tree
(define (negate [t : Tree]): Tree
  (type-case Tree t
    [(leaf n) (leaf (- 0 n))]
    [ (node v l r)
          (node (- 0 v) (negate l) (negate r))]))


(test (negate (node 5 (leaf 6) (leaf 7)))
        (node -5 (leaf -6) (leaf -7)))
  (test (negate (leaf 5))
        (leaf -5))


; finds if the value provided is part of the tree leaf-values
(define (contains? [t : Tree] [n : Number]): Boolean
   (type-case Tree t
     [(leaf v) (eq? v n)]
     [(node v l r) (or (eq? v n)
                       (or (contains? l n) (contains? r n)))]))

(test (contains? (node 5 (leaf 6) (leaf 7)) 6)
      #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 10)
      #f)

; Takes a tree and returns #t if every leaf 
; is bigger than the sum of numbers in the path of nodes from the root that reaches 
; the leaf
(define (bigger-leaves? [t : Tree] [n : Number]): Boolean
  (type-case Tree t
    [(leaf v) (> v n)]
    [(node v l r) (and (bigger-leaves? l (+ v n))
                       (bigger-leaves? r (+ v n)))]))

(define (big-leaves? [t : Tree]) : Boolean
  (bigger-leaves? t 0))

(test (big-leaves? (node 5 (leaf 6) (leaf 7)))
      #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7)))
      #f)

; Method that returns a Boolean if all the trees in the list hold a positive sum
(define (positive-trees? [lst : (Listof Tree)]) : Boolean
  (cond 
  [(empty? lst) #t]
  [(cons? lst) (and ( <= 0 ( sum (first lst )))
                   (positive-trees? (rest lst)))]))

(test (positive-trees? (cons (leaf 6) empty))
 #t)
(test (positive-trees? (cons (leaf -6) empty))
 #f)
(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6)) empty))
 #t)
(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6)) (cons (node 0 (leaf 0) (leaf 1)) empty)))
 #t)
(test (positive-trees? (cons (node -1 (leaf 6) (leaf -6)) (cons (node 0 (leaf 0) (leaf 1)) empty)))
 #f)