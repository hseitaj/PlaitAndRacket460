; Hansi Seitaj, CMPSC 460, Hw 0

#lang plait

#|PROBLEM 1
returns a list containing n copies of x|#
(define (duple [n : Number] x)
  (cond
    [(= n 0) '()]
    [else (append (list x)
                  (duple (- n 1) x))]))

(test (duple 2 3)
      '(3 3))
(test (duple 4 '(ha ha))
      '((ha ha) (ha ha) (ha ha) (ha ha)))
(test (duple 0 '(word))
      '())

#|PROBLEM 2
return a sorted list of integers based on operator
less than < ascending
greater than > descending
> |#
(define (merge-helper [op : (Number Number -> Boolean)]
                      [int-list1 : (Listof Number)]
                      [int-list2 : (Listof Number)]
                      [output : (Listof Number)]) : (Listof Number)
  (cond
    [(empty? int-list1) (append output int-list2)] ;base-case
    [(empty? int-list2) (append output int-list1)] ;base-case
    [(op (first int-list1) (first int-list2))                
     (merge-helper op (rest int-list1) int-list2 (append output (list (first int-list1))))]
    [else
     (merge-helper op  int-list1 (rest int-list2) (append output (list (first int-list2))))
     ]))

(define (merge [op : (Number Number -> Boolean)]
               [int-list1 : (Listof Number)]
               [int-list2 : (Listof Number)]) : (Listof Number)
  (merge-helper op int-list1 int-list2 '()))
 
(test (merge < '(1 4 6) '(2 5 8))
      '(1 2 4 5 6 8))
(test (merge > '(6 4 1) '(8 5 2))
      '(8 6 5 4 2 1))
 
#|Problem 3
return an association list from a list of symbols and a list of numbers
define a type to allow for the output of a list of associations
?_t is to be replaced with your appropriately named type  |# 

(define-type Assoc
  (assoc [s : Symbol]
         [n : Number]))
 
(define (make-new-assoc [names : (Listof Symbol)]   
                        [values : (Listof Number)]
                        [output : (Listof Assoc)]): (Listof Assoc) 

  (cond
    [(empty? names) output]
    [else (make-new-assoc (rest names) (rest values)
                          (append output (list (assoc (first names)
                                                      (first values)))))]))

(define (associatons [names : (Listof Symbol)]   
                      [values : (Listof Number)]): (Listof Assoc)
  (make-new-assoc names values empty))


(test (associatons '(a b c d) '(1 2 3 4))
      (list (assoc 'a 1) (assoc 'b 2) (assoc 'c 3) (assoc 'd 4)))
(test (associatons '(t a c o tuesday) '(0 1 34 1729 42))
      (list (assoc 't 0) (assoc 'a 1) (assoc 'c 34) (assoc 'o 1729) (assoc 'tuesday 42)))

