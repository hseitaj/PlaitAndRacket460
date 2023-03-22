#lang plait

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

