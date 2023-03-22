#lang racket

; define list in three ways
(cons 1 (cons 2 null))
'(1 2)
(list 1 2)

(define lst '(1 2 3 4 5 6 7 8 9 10 11 12 13))
(first lst)
(rest lst)

; if <expr> -> Bool, then, else

(define (11th lst)
  (define (helper lst count) 
  (if (= count 1)
      (first lst)
      (helper (rest lst) (- count 1))
      ))
  (helper lst 11)
  )

(11th lst) ;11