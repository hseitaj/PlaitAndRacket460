#lang plait

(define (not-1 x)
  (not (< x 10)))

(filter not-1 '(11 1 2 22 3 412 23))

(define (gt10 x)
  (> x 10))


(define (me-filter func lst out)
  (if (empty? lst) 
      out 
  (if (func (first lst))
  (me-filter func (rest lst)
                 (append out (list (first lst))))
  (me-filter func (rest lst) out))))

; function trace for debugging
(trace me-filter)
(me-filter gt10 '(1 2 3 11 212 231 42) '())