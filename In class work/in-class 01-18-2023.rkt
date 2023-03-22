#lang plait

(define s `(1 2 3))
(define ps (s-exp->list s))
(define (s>n s)
  (s-exp->number s))