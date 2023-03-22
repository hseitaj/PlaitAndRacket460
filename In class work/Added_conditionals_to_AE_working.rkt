#lang plait 
(print-only-errors #t)
(define-type Value
  [numV  [n : Number]]
  [boolV [b : Boolean]])

(define-type Exp
  (numE [n : Number])
  (boolE [b : Boolean])
  (plusE [l : Exp]
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (cndE  [tst : Exp]
         [thn : Exp]
         [els : Exp]))

;; An EXP-S-EXP is either
;; - `NUMBER
;; - `{+ EXP-S-EXP EXP-S-EXP}
;; - `{* EXP-S-EXP EXP-S-EXP}

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))] 
    [(s-exp-match? `{cnd ANY ANY ANY} s)
     (cndE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(or (equal? `#t s) (equal? `#f s)) (boolE (s-exp->boolean s))]
    [else (error 'parse "invalid input")]))

(test (parse `{cnd 0 1 2})
      (cndE (numE 0) (numE 1) (numE 2)))
(test/exn (parse `(7 6)) "invalid input")
(test (parse `2)
      (numE 2))
(test (parse `{+ 2 1})
      (plusE (numE 2) (numE 1)))

(define (interp [a : Exp]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(boolE b) (boolV b)] 
    [(plusE l r) (add/mul + (interp l) (interp r))]
    [(multE l r) (add/mul * (interp l) (interp r))]
    [(cndE tst th el) (if (bool-dec (interp tst))
                          (interp th)
                          (interp el)) ]
    ))

(define (add/mul op v1 v2)
  (type-case Value v1
    [(numV n1) (type-case Value v2
                 [(numV n2) (numV (op n1 n2))]
                 [else (error '+ "RHS should be num")]) ]
    [else (error '+ "LHS should be num")]))

(define (bool-dec tst)
  (type-case Value tst
    [(boolV b) b]
    [else (error 'cnd "Only tests booleans")]))


(test (interp (parse `2))
      (numV 2))
(test (interp (parse `{+ 2 1}))
      (numV 3))

(test (interp (parse `{cnd  #t 2 1}))
      (numV 2))
(test (interp (parse `{cnd  #f 2 1}))
      (numV 1))
