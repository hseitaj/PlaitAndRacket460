#lang plait
(print-only-errors #true)

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

(define-type Exp
  (numE  [n : Number])
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
;; - `{cnd EXP-S-EXP EXP-S-EXP EXP-S-EXP} 

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `BOOLEAN s) (boolE (s-exp->boolean s))]
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
    [else (error 'parse "invalid input")]))

(test (parse `2)
      (numE 2))
(test (parse `{+ 2 1})
      (plusE (numE 2) (numE 1)))
(test (parse `{* 3 4})
      (multE (numE 3) (numE 4)))
(test (parse `{+ {* 3 4} 8})
      (plusE (multE (numE 3) (numE 4))
             (numE 8)))
(test (parse `{cnd 1 1 0})
      (cndE (numE 1) (numE 1) (numE 0)))

(test/exn (parse `{2 1}) "invalid input")

(define (interp [a : Exp]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(plusE l r) (add/mult '+ (interp l) (interp r))]
    [(multE l r) (add/mult '* (interp l) (interp r))]
    [(cndE tst thn els) (if (bool-decide (interp tst))
                            (interp thn)
                            (interp els))]))

(define (add/mult op v1 v2)
  (type-case Value v1
    [(numV n) (type-case Value v2
                [(numV m) (numV (op n m))]
                [(boolV b) (error 'add/mult "you can't add bools") ])]
    [(boolV b) (error 'add/mult "you can't add bools")] ))

(define (bool-decide v)
  (type-case Value v
    [(boolV b) b]
    [(numV n) (if (= n 1)
                  #t
                  #f )]))
;    [else (error 'bool-decide "bools only")]))

(test (interp (parse `{cnd 1 1 0})) 0)
(test (interp (parse `{cnd 0 1 0})) 1)

(test (interp (parse `2))
      2)
(test (interp (parse `{+ 2 1}))
      3)
(test (interp (parse `{* 2 1}))
      2)
(test (interp (parse `{+ {* 2 3}
                         {+ 5 8}}))
      19)