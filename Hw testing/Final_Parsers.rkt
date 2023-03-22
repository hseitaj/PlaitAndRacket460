#lang plait
(define $$$$$$$BEGIN$inherit-parse 'a)
(module+ test
  (print-only-errors #t))
(require "all-classes.rkt")

;; ----------------------------------------

(define (parse-class [s : S-Exp]) : (Symbol * ClassI)
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     (values (s-exp->symbol (second (s-exp->list s)))
             (classI
              (s-exp->symbol (fourth (s-exp->list s)))
              (map parse-field
                   (s-exp->list (fourth (rest (s-exp->list s)))))
              (map parse-method 
                   (rest (rest (rest (rest (rest (s-exp->list s)))))))))]
    [else (error 'parse-class "invalid input")]))

(define (parse-field [s : S-Exp]) : Symbol
  (cond
    [(s-exp-match? `SYMBOL s)
     (s-exp->symbol s)]
    [else (error 'parse-field "invalid input")]))

(define (parse-method [s : S-Exp]) : (Symbol * ExpI)
  (cond
    [(s-exp-match? `[SYMBOL {arg} ANY] s)
     (values (s-exp->symbol (first (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [else (error 'parse-method "invalid input")]))

(define (parse [s : S-Exp]) : ExpI
  (cond
    [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
    [(s-exp-match? `arg s) (argI)]
    [(s-exp-match? `this s) (thisI)]
    [(s-exp-match? `null s) (nullI)]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusI (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multI (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (let ([pList (map parse (rest (s-exp->list s)))])
       (if0I (first pList)
             (second pList)
             (third pList)))]
    [(s-exp-match? `{newarray SYMBOL ANY ANY} s)
     (newArrayI
      (parse-type (second (s-exp->list s)))
      (parse (third (s-exp->list s)))
      (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{arrayref ANY ANY} s)
     (arrayRefI (parse (second (s-exp->list s)))
                (parse (third (s-exp->list s))))]
    [(s-exp-match? `{arrayset ANY ANY ANY} s)
     (let ([pList (map parse (rest (s-exp->list s)))])
       (arraySetI (first pList)
                  (second pList)
                  (third pList)))]
    [(s-exp-match? `{cast SYMBOL ANY} s)
     (castI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{new SYMBOL ANY ...} s)
     (newI (s-exp->symbol (second (s-exp->list s)))
           (map parse (rest (rest (s-exp->list s)))))]
    [(s-exp-match? `{get ANY SYMBOL} s)
     (getI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? `{send ANY SYMBOL ANY} s)
     (sendI (parse (second (s-exp->list s)))
            (s-exp->symbol (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{super SYMBOL ANY} s)
     (superI (s-exp->symbol (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test

  (test (parse `{arrayset {newarray Posn 5 {new Posn 1 2}} 3 {new Posn 3 4}})
        (arraySetI (newArrayI (objT 'Posn) (numI 5) (newI 'Posn (list (numI 1) (numI 2))))
                   (numI 3)
                   (newI 'Posn (list (numI 3) (numI 4)))))
  (test (parse `{cast Object {new Posn 1 2}})
        (castI 'Object (newI 'Posn (list (numI 1) (numI 2)))))

  (test (parse `{if0 5 3 4})
        (if0I (numI 5) (numI 3) (numI 4)))
  (test (parse `{if0 0 {+ 1 2} {new Posn 1 2}})
        (if0I (numI 0) (plusI (numI 1) (numI 2)) (newI 'Posn (list (numI 1) (numI 2)))))

  (test (parse `{newarray Posn 5 {new Posn 1 2}})
        (newArrayI (objT 'Posn) (numI 5) (newI 'Posn (list (numI 1) (numI 2)))))

  (test (parse `{if0 0 null null})
        (if0I (numI 0) (nullI) (nullI)))
  
  (test (parse `0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse `{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse `{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse `{new Posn 1 2})
        (newI 'Posn (list (numI 1) (numI 2))))
  (test (parse `{get 1 x})
        (getI (numI 1) 'x))
  (test (parse `{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse `{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field `{x 1})
            "invalid input")

  (test (parse-method `[m {arg} this])
        (values 'm (thisI)))
  (test/exn (parse-method `[m {arg} 1 2])
            "invalid input")
  
  (test (parse-class `{class Posn3D extends Posn
                        {x y z}
                        [m1 {arg} arg]
                        [m2 {arg} this]})
        (values 'Posn3D
                (classI 'Posn
                        (list 'x 'y 'z)
                        (list (values 'm1 (argI))
                              (values 'm2 (thisI))))))
  (test/exn (parse-class `{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-prog [classes : (Listof S-Exp)] [a : S-Exp]) : S-Exp
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [(numV n) (number->s-exp n)]
      [(objV class-name field-vals) `object]
      [(nullV) `null]
      [(arrayV t vals) `array])))

(module+ test
  (define posnL (list
                 `{class Posn extends Object
                    {x y}
                    [mdist {arg} {+ {get this x} {get this y}}]
                    [addDist {arg} {+ {send arg mdist 0}
                                      {send this mdist 0}}]}
         
                 `{class Posn3D extends Posn
                    {z}
                    [mdist {arg} {+ {get this z} 
                                    {super mdist arg}}]}))

  (test (interp-prog
         posnL
         `{cast Posn {new Posn3D 5 3 1}})
        `object)
  (test (interp-prog
         posnL
         `{newarray Posn 5 {new Posn 1 2}})
        `array)
  
  (test (interp-prog 
         posnL
         `{if0 0
               {send {new Posn3D 5 3 1} addDist {new Posn -2 -7}}
               {send {new Posn3D 5 3 1} addDist {new Posn 2 7}}})
        `0)

  
  (test (interp-prog
         (list
          `{class Empty extends Object
             {}})
         `{new Empty})
        `object)

  (test (interp-prog 
         posnL
         `{send {new Posn3D 5 3 1} addDist {new Posn 2 7}})
        `18))
(define $$$$$$$END$inherit-parse 'a)



(define $$$$$$$BEGIN$type-parse 'a)
;; ----------------------------------------

(define (parse-t-class [s : S-Exp]) : (Symbol * ClassT)
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     (values
      (s-exp->symbol (second (s-exp->list s)))
      (classT (s-exp->symbol (fourth (s-exp->list s)))
              (map parse-t-field
                   (s-exp->list (fourth (rest (s-exp->list s)))))
              (map parse-t-method 
                   (rest (rest (rest (rest (rest (s-exp->list s)))))))))]
    [else (error 'parse-t-class "invalid input")]))

(define (parse-t-field [s : S-Exp]) : (Symbol * Type)
  (cond
    [(s-exp-match? `[SYMBOL : ANY] s)
     (values (s-exp->symbol (first (s-exp->list s)))
             (parse-type (third (s-exp->list s))))]
    [else (error 'parse-t-field "invalid input")]))

(define (parse-t-method [s : S-Exp]) : (Symbol * MethodT)
  (cond
    [(s-exp-match? `[SYMBOL {[arg : ANY]} : ANY ANY] s)
     (values
      (s-exp->symbol (first (s-exp->list s)))
      (methodT (parse-type (local [(define args (second (s-exp->list s)))
                                   (define arg (first (s-exp->list args)))]
                             (third (s-exp->list arg))))
               (parse-type (fourth (s-exp->list s)))
               (parse (fourth (rest (s-exp->list s))))))]
    [else (error 'parse-t-method "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
    [(s-exp-match? `num s)
     (numT)]
    [(s-exp-match? `SYMBOL s)
     (objT (s-exp->symbol s))]
    [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse-type `num)
        (numT))
  (test (parse-type `Object)
        (objT 'Object))
  (test/exn (parse-type `{})
            "invalid input")
  
  (test (parse-t-field `[x : num])
        (values 'x (numT)))
  (test/exn (parse-t-field `{x 1})
            "invalid input")

  (test (parse-t-method `[m {[arg : num]} : Object this])
        (values 'm (methodT (numT) (objT 'Object) (thisI))))
  (test/exn (parse-t-method `{m 1})
            "invalid input")
  
  (test (parse-t-class `{class Posn3D extends Posn
                          {[x : num] [y : num]}
                          [m1 {[arg : num]} : num arg]
                          [m2 ([arg : num]) : Object this]})
        (values 'Posn3D
                (classT 'Posn
                        (list (values 'x (numT))
                              (values 'y (numT)))
                        (list (values 'm1 (methodT (numT) (numT) (argI)))
                              (values 'm2 (methodT (numT) (objT 'Object) (thisI)))))))
  (test/exn (parse-t-class `{class})
            "invalid input"))

;; ----------------------------------------

(define (interp-t-prog [classes : (Listof S-Exp)] [a : S-Exp]) : S-Exp
  (let ([v (interp-t (parse a)
                     (map parse-t-class classes))])
    (type-case Value v
      [(numV n) (number->s-exp n)]
      [(objV class-name field-vals) `object]
      [(nullV) `null]
      [(arrayV t vals) `(array of type ,(string->s-exp (to-string t)))])))

(module+ test

  (define posnTL (list
                  `{class Posn extends Object
                     {[x : num]
                      [y : num]}
                     [mdist {[arg : num]} : num
                            {+ {get this x} {get this y}}]
                     [addDist {[arg : Posn]} : num
                              {+ {send arg mdist 0}
                                 {send this mdist 0}}]}
         
                  `{class Posn3D extends Posn
                     {[z : num]}
                     [mdist {[arg : num]} : num
                            {+ {get this z} 
                               {super mdist arg}}]}))

  (test (interp-t-prog
         posnTL
         `{newarray Posn 5 {new Posn 2 7}})
        `(array of type "(objT 'Posn)"))
  (test (interp-t-prog
         posnTL
         `{arrayref {newarray Posn 5 {new Posn 2 7}} 2})
        `object)
  
  (test (interp-t-prog
         (list
          `{class Empty extends Object
             {}})
         `{new Empty})
        `object)

  (test (interp-t-prog 
         posnTL
         `{send {new Posn3D 5 3 1} addDist {new Posn 2 7}})
        `18))



(define $$$$$$$END$type-parse 'a)