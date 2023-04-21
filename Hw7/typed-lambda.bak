#lang plait
; Hansi Seitaj, CMPSC 460, Hw7 - Types

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (pairV  [fst : Value]
          [snd : Value]))

(define-type Exp
  (numE   [n : Number])
  (trueE)
  (falseE)
  (ifE    [cond : Exp]
          [pass : Exp]
          [fail : Exp])
 
  (=E     [l : Exp]
          [r : Exp])
  
  (fstE   [p : Exp])
  (sndE   [p : Exp])
  (pairE  [fst : Exp] 
          [snd : Exp])
  (idE    [s : Symbol])
  (plusE  [l : Exp] 
          [r : Exp])
  (multE  [l : Exp]
          [r : Exp])
  (lamE   [ns : (Listof Symbol)]
          [arg-types : (Listof Type)]
          [body : Exp])
  (appE   [fun : Exp]
          [args : (Listof Exp)]))  

(define-type Type
  (numT)
  (boolT)
  (arrowT [args : (Listof Type)]
          [result : Type])
  (crossT [t1 : Type]
          [t2 : Type]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `true s) (trueE)]
    [(s-exp-match? `false s) (falseE)]
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{if ANY ANY ANY} s)
     (let ([s-lst (s-exp->list s)])
       (ifE (parse (second s-lst))
            (parse (third s-lst))
            (parse (fourth s-lst))))]
    
    [(s-exp-match? `{= ANY ANY} s)
     (=E (parse (second (s-exp->list s)))
         (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{pair ANY ANY} s)
     (pairE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{fst ANY} s)
     (fstE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{snd ANY} s)
     (sndE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (cons (s-exp->symbol (first bs)) empty)
                   (cons (parse-type (third bs)) empty)
                   (parse (third (s-exp->list s))))
             (cons (parse (fourth bs)) empty)))]
    
    [(s-exp-match? `{lambda {[SYMBOL : ANY] ...} ANY} s)
     (let ([args (map s-exp->list
                 (s-exp->list 
                         (second (s-exp->list s))))])
       (lamE (map s-exp->symbol (map first args))
             (map parse-type (map third args))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]
   [(s-exp-match? `(ANY ... -> ANY) s)
    (arrowT (map parse-type (reverse (rest (rest (reverse (s-exp->list s))))))
            (parse-type (first (reverse (s-exp->list s)))))]
   [(s-exp-match? `(ANY * ANY) s)
    (crossT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
   [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  
  (test (parse `{let {[x : num {+ 1 2}]}
                  y})
        (appE (lamE '(x) (list (numT)) (idE 'y)) (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {[x : num]} 9})
        (lamE '(x) (list (numT)) (numE 9)))
  
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (list (numT)) (boolT)))
  (test/exn (parse-type `1)
            "invalid input"))

;; It recursively extends the environment by binding each symbol in ns to the value of the corresponding
;; expression in args, in the context of the current environment env.
(define (extend-helper [ns : (Listof Symbol)] [args : (Listof Exp)] [env : Env]) : Env
  (type-case (Listof Symbol) ns
    [empty env]
    [(cons fst rst) (extend-helper
                     (rest ns)
                     (rest args) (extend-env (bind fst (interp (first args) env)) env))]))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(trueE) (boolV #t)]
    [(falseE) (boolV #f)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(ifE cond pass fail)
     (type-case Value (interp cond env)
       [(boolV val) (if val
                        (interp pass env)
                        (interp fail env))]
       [else (error 'interp "not a boolean")])]
    [(=E l r)
     (type-case Value (interp l env)
       [(numV n1)
        (type-case Value (interp r env)
          [(numV n2)
           (if (= n1 n2)
               (boolV #t)
               (boolV #f))]
          [else (error 'interp "not a number")])]
       [else (error 'interp "not a number")])]
    [(fstE p)
     (type-case Value (interp p env)
       [(pairV fst snd) fst]
       [else (error 'interp "not a pair")])]
    [(sndE p)
     (type-case Value (interp p env)
       [(pairV fst snd) snd]
       [else (error 'interp "not a pair")])]
    [(pairE fst snd)
     (pairV (interp fst env) (interp snd env))]
    [(lamE ns t body)
     (closV ns body env)]
    [(appE fun args) (type-case Value (interp fun env)
                       [(closV ns body c-env)
                        (interp body
                                (extend-helper ns args c-env))]
                       [else (error 'interp "not a function")])]))

(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19)) 
  (test (interp (parse `{lambda {[x : num]} {+ x x}})
                mt-env)
       (closV '(x) (plusE (idE 'x) (idE 'x)) '()))
  (test (interp (parse `{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
    (type-case (Listof 'a) vals
      [empty (error 'find "free variable")]
      [(cons val rst-vals) (if (equal? name (name-of val))
                               (val-of (first vals))
                               ((make-lookup name-of val-of) name rst-vals))])))

(define lookup
  (make-lookup bind-name bind-val))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; typecheck ----------------------------------------
(define (typecheck [a : Exp] [tenv : Type-Env])
  (type-case Exp a
    [(numE n) (numT)]
    [(trueE) (boolT)]
    [(falseE) (boolT)]
    [(plusE l r) (typecheck-nums l r tenv)]
    [(multE l r) (typecheck-nums l r tenv)]
    [(idE n) (type-lookup n tenv)]
    [(lamE ns arg-type body)
     (arrowT arg-type 
             (typecheck body 
                        (append (map2 tbind ns arg-type) 
                                    tenv)))]
    [(ifE cond pass fail)
     (type-case Type (typecheck cond tenv)
       [(boolT)
        (let ([ptype (typecheck pass tenv)])
          (if (equal? ptype (typecheck fail tenv))
              ptype
              (type-error cond "equal sub-expression types(ifE)")))]
       [else (type-error cond "boolean")])]
    [(=E l r) (let ([ltype (typecheck l tenv)]) 
                (let ([rtype (typecheck r tenv)])
                  (if (and (equal? ltype (numT)) (equal? ltype rtype))
                      (boolT)
                      (type-error l "both numT expressions (=E)"))))]
    [(fstE p)
     (type-case Type (typecheck p tenv)
       [(crossT t1 t2) t1]
       [else (type-error p "a pair")])]
    [(sndE p) (type-case Type (typecheck p tenv)
                [(crossT t1 t2) t2]
                [else (type-error p "a pair")])]
    [(pairE fst snd)
     (crossT (typecheck fst tenv) (typecheck snd tenv))]
    [(appE fun args)
     (type-case Type (typecheck fun tenv)   
       [(arrowT arg-types result-type)
        (if (equal? arg-types  
                     (map (lambda (arg) (typecheck arg tenv)) args)) 
            result-type
            (type-error args
                        (to-string arg-types)))]
       [else (type-error fun "function")])]))

(define (typecheck-nums l r tenv)
  (type-case Type (typecheck l tenv)
    [(numT)
     (type-case Type (typecheck r tenv)
       [(numT) (numT)]
       [else (type-error r "num")])]
    [else (type-error l "num")]))
 
(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test
  (test (typecheck (parse `10) mt-env)
        (numT))
  (test (typecheck (parse `{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{lambda {[x : num]} 12}) mt-env)
        (arrowT (list (numT)) (numT)))
  (test (typecheck (parse `{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))

  (test/exn (typecheck (parse `{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))

;; Testing ========================================================

(test (interp (parse `{if true 4 5}) mt-env)
      (numV 4))
(test (interp (parse `{if false 4 5}) mt-env)
      (numV 5))
(test (interp (parse `{if {= 13 {if {= 1 {+ -1 2}}
                                   12
                                   13}}
                           4
                           5}) mt-env)
      (numV 5))
(test (typecheck (parse `{= 13 {if {= 1 {+ -1 2}} 12 13}}) mt-env)
      (boolT))
(test (typecheck (parse `{if {= 1 {+ -1 2}}
                            {lambda {[x : num]} {+ x 1}}
                            {lambda {[y : num]} y}}) mt-env)
      ;; This result may need to be adjusted after part 3:
      (arrowT (list (numT)) (numT)))
(test/exn (typecheck (parse `{+ 1 {if true true false}}) mt-env)
          "no type")

;; Part 2

(test (interp (parse `{pair 10 8}) mt-env)
 ;; Your constructor might be different than pairV:
 (pairV (numV 10) (numV 8)))

 (test (interp (parse `{fst {pair 10 8}}) mt-env)
 (numV 10))

 (test (interp (parse `{snd {pair 10 8}}) mt-env)
 (numV 8))

 (test (interp (parse `{let {[p : (num * num) {pair 10 8}]} {fst p}})  mt-env)
 (numV 10))

 (test (typecheck (parse `{pair 10 8}) mt-env)
 ;; Your constructor might be different than crossT:
 (crossT (numT) (numT)))

 (test (typecheck (parse `{fst {pair 10 8}}) mt-env)
 (numT))

 (test (typecheck (parse `{+ 1 {snd {pair 10 8}}}) mt-env)
 (numT))

 (test (typecheck (parse `{lambda {[x : (num * bool)]} {fst x}}) mt-env)
 ; Your constructor might be different than crossT:
 (arrowT (list (crossT (numT) (boolT))) (numT))) 

 (test (typecheck (parse `{{lambda {[x : (num * bool)]}  {fst x}}  {pair 1 false}}) mt-env)
 (numT))

 (test (typecheck (parse `{{lambda {[x : (num * bool)]} {snd x}} {pair 1 false}}) mt-env)
 (boolT))

 (test/exn (typecheck (parse `{fst 10}) mt-env) "no type")

 (test/exn (typecheck (parse `{+ 1 {fst {pair false 8}}})  mt-env)
 "no type")

 (test/exn (typecheck (parse `{lambda {[x : (num * bool)]} {if {fst x} 1 2}})  mt-env) 
 "no type")

;; Part 3

 (test (interp (parse `{{lambda {}  10}})  mt-env)
 (numV 10))

 (test (interp (parse `{{lambda {[x : num] [y : num]} {+ x y}}  10  20}) mt-env)
 (numV 30))
  

 (test (typecheck (parse `{{lambda {[x : num] [y : bool]} y}  10 false}) mt-env)
 (boolT))

 (test/exn (typecheck (parse `{{lambda {[x : num] [y : bool]} y} false 10}) mt-env)
 "no type")


