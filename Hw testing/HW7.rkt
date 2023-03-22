#lang plait
;;HW 7               CS 3520
;;Dan Ruley          u0956834

(define-type Value
  (numV [n : Number])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (contV [k : Cont]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (negE [n : Exp])
  (avgE [n1 : Exp]
        [n2 : Exp]
        [n3 : Exp])
  (if0 [cond : Exp]
       [pass : Exp]
       [fail : Exp])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [ns : (Listof Symbol)]
        [body : Exp])
  (appE [fun : Exp]
        [args : (Listof Exp)])
  (let/ccE [n : Symbol]
           [body : Exp]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)
(define extend-env* append)

(define-type Cont
  (doneK)
  (plusSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (makeNegK    [k : Cont])
  (avgSecondK  [n2 : Exp]
               [n3 : Exp]
               [e : Env]
               [k : Cont])
  (avgThirdK   [n3 : Exp]
               [v1 : Value]
               [e : Env]
               [k : Cont])
  (doAvgK      [v1 : Value]
               [v2 : Value]
               [k : Cont])
  (ifSecondK   [pass : Exp]
               [fail : Exp]
               [e : Env]
               [k : Cont])
  (doPlusK     [v : Value]
               [k : Cont])
  (multSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doMultK     [v : Value]
               [k : Cont])
  (firstArgK   [args : (Listof Exp)]
               [e : Env]
               [k : Cont])
  (appArgK     [args : (Listof Exp)]      ;appArgK has a TODO list of expression args it needs to interp
               [vals : (Listof Value)]    ;appArgK has a DONE list of values it builds through repeated continues
               [closure : Value]          ;appArgK needs to store the lambda closure as it continues processing the args.
               [e : Env]                  
               [k : Cont]))                     


(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{neg ANY} s) (negE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{avg ANY ANY ANY} s)
     (avgE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0 (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (second bs)))))]
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s)
     (lamE (map s-exp->symbol (s-exp->list 
                               (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let/cc SYMBOL ANY} s)
     (let/ccE (s-exp->symbol (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {x} 9})
        (lamE (list 'x) (numE 9)))
  (test (parse `{let/cc k 0})
        (let/ccE 'k (numE 0)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test/exn (parse `{})
            "invalid input"))

;; interp & continue ----------------------------------------
(define (interp [a : Exp] [env : Env] [k : Cont]) : Value
  (type-case Exp a
    [(numE n) (continue k (numV n))]
    [(idE s) (continue k (lookup s env))]
    [(plusE l r) (interp l env
                         (plusSecondK r env k))]
    [(multE l r) (interp l env
                         (multSecondK r env k))]
    [(negE n) (interp n env (makeNegK k))]
    [(avgE n1 n2 n3) (interp n1 env (avgSecondK n2 n3 env k))]
    [(if0 cond pass fail) (interp cond env (ifSecondK pass fail env k))]
    [(lamE ns body)
     (continue k (closV ns body env))]
    [(appE fun args) (interp fun env
                             (firstArgK args env k))]
    [(let/ccE n body)
     (interp body
             (extend-env (bind n (contV k))
                         env)
             k)]))

;; interp-expr -------------------------------------
(define (interp-expr [e : Exp]) : S-Exp
  (let ([val (interp e mt-env (doneK))])
    (type-case Value val
      [(numV n) (number->s-exp n)]
      [(closV arg body env) `function]
      [(contV k) `function])))

;; continue -----------------------------------------
(define (continue [k : Cont] [v : Value]) : Value
  (type-case Cont k
    [(doneK) v]
    [(makeNegK next-k)
     (type-case Value v
       [(numV n) (continue next-k (numV (* -1 n)))]
       [else (error 'interp "neg NaN")])]
    [(avgSecondK n2 n3 env next-k)
     (interp n2 env (avgThirdK n3 v env next-k))]
    [(avgThirdK n3 v1 env next-k)
     (interp n3 env (doAvgK v v1 next-k))]
    [(doAvgK v1 v2 next-k)
     (continue next-k (avg-helper v v1 v2))]
    [(ifSecondK pass fail env next-k)
     (type-case Value v
       [(numV n)
        (if (= 0 n)
            (interp pass env next-k)
            (interp fail env next-k))]            
       [else (error 'interp "if0 NaN")])]
    [(plusSecondK r env next-k)
     (interp r env
             (doPlusK v next-k))]
    [(doPlusK v-l next-k)
     (continue next-k (num+ v-l v))]
    [(multSecondK r env next-k)
     (interp r env
             (doMultK v next-k))]
    [(doMultK v-l next-k)
     (continue next-k (num* v-l v))]
    ;firstArgK is the first continue for processing a fn application.  If there are no args, it immediately evaluates the closure v.
    ;Otherwise, it starts the process of interping the args, stores the closure in an appArgK
    [(firstArgK args env next-k)
     (type-case (Listof Exp) args
       [empty (type-case Value v
                [(closV ns body c-env)
                 (interp body c-env next-k)]
                [(contV k-v) (error 'interp "cannot call contV with zero arguments")]
                [else (error 'interp "not a function")])]
       [(cons fst rst)
        (interp fst env (appArgK rst empty v env next-k))])]
    ;The app argK stores the args and saves interped args in vals.  It stores the closure, env, and next-k as well.
    ;Once args is empty, we proceed with processing the closV, extending the environment to map ns to vals
    [(appArgK args vals closure env next-k)
     (type-case (Listof Exp) args
       [empty (type-case Value closure
                [(closV ns body c-env)
                 (interp body
                         (extend-env*
                          (map2 bind ns (reverse (cons v vals))) ;;need to reverse value order
                          c-env)
                         next-k)]
                [(contV k-v)
                 (if (zero? (- 1 (length (cons v vals))))
                     (continue k-v v)
                     (error 'interp "cannot call contV with > 1 argument"))]
                [else (error 'interp "not a function")])]
       [(cons fst rst)
        (interp fst env (appArgK rst (cons v vals) closure env next-k))])]))


(module+ test
  ;tests for the avg-helper fn.
  (test (avg-helper (numV 1) (numV 2) (numV 3))
        (numV 2))
  (test/exn (avg-helper (closV (list 'x) (idE 'x) mt-env) (numV 2) (numV 3))
            "avg NaN")
  (test/exn (avg-helper (numV 2) (closV (list 'x) (idE 'x) mt-env) (numV 3))
            "avg NaN")
  (test/exn (avg-helper (numV 2) (numV 3) (closV (list 'x) (idE 'x) mt-env))
            "avg NaN"))

;; Kind of ugly helper for computing the average
(define (avg-helper [v1 : Value] [v2 : Value] [v3 : Value]) : Value
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2)
        (type-case Value v3
          [(numV n3) (numV (/ (+ n1 (+ n2 n3)) 3))]
          [else (error 'interp "avg NaN")])]
       [else (error 'interp "avg NaN")])]
    [else (error 'interp "avg NaN")]))




(module+ test

  ;neg tests
  (test (interp-expr (parse `{neg 2}))
        `-2)
  (test (interp-expr (parse `{neg {+ -3 -4}}))
        `7)
  (test (interp-expr (parse `{let/cc k {neg {k 3}}}))
        `3)
  (test/exn (interp-expr (parse `{neg {lambda {x} x}}))
            "neg NaN")
  
  ;avg tests
  (test (interp-expr (parse `{avg 0 6 6}))
        `4)
  (test (interp-expr (parse `{let/cc k {avg 0 {k 3} 0}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg {k 2} {k 3} 0}}))
        `2)

  ;if0 tests
  (test (interp-expr (parse `{if0 1 2 3}))
        `3)
  (test/exn (interp-expr (parse `{if0 {lambda {x} x} 2 3}))
            "if0 NaN")
  (test (interp-expr (parse `{if0 0 2 3}))
        `2)
  (test (interp-expr (parse `{let/cc k {if0 {k 9} 2 3}}))
        `9)

  ;multiple-argument fn tests
  (test (interp-expr (parse `{{lambda {x y} {+ y {neg x}}} 10 12}))
        `2)
  (test (interp-expr (parse `{lambda {} 12}))
        `function)
  (test (interp-expr (parse `{lambda {x} {lambda {} x}}))
        `function)
  (test (interp-expr (parse `{{{lambda {x} {lambda {} x}} 13}}))
        `13)

  (test (interp-expr (parse `{let/cc esc {{lambda {x y} x} 1 {esc 3}}}))
        `3)
  (test (interp-expr (parse `{{let/cc esc {{lambda {x y} {lambda {z} {+ z y}}}
                                           1 
                                           {let/cc k {esc k}}}}
                              10}))
        `20)

  (test/exn (interp-expr (parse `{let/cc esc {esc}}))
            ;; error because continuation is given 0 arguments,
            ;; but the specific error message is not specified
            "")
  (test/exn (interp-expr (parse `{let/cc esc {esc 1 2}}))
            ;; error because continuation is given 2 arguments
            "")

  (test/exn (continue (firstArgK empty mt-env (doneK)) (numV 7))
            "not a function")
  (test (interp-expr (parse `{lambda {x} {+ x x}}))
        `function)
  (test (interp-expr (parse ` {let/cc k k}))
        `function)
  (test (interp-expr (parse `{lambda {x} {lambda {} x}}))
        `function)
  
  (test (interp (parse `2) mt-env (doneK))
        (numV 2))
  (test/exn (interp (parse `x) mt-env (doneK))
            "free variable")
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env (doneK))
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env (doneK))
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                (doneK))
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                (doneK))
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK))
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                (doneK))
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK))
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK))
        (numV 16))

  (test (interp (parse `{let/cc k {+ 1 {k 0}}})
                mt-env
                (doneK))
        (numV 0))
  (test (interp (parse `{let {[f {let/cc k k}]}
                          {f {lambda {x} 10}}})
                mt-env
                (doneK))
        (numV 10))

  (test/exn (interp (parse `{1 2}) mt-env (doneK))
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK))
            "free variable")
  ;; Eager:
  (test/exn (interp (parse `{{lambda {x} 0} {1 2}}) mt-env (doneK))
            "not a function")

  (test (continue (doneK) (numV 5))
        (numV 5))
  (test (continue (plusSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 11))
  (test (continue (doPlusK (numV 7) (doneK)) (numV 5))
        (numV 12))
  (test (continue (multSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 30))
  (test (continue (doMultK (numV 7) (doneK)) (numV 5))
        (numV 35)))
;  (test (continue (appArgK (numE 5) mt-env (doneK)) (closV (list 'x) (idE 'x) mt-env))
;        (numV 5))
;  (test (continue (doAppK (closV (list 'x) (idE 'x) mt-env) (doneK)) (numV 8))
;        (numV 8)))

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
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b))
                         (bind-val b)]
                        [else (lookup n rst-env)])]))

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
