#lang plait

;; Direct-style interpreter

(define-type Exp
  (numE [n : Number])
  (plusE [lhs : Exp]
         [rhs : Exp])
  (multE [lhs : Exp]
         [rhs : Exp])
  (idE [name : Symbol])
  (lamE [param : Symbol]
        [body : Exp])
  (appE [fun-expr : Exp]
        [arg-expr : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp]))

(define-type Value
  (numV [n : Number])
  (closV [n : Symbol]
         [body : Exp]
         [env : Env]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

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
   [(s-exp-match? `{lambda {SYMBOL} ANY} s)
    (lamE (s-exp->symbol (first (s-exp->list 
                                 (second (s-exp->list s)))))
          (parse (third (s-exp->list s))))]
   [(s-exp-match? `{ANY ANY} s)
    (appE (parse (first (s-exp->list s)))
          (parse (second (s-exp->list s))))]
   [(s-exp-match? `{if0 ANY ANY ANY} s)
    (if0E (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `3) (numE 3))
  (test (parse `x) (idE 'x))
  (test (parse `{+ 1 2}) (plusE (numE 1) (numE 2)))
  (test (parse `{* 1 2}) (multE (numE 1) (numE 2)))
  (test (parse `{lambda {x} x}) (lamE 'x (idE 'x)))
  (test (parse `{1 2}) (appE (numE 1) (numE 2)))
  (test (parse `{if0 0 1 2}) (if0E (numE 0) (numE 1) (numE 2)))
  (test/exn (parse `{}) "invalid input"))

;; ----------------------------------------

(define (interp a env)
  (type-case Exp a
    [(numE n) (numV n)]
    [(plusE l r) (num+ (interp l env)
                       (interp r env))]
    [(multE l r) (num* (interp l env)
                       (interp r env))]
    [(idE name) (lookup name env)]
    [(lamE n body-expr)
     (closV n body-expr env)]
    [(appE fun-expr arg-expr)
     (let ([fun-val (interp fun-expr env)]
           [arg-val (interp arg-expr env)])
       (interp (closV-body fun-val)
               (extend-env (bind (closV-n fun-val)
                                 arg-val)
                           (closV-env fun-val))))]
    [(if0E test-expr then-expr else-expr)
     (if (numzero? (interp test-expr env))
         (interp then-expr env)
         (interp else-expr env))]))

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num* (num-op *))

(define (numzero? x)
  (zero? (numV-n x)))

(define (lookup name env)
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (if (symbol=? name (bind-name (first env)))
             (bind-val (first env))
             (lookup name (rest env)))]))

;; ----------------------------------------

(module+ test
  (test (interp (parse `10) mt-env)
        (numV 10))
  (test (interp (parse `{+ 10 7}) mt-env)
        (numV 17))
  (test (interp (parse `{* 10 7}) mt-env)
        (numV 70))
  (test (interp (parse `{{lambda {x} {+ x 12}}
                         {+ 1 17}})
                mt-env)
        (numV 30))
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 10))
                            mt-env))
        (numV 10))
  (test (interp (parse `{{lambda {x} {+ x 12}}
                         {+ 1 17}})
                mt-env)
        (numV 30))
  (test (interp (parse `{{lambda {x}
                           {{lambda {f}
                              {+ {f 1}
                                 {{lambda {x}
                                    {f 2}}
                                  3}}}
                            {lambda {y} {+ x y}}}}
                         0})
                mt-env)
        (numV 3))
  (test (interp (parse `{if0 0 1 2})
                mt-env)
        (numV 1))
  (test (interp (parse `{if0 1 1 2})
                mt-env)
        (numV 2))
  (test (interp (parse 
                 `{{lambda {mkrec}
                     {{lambda {fib}
                        ;; Call fib on 4:
                        {fib 4}}
                      ;; Create recursive fib:
                      {mkrec
                       {lambda {fib}
                         ;; Fib:
                         {lambda {n}
                           {if0 n
                                1
                                {if0 {+ n -1}
                                     1
                                     {+ {fib {+ n -1}}
                                        {fib {+ n -2}}}}}}}}}}
                   ;; mkrec:
                   {lambda {body-proc}
                     {{lambda {fX}
                        {fX fX}}
                      {lambda {fX}
                        {body-proc {lambda {x} {{fX fX} x}}}}}}})
                mt-env)
        (numV 5))

  (test/exn (interp (parse `x) mt-env)
            "free variable")

  ;; Timing test --------------------
  (define c (parse
             `{{{{lambda {x}
                   {lambda {y} 
                     {lambda {z} {+ {+ x x} {+ x x}}}}}
                 1}
                2}
               3}))
  (define (multi-interp n)
    (if (zero? n)
        (void)
        (begin
          (interp c mt-env)
          (multi-interp (- n 1)))))
  (time (multi-interp 10000)))

