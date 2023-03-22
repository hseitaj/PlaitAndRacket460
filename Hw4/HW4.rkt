#lang plait

;Solution to HW4
;Dan Ruley
;u0956834

(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location])
  (recV [ns : (Listof Symbol)]
        [vs : (Listof Value)]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp]
           [val : Exp])
  (beginE [expL : (Listof Exp)])
  (recordE [ns : (Listof Symbol)]
           [args : (Listof Exp)])
  (getE [rec : Exp]
        [n : Symbol]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type RecordStore
  (recsto [sto : Store]
          [vals : (Listof Value)]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)

(module+ test
  (print-only-errors #t))

;Updated implementation of override-store: Looks for a cell with the same location
;already in storage - if so, replace it.  Otherwise, cons it into the Store.
;; override-store --------------------------------
(define (override-store [sto : Storage] [sto-list : Store] ) : Store
  (type-case Store sto-list
    [empty (cons sto empty)]
    [(cons first rest)
     (cond
       [(= (cell-location first) (cell-location sto))
        (cons sto rest)]
       [else (cons first (override-store sto rest))])]))

(module+ test

  ;adding to empty behaves as expected
  (test (override-store (cell 1 (numV 3.14)) mt-store)
        (list (cell 1 (numV 3.14))))

  ;override-store should replace the value in a cell if the location is stored already
  (test (override-store (cell 1 (numV 15)) (override-store (cell 1 (numV 10)) mt-env))
        (list (cell 1 (numV 15))))

  ;no effect on other cells in Store
  (test (override-store (cell 1 (numV 15)) (override-store (cell 2 (numV 10)) mt-env))
        (list (cell 2 (numV 10)) (cell 1 (numV 15))))

  (test (override-store (cell 1 (numV 10))
                        (override-store (cell 2 (numV 25))
                                        (override-store (cell 1 (numV 17))
                                                        mt-store)))
        (list (cell 1 (numV 10)) (cell 2 (numV 25)))))

(define-type Result
  (v*s [v : Value] [s : Store]))



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
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{begin ANY ANY ...} s)
     (beginE (map parse (rest (s-exp->list s))))]
    [(s-exp-match? `{record {SYMBOL ANY} ...} s)
     (recordE (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                     (rest (s-exp->list s))))]
    [(s-exp-match? `{get ANY SYMBOL} s)
     (getE (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

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
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{box 0})
        (boxE (numE 0)))
  (test (parse `{unbox b})
        (unboxE (idE 'b)))
  (test (parse `{set-box! b 0})
        (setboxE (idE 'b) (numE 0)))
  (test (parse `{begin 1 2})
        (beginE (list (numE 1) (numE 2))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test (parse `{record {x 2} {y 3}})
        (recordE (list 'x 'y)
                 (list (numE 2) (numE 3))))
  (test (parse `{get {+ 1 2} a})
        (getE (plusE (numE 1) (numE 2)) 'a)))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
        body)
  (type-case Result call
    [(v*s v-id sto-id) body]))


;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
     (with [(v-l sto-l) (interp l env sto)]
           (with [(v-r sto-r) (interp r env sto-l)]
                 (v*s (num+ v-l v-r) sto-r)))]
    [(multE l r)
     (with [(v-l sto-l) (interp l env sto)]
           (with [(v-r sto-r) (interp r env sto-l)]
                 (v*s (num* v-l v-r) sto-r)))]
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
           (interp body
                   (extend-env
                    (bind n v-rhs)
                    env)
                   sto-rhs))]
    [(lamE n body)
     (v*s (closV n body env) sto)]
    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
           (with [(v-a sto-a) (interp arg env sto-f)]
                 (type-case Value v-f
                   [(closV n body c-env)
                    (interp body
                            (extend-env
                             (bind n v-a)
                             c-env)
                            sto-a)]
                   [else (error 'interp "not a function")])))]
    [(boxE a)
     (with [(v sto-v) (interp a env sto)]
           (let ([l (new-loc sto-v)])
             (v*s (boxV l) 
                  (override-store (cell l v) 
                                  sto-v))))]
    [(unboxE a)
     (with [(v sto-v) (interp a env sto)]
           (type-case Value v
             [(boxV l) (v*s (fetch l sto-v) 
                            sto-v)]
             [else (error 'interp "not a box")]))]
    [(setboxE bx val)
     (with [(v-b sto-b) (interp bx env sto)]
           (with [(v-v sto-v) (interp val env sto-b)]
                 (type-case Value v-b
                   [(boxV l)
                    (v*s v-v
                         (override-store (cell l v-v)
                                         sto-v))]
                   [else (error 'interp "not a box")])))]
    [(beginE expL)
     (interp-begin expL env sto)]
    [(recordE ns args) (let ([rec-sto (interp-record args env sto)])
                         (v*s (recV ns (recsto-vals rec-sto)) (recsto-sto rec-sto)))]
    [(getE rec n)
     (let ([record (interp rec env sto)])
       (type-case Value (v*s-v record)
         [(recV ns vs) (v*s (find n ns vs) (v*s-s record))]
         [else (error 'interp "not a record")]))]))


;Helper for interping records.  Accumulates a list of symbols and their associated values,
;creating a RecordStore containing the Store and a list of symbols
;;interp-record-----------------------------------
(define (interp-record [args : (Listof Exp)] [env : Env] [sto : Store]) : RecordStore
  (type-case (Listof Exp) args
    [(cons fst rst)
     (let ([item (interp fst env sto)])
       (let ([last (interp-record rst env (v*s-s item))])
         (recsto (recsto-sto last) (cons (v*s-v item) (recsto-vals last)))))]
    [empty (recsto sto empty)]))

;tests for interp-record
(module+ test
  (test (interp-record (list (multE (numE 10) (numE 2))) mt-env mt-store)
        (recsto mt-store (list (numV 20))))
  (test (interp-record empty mt-env mt-store)
        (recsto empty empty)))


;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find [n : Symbol] [ns : (Listof Symbol)] [vs : (Listof Value)])
  : Value
  (cond
   [(empty? ns) (error 'interp "no such field")]
   [else (if (symbol=? n (first ns))
             (first vs)
             (find n (rest ns) (rest vs)))]))

;find tests
(module+ test
  (test (find 'a (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 1))
  (test (find 'b (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 2))
  (test/exn (find 'a empty empty)
            "no such field"))

;Returns the result of an interp in s-expression form
;; interp-expr------------------------------------
(define (interp-expr [e : Exp]) : S-Exp
  (with [(value store) (interp e mt-env mt-store)]
        (cond
          [(numV? value) (number->s-exp (numV-n value))]
          [(closV? value) `function]
          [(boxV? value) `box]
          [(recV? value) `record])))

;tests for interp-expr
(module+ test
  (test (interp-expr (parse `{record {a {+ 1 1}}
                                       {b {+ 2 2}}}))
        `record)
  (test (interp-expr (parse `{+ 1 4}))
        `5)
  (test (interp-expr (parse `{record {a 10} {b {+ 1 2}}}))
        `record)
  (test (interp-expr (parse `{get {record {a 10} {b {+ 1 0}}} b}))
        `1)
  (test/exn (interp-expr (parse `{get {record {a 10}} b}))
            "no such field")
  (test (interp-expr (parse `{get {record {r {record {z 0}}}} r}))
        `record)
  (test (interp-expr (parse `{get {get {record {r {record {z 0}}}} r} z}))
        `0)
  (test (interp-expr (parse `{let {[b {box 0}]}
                               {let {[r {record {a {unbox b}}}]}
                                 {begin
                                   {set-box! b 1}
                                   {get r a}}}}))
        `0)
  (test (interp-expr (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}}))
                `12)

  (test (interp-expr (parse `{lambda {x} {+ x x}}))
        `function)

  (test (interp-expr (parse `{box 5}))
        `box))



;Helper for interping a beginE's list of expressions, making sure to accumulate the
;changes to the Store throughout the process.
;; interp-begin-----------------------------------
(define (interp-begin [expL : (Listof Exp)] [env : Env] [sto : Store]) : Result
  (cond
    [(= 0 (length (rest expL)))
     (interp (first expL) env sto)]
    [else
     (interp-begin (rest expL) env
                   (v*s-s (interp (first expL) env sto)))]))

;note: I tried to make tests for this helper, but without initiating it with a letE it seems that the environment does
;not update properly (even when I tried to manually add to the environment) and I kept getting errors.


(module+ test

  (test/exn (interp (parse `{get {+ 8 7} s}) mt-env mt-store)
        "not a record")
  
  ;Updated tests for interping begin expressions
  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                            {set-box! b {+ 2 {unbox b}}}
                            {set-box! b {+ 3 {unbox b}}}
                            {set-box! b {+ 4 {unbox b}}}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (override-store (cell 1 (numV 10))
                             mt-store)))

  (test (interp (parse `{let {[b {box 0}]}
                          {begin
                            {set-box! b {+ 1 {unbox b}}}
                            {set-box! b {* 10 {unbox b}}}}})
                mt-env
                mt-store)
        (v*s (numV 10)
             (cons (cell 1 (numV 10)) mt-store)))
  
  ;Updated interp tests using new override-store
  (test (interp (parse `{let {[b {box 1}]}
                          {begin
                            {set-box! b 2}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))

  (test (interp (parse `{let {[b {box 5}]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (list (cell 1 (numV 6)))))

  (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (cons (cell 1 (numV 6)) mt-store)))
  
  
  
  (test (interp (parse `2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse `{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse `{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (cons (cell 1 (numV 5))
                   mt-store)))

  
  (test (interp (parse `{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  
  

  (test/exn (interp (parse `{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse `{unbox 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{set-box! 1 2}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
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
  
;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
    [empty 0]
    [(cons c rst-sto) (max (cell-location c)
                           (max-address rst-sto))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
    [empty (error 'interp "unallocated location")]
    [(cons c rst-sto) (if (equal? l (cell-location c))
                          (cell-val c)
                          (fetch l rst-sto))]))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (cons (cell 2 (numV 10))
                                 (cons (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (cons (cell 2 (numV 10))
                                 (cons (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))
(test (interp (parse `{let {[b {box 1}]} 
                          {begin 
                           {set-box! b 2} 
                           {unbox b}}}) 
                mt-env 
                mt-store) 
        (v*s (numV 2) 
             (override-store (cell 1 (numV 2))  
                             mt-store)))