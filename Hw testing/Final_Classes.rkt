#lang plait
(define $$$$$$$$$$$BEGIN$class 'a)
(define-type Exp
  (numE    [n : Number])
  (plusE   [lhs : Exp]
           [rhs : Exp])
  (multE   [lhs : Exp]
           [rhs : Exp])
  (if0E    [cond : Exp]
           [pass : Exp]
           [fail : Exp])
  (castE   [s : Symbol]
           [exp : Exp])
  (newArrayE [t : Type]
             [size : Exp]
             [init : Exp])
  (arrayRefE [arr : Exp]
             [idx : Exp])
  (arraySetE [arr : Exp]
             [idx : Exp]
             [ele : Exp])
  (nullE)
  (argE)
  (thisE)
  (newE    [class-name : Symbol]
           [args : (Listof Exp)])
  (getE    [obj-expr : Exp]
           [field-name : Symbol])
  (sendE   [obj-expr : Exp]
           [method-name : Symbol]
           [arg-expr : Exp])
  (ssendE  [obj-expr : Exp]
           [class-name : Symbol]
           [method-name : Symbol]
           [arg-expr : Exp]))

(define-type Class
  (classC  [field-names : (Listof Symbol)]
           [methods : (Listof (Symbol * Exp))]))

(define-type Value
  (numV [n : Number])
  (objV [class-name : Symbol]
        [field-values : (Listof Value)])
  (nullV)
  (arrayV  [t : Type]
           [vals : (Listof (Boxof Value))]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
  (type-case (Listof (Symbol * 'a)) l
    [empty
     (error 'find (string-append "not found: " (symbol->string name)))]
    [(cons p rst-l)
     (if (symbol=? (fst p) name)
         (snd p)
         (find rst-l name))]))

(module+ test
  (test (find (list (values 'a 1)) 'a)
        1)
  (test (find (list (values 'a 1) (values 'b 2)) 'b)
        2)
  (test/exn (find empty 'a)
            "not found: a")
  (test/exn (find (list (values 'a 1)) 'x)
            "not found: x"))

;; ----------------------------------------
(define (make-list [n : Number] [v : Value] [vals : (Listof (Boxof Value))]) : (Listof (Boxof Value))
  (if (= 0 n)
      vals
      (make-list (- n 1) v (cons (box v) vals))))

(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(nullE) (nullV)]
        [(thisE) this-val]
        [(argE) arg-val]
        [(if0E cond pass fail)
         (type-case Value (recur cond)
           [(numV n)
            (if (= 0 n)
                (recur pass)
                (recur fail))]
           [else (error 'interp "if0 condition NaN")])]
        [(castE s exp) (recur exp)]
        [(newArrayE t size init)
         (type-case Value (recur size)
           [(numV n)
            (let ([initV (recur init)])
              (type-case Value initV
                [(objV name fields)
                 (arrayV t (make-list n initV empty))]
                [else (error 'interp "array value not an object")]))]
           [else (error 'interp "array size NaN")])]
        [(arrayRefE arr idx)
         (type-case Value (recur arr)
           [(arrayV t vals)
            (type-case Value (recur idx)
              [(numV n) (unbox (list-ref vals n))]
              [else (error 'interp "array index NaN")])]
           [else (error 'interp "not an array")])]
        [(arraySetE arr idx ele)
         (type-case Value (recur arr)
           [(arrayV t vals)
            (type-case Value (recur idx)
              [(numV n) (begin
                          (set-box! (list-ref vals n) (recur ele))
                          (numV 0))]
              [else (error 'interp "array index NaN")])]
           [else (error 'interp "not an array")])]
        [(newE class-name field-exprs)
         (local [(define c (find classes class-name))
                 (define vals (map recur field-exprs))]
           (if (= (length vals) (length (classC-field-names c)))
               (objV class-name vals)
               (error 'interp "wrong field count")))]
        [(getE obj-expr field-name)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC field-names methods)
               (find (map2 (lambda (n v) (values n v))
                           field-names
                           field-vals)
                     field-name)])]
           [else (error 'interp "not an object")])]
        [(sendE obj-expr method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (call-method class-name method-name classes
                           obj arg-val)]
             [else (error 'interp "not an object")]))]
        [(ssendE obj-expr class-name method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (call-method class-name method-name classes
                        obj arg-val))]))))


(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC field-names methods)
     (let ([body-expr (find methods method-name)])
       (interp body-expr
               classes
               obj
               arg-val))]))

(define (num-op [op : (Number Number -> Number)]
                [op-name : Symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (values 'Posn
            (classC 
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2))))))))
    
  (define posn3D-class
    (values 'Posn3D
            (classC 
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE)))))))

  (define posn27 (newE 'Posn (list (numE 2) (numE 7))))
  (define posn531 (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (define posn12 (newE 'Posn (list (numE 1) (numE 2))))
  (define posn34 (newE 'Posn (list (numE 3) (numE 4))))
  (define pV (interp-posn posn12))
  (define pV2 (interp-posn posn34))
  (test (interp-posn (newArrayE (objT 'Posn) (numE 5) posn12))
        (arrayV (objT 'Posn) (list (box pV) (box pV) (box pV) (box pV) (box pV))))
  (test (interp-posn (arrayRefE (newArrayE (objT 'Posn) (numE 5) posn12) (numE 2)))
        pV)
  (test (interp-posn (arraySetE (newArrayE (objT 'Posn) (numE 5) posn12) (numE 2) posn34))
        (numV 0))
  
  (let ([arrV (interp-posn (newArrayE (objT 'Posn) (numE 5) posn12))])
    (begin
      (set-box! (list-ref (arrayV-vals arrV) 2) pV2)
      (arrayV-vals arrV)))

  (test (interp-posn (arraySetE (newArrayE (objT 'Posn) (numE 5) posn12) (numE 2) posn34))
        (numV 0))
  (test/exn (interp-posn (arraySetE (numE 1) (numE 2) posn34))
            "not an array")
  (test/exn (interp-posn (arraySetE (newArrayE (objT 'Posn) (numE 5) posn12) posn12 posn34))
            "index NaN")
  (test/exn (interp-posn (arrayRefE (newArrayE (objT 'Posn) (numE 5) posn12) (nullE)))
            "index NaN")
  (test/exn (interp-posn (arrayRefE (nullE) (numE 0)))
            "not an array")
  (test/exn (interp-posn (newArrayE (objT 'Posn) (numE 5) (numE 1)))
            "not an object")
  (test/exn (interp-posn (newArrayE (objT 'Posn) (nullE) posn12))
            "size NaN")
  (test/exn (interp-posn (if0E (nullE) (numE 1) (numE 2)))
            "if0 condition NaN")
  
  (test (interp-posn (if0E (numE 0) (nullE) posn27))
        (nullV))
  (test (interp-posn (if0E (numE 1) (nullE) posn27))
        (objV 'Posn (list (numV 2) (numV 7))))
  (test (interp-posn (castE 'Object posn27))
        (objV 'Posn (list (numV 2) (numV 7))))
  
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0))
        (numV 10))
  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0))
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0))
        (numV 70))

  (test (interp-posn (newE 'Posn (list (numE 2) (numE 7))))
        (objV 'Posn (list (numV 2) (numV 7))))

  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 12))

  (test (interp-posn (sendE (ssendE posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 30))

  (test (interp-posn (sendE posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusE (numE 1) posn27))
            "not a number")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (newE 'Posn (list (numE 0))))
            "wrong field count"))
(define $$$$$$$$$END$class 'a)




(define $$$$$$$$$$BEGIN$inherit 'a)
;#lang plait
;
;;; Make all "class.rkt" definitions available here, where
;;; the "class.rkt" file must be in the same directory
;;; as this one:
;(require "class.rkt")

(define-type ExpI
  (numI      [n : Number])
  (plusI     [lhs : ExpI]
             [rhs : ExpI])
  (multI     [lhs : ExpI]
             [rhs : ExpI])
  (if0I      [cond : ExpI]
             [pass : ExpI]
             [fail : ExpI])
  (castI     [s : Symbol]
             [exp : ExpI])
  (newArrayI [t : Type]
             [size : ExpI]
             [init : ExpI])
  (arrayRefI [arr : ExpI]
             [idx : ExpI])
  (arraySetI [arr : ExpI]
             [idx : ExpI]
             [ele : ExpI])
  (nullI)
  (argI)
  (thisI)
  (newI      [class-name : Symbol]
             [args : (Listof ExpI)])
  (getI      [obj-expr : ExpI]
             [field-name : Symbol])
  (sendI     [obj-expr : ExpI]
             [method-name : Symbol]
             [arg-expr : ExpI])
  (superI    [method-name : Symbol]
             [arg-expr : ExpI]))

(define-type ClassI
  (classI [super-name : Symbol]
          [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * ExpI))]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (exp-i->c [a : ExpI] [super-name : Symbol]) : Exp
  (local [(define (recur expr)
            (exp-i->c expr super-name))]
    (type-case ExpI a
      [(numI n) (numE n)]
      [(plusI l r) (plusE (recur l) (recur r))]
      [(multI l r) (multE (recur l) (recur r))]
      [(if0I cond pass fail) (if0E (recur cond) (recur pass) (recur fail))]
      [(castI s exp) (castE s (recur exp))]
      [(newArrayI t size init) (newArrayE t (recur size) (recur init))]
      [(arrayRefI arr idx) (arrayRefE (recur arr) (recur idx))]
      [(arraySetI arr idx ele) (arraySetE (recur arr) (recur idx) (recur ele))]
      [(nullI) (nullE)]
      [(argI) (argE)]
      [(thisI) (thisE)]
      [(newI class-name field-exprs)
       (newE class-name (map recur field-exprs))]
      [(getI expr field-name)
       (getE (recur expr) field-name)]
      [(sendI expr method-name arg-expr)
       (sendE (recur expr)
              method-name
              (recur arg-expr))]
      [(superI method-name arg-expr)
       (ssendE (thisE)
               super-name
               method-name
               (recur arg-expr))])))

(module+ test
  (define posnI12 (newI 'Posn (list (numI 1) (numI 2))))
  (define posnI34 (newI 'Posn (list (numI 3) (numI 4))))
  
  (test (exp-i->c (arraySetI (newArrayI (objT 'Posn) posnI12 (numI 5)) (numI 2) posnI34) 'Object)
        (arraySetE
         (newArrayE
          (objT 'Posn)
          (newE 'Posn (list (numE 1) (numE 2)))
          (numE 5))
         (numE 2)
         (newE 'Posn (list (numE 3) (numE 4)))))
  
  (test (exp-i->c (arrayRefI (newArrayI (objT 'Posn) posnI12 (numI 5)) (numI 2)) 'Object)
        (arrayRefE (newArrayE (objT 'Posn) (newE 'Posn (list (numE 1) (numE 2))) (numE 5)) (numE 2)))
  (test (exp-i->c (newArrayI (objT 'Posn) (numI 5) (newI 'Posn (list (numI 1) (numI 2)))) 'Object)
        (newArrayE (objT 'Posn) (numE 5) (newE 'Posn (list (numE 1) (numE 2)))))
  (test (exp-i->c (newArrayI (objT 'Posn) (numI 5) (newI 'Posn (list (numI 1) (numI 2)))) 'Object)
        (newArrayE (objT 'Posn) (numE 5) (newE 'Posn (list (numE 1) (numE 2)))))
  (test (exp-i->c (castI 'Object (newI 'Thingie empty)) 'Object)
        (castE 'Object (newE 'Thingie empty)))
  (test (exp-i->c (if0I (numI 0) (nullI) (nullI)) 'Object)
        (if0E (numE 0) (nullE) (nullE)))
  
  (test (exp-i->c (numI 10) 'Object)
        (numE 10))
  (test (exp-i->c (plusI (numI 10) (numI 2)) 'Object)
        (plusE (numE 10) (numE 2)))
  (test (exp-i->c (multI (numI 10) (numI 2)) 'Object)
        (multE (numE 10) (numE 2)))
  (test (exp-i->c (argI) 'Object)
        (argE))
  (test (exp-i->c (thisI) 'Object)
        (thisE))
  (test (exp-i->c (newI 'Object (list (numI 1))) 'Object)
        (newE 'Object (list (numE 1))))
  (test (exp-i->c (getI (numI 1) 'x) 'Object)
        (getE (numE 1) 'x))
  (test (exp-i->c (sendI (numI 1) 'mdist (numI 2)) 'Object)
        (sendE (numE 1) 'mdist (numE 2)))
  (test (exp-i->c (superI 'mdist (numI 2)) 'Posn)
        (ssendE (thisE) 'Posn 'mdist (numE 2))))

;; ----------------------------------------

(define (class-i->c-not-flat [c : ClassI]) : Class
  (type-case ClassI c
    [(classI super-name field-names methods)
     (classC
      field-names
      (map (lambda (m)
             (values (fst m)
                     (exp-i->c (snd m) super-name)))
           methods))]))

(module+ test
  (define posn3d-mdist-i-method
    (values 'mdist
            (plusI (getI (thisI) 'z)
                   (superI 'mdist (argI)))))
  (define posn3d-mdist-c-method
    (values 'mdist
            (plusE (getE (thisE) 'z)
                   (ssendE (thisE) 'Posn 'mdist (argE)))))

  (define posn3d-i-class 
    (values 'Posn3D
            (classI
             'Posn
             (list 'z)
             (list posn3d-mdist-i-method))))
  (define posn3d-c-class-not-flat
    (values 'Posn3D
            (classC (list 'z)
                    (list posn3d-mdist-c-method))))

  (test (class-i->c-not-flat (snd posn3d-i-class))
        (snd posn3d-c-class-not-flat)))

;; ----------------------------------------

(define (flatten-class [name : Symbol]
                       [classes-not-flat : (Listof (Symbol * Class))] 
                       [i-classes : (Listof (Symbol * ClassI))]) : Class
  (type-case Class (find classes-not-flat name)
    [(classC field-names methods)
     (type-case Class (flatten-super name classes-not-flat i-classes)
       [(classC super-field-names super-methods)
        (classC
         (add-fields super-field-names field-names)
         (add/replace-methods super-methods methods))])]))

(define (flatten-super [name : Symbol]
                       [classes-not-flat : (Listof (Symbol * Class))] 
                       [i-classes : (Listof (Symbol * ClassI))]) : Class
  (type-case ClassI (find i-classes name)
    [(classI super-name field-names i-methods)
     (if (equal? super-name 'Object)
         (classC empty empty)
         (flatten-class super-name
                        classes-not-flat
                        i-classes))]))

(module+ test
  (define posn-i-class
    (values
     'Posn
     (classI 'Object
             (list 'x 'y)
             (list (values 'mdist
                           (plusI (getI (thisI) 'x)
                                  (getI (thisI) 'y)))
                   (values 'addDist
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0))))))))
  (define addDist-c-method
    (values 'addDist
            (plusE (sendE (thisE) 'mdist (numE 0))
                   (sendE (argE) 'mdist (numE 0)))))
  (define posn-c-class-not-flat
    (values
     'Posn
     (classC (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x)
                                  (getE (thisE) 'y)))
                   addDist-c-method))))
  (define posn3d-c-class
    (values 'Posn3D
            (classC (list 'x 'y 'z)
                    (list posn3d-mdist-c-method
                          addDist-c-method))))

  (test (flatten-class 'Posn3D
                       (list posn-c-class-not-flat
                             posn3d-c-class-not-flat)
                       (list posn-i-class
                             posn3d-i-class))
        (snd posn3d-c-class)))

;; ----------------------------------------

(define add-fields append)

(define (add/replace-methods [methods : (Listof (Symbol * Exp))]
                             [new-methods : (Listof (Symbol * Exp))])
  : (Listof (Symbol * Exp))
  (cond
    [(empty? new-methods) methods]
    [else (add/replace-methods
           (add/replace-method methods (first new-methods))
           (rest new-methods))]))

(define (add/replace-method [methods : (Listof (Symbol * Exp))] 
                            [new-method : (Symbol * Exp)])
  : (Listof (Symbol * Exp))
  (cond
    [(empty? methods) (list new-method)]
    [else
     (if (equal? (fst (first methods))
                 (fst new-method))
         (cons new-method (rest methods))
         (cons (first methods) 
               (add/replace-method (rest methods)
                                   new-method)))]))

(module+ test
  (test (add-fields (list 'x 'y) (list 'z))
        (list 'x 'y 'z))

  (test (add/replace-methods empty empty)
        empty)
  (test (add/replace-methods empty (list (values 'm (numE 0))))
        (list (values 'm (numE 0))))
  (test (add/replace-methods (list (values 'm (numE 0))) empty)
        (list (values 'm (numE 0))))
  (test (add/replace-methods (list (values 'm (numE 0)))
                             (list (values 'm (numE 1))))
        (list (values 'm (numE 1))))
  (test (add/replace-methods (list (values 'm (numE 0))
                                   (values 'n (numE 2)))
                             (list (values 'm (numE 1))))
        (list (values 'm (numE 1))
              (values 'n (numE 2))))
  (test (add/replace-methods (list (values 'm (numE 0)))
                             (list (values 'm (numE 1))
                                   (values 'n (numE 2))))
        (list (values 'm (numE 1))
              (values 'n (numE 2))))

  (test (add/replace-method (list (values 'm (numE 0)))
                            (values 'm (numE 1)))
        (list (values 'm (numE 1))))
  (test (add/replace-method (list (values 'm (numE 0)))
                            (values 'n (numE 2)))
        (list (values 'm (numE 0))
              (values 'n (numE 2)))))

;; ----------------------------------------

(define (interp-i [i-a : ExpI] [i-classes : (Listof (Symbol * ClassI))]) : Value
  (local [(define a (exp-i->c i-a 'Object))
          (define classes-not-flat
            (map (lambda (i)
                   (values (fst i)
                           (class-i->c-not-flat (snd i))))
                 i-classes))
          (define classes
            (map (lambda (c)
                   (let ([name (fst c)])
                     (values name
                             (flatten-class name classes-not-flat i-classes))))
                 classes-not-flat))]
    (interp a classes (objV 'Object empty) (numV 0))))

(module+ test
  (test (interp-i (numI 0) empty)
        (numV 0))

  (test (interp-i
         (sendI (newI 'Posn3D (list (numI 5) (numI 3) (numI 1)))
                'addDist
                (newI 'Posn (list (numI 2) (numI 7))))
         (list posn-i-class
               posn3d-i-class))
        (numV 18)))


(define $$$$$$$$END$inherit 'a)


;#lang plait
;
;(require "class.rkt"
;         "inherit.rkt")

(define $$$$$$$$$BEGIN$typeclass 'a)
(define-type ClassT
  (classT [super-name : Symbol]
          [fields : (Listof (Symbol * Type))]
          [methods : (Listof (Symbol * MethodT))]))

(define-type MethodT
  (methodT [arg-type : Type]
           [result-type : Type]
           [body-expr : ExpI]))

(define-type Type
  (numT)
  (nullT)
  (objT [class-name : Symbol])
  (arrayT [t : Type]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'Object)
      empty        
      (type-case ClassT (find t-classes class-name)
        [(classT super-name fields methods)
         (append 
          (get-all-field-types super-name t-classes)
          (map snd fields))])))

;; ----------------------------------------

(define (make-find-in-tree class-items)
  (lambda (name class-name t-classes)
    (local [(define t-class (find t-classes class-name))
            (define items (class-items t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'Object)
          (find items name)
          (try (find items name)
               (lambda ()
                 ((make-find-in-tree class-items)
                  name 
                  super-name
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree classT-fields))

(define find-method-in-tree
  (make-find-in-tree classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) #t]
    [(equal? name1 'Object) #f]
    [else
     (type-case ClassT (find t-classes name1)
       [(classT super-name fields methods)
        (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [(objT name1)
     (type-case Type t2
       [(nullT) #t]
       [(objT name2)
        (is-subclass? name1 name2 t-classes)]
       [else #f])]
    [(nullT)
     (type-case Type t2
       [(numT) #f]
       [else #t])]
    [else (equal? t1 t2)]))

;;Find the least upper bound, fortunately if t1 and t2 are objects, we can just recur on t2's chain of superclasses,
;;since the relationship is reflexive.
(define (least-upper-bound [exp : ExpI] [t1 : Type] [t2 : Type] [t-classes : (Listof (Symbol * ClassT))]) : Type
  (type-case Type t1
    [(numT)
     (type-case Type t2
       [(numT) (numT)]
       [else (type-error exp "object")])]
    [(objT cname1)
     (type-case Type t2
       [(objT cname2)
        (if (is-subclass? cname1 cname2 t-classes)
            t2
            (let ([class2 (find t-classes cname2)])
              (least-upper-bound
               exp
               t1
               (objT (classT-super-name class2))
               t-classes)))]
       [(nullT) t2]
       [else (type-error exp "object")])]
    [(nullT)
     (type-case Type t2
       [(numT) (type-error exp "object")]
       [else (nullT)])]
    [(arrayT at1)
     (type-case Type t2
       [(arrayT at2) (arrayT (least-upper-bound exp at1 at2 t-classes))]
       [else (type-error exp "array with valid subtypes")])]))


;Obj <- A <- B <- C
;Obj <- D <- E <- F <- G <- H
;LUB(H, E) -> E
;LUB(A, C) -> A
;LUB(H, C) -> Object
;;Tests for least upper bound and typecheck for if0
;;Tests for typecast for cast
;;Tests for typecheck for null
(module+ test
  (define a-t-class (values 'A (classT 'Object empty empty)))
  (define b-t-class (values 'B (classT 'A empty empty)))
  (define c-t-class (values 'C (classT 'B empty empty)))
  (define abcL (list a-t-class b-t-class c-t-class))
  
  (define d-t-class (values 'D (classT 'Object empty empty)))
  (define e-t-class (values 'E (classT 'D empty empty)))
  (define f-t-class (values 'F (classT 'E empty empty)))
  (define g-t-class (values 'G (classT 'F empty empty)))
  (define h-t-class (values 'H (classT 'G empty empty)))

  (define i-t-class (values 'I (classT 'nil empty empty)))
  (define defghL (list d-t-class e-t-class f-t-class g-t-class h-t-class))

  (test/exn (least-upper-bound (numI -1) (objT 'H) (numT) defghL)
            "no type")
  (test/exn (least-upper-bound (numI -1) (numT) (objT 'H) defghL)
            "no type")

  (test/exn (least-upper-bound (numI -1) (objT 'H) (objT 'I) (cons i-t-class defghL))
            "not found")

  (test (least-upper-bound (numI -1) (numT) (numT) defghL)
        (numT))
  
  (test (least-upper-bound (numI -1) (objT 'H) (objT 'E) defghL)
        (objT 'E))
  (test (least-upper-bound (numI -1) (objT 'E) (objT 'H) defghL)
        (objT 'E))

  (test (least-upper-bound (numI -1) (objT 'A) (objT 'C) abcL)
        (objT 'A))
  (test (least-upper-bound (numI -1) (objT 'C) (objT 'A) abcL)
        (objT 'A))

  (test (least-upper-bound (numI -1) (objT 'H) (objT 'C) (append abcL defghL))
        (objT 'Object))
  (test (least-upper-bound (numI -1) (objT 'H) (objT 'C) (append abcL defghL))
        (objT 'Object))

  (test (typecheck (if0I (numI 0) (newI 'H empty) (newI 'C empty)) (append abcL defghL))
        (objT 'Object))
  (test (typecheck (if0I (numI 0) (newI 'H empty) (newI 'C empty)) (append abcL defghL))
        (objT 'Object))
  (test (typecheck (if0I (numI 0) (newI 'A empty) (newI 'C empty)) (append abcL defghL))
        (objT 'A))

  (test/exn (typecheck (if0I (newI 'B empty) (newI 'A empty) (newI 'C empty)) (append abcL defghL))
            "no type")
  (test/exn (typecheck (if0I (numI 0) (newI 'A empty) (newI 'I empty)) (cons i-t-class abcL))
            "if0E conditions must have subtype relationship")

  (test (typecheck (castI 'A (newI 'B empty)) abcL)
        (objT 'A))
  (test (typecheck (castI 'A (newI 'C empty)) abcL)
        (objT 'A))
  (test/exn (typecheck (castI 'C (newI 'A empty)) abcL)
            "no type")
  (test/exn (typecheck (castI 'C (newI 'H empty)) (append abcL defghL))
            "no type")
  (test/exn (typecheck (castI 'C (numI 5)) abcL)
            "no type")
  
  (test (is-subclass? 'Object 'Object empty)
        #t)
  (test (is-subclass? 'A 'B (list a-t-class b-t-class))
        #f)
  (test (is-subclass? 'B 'A (list a-t-class b-t-class))
        #t)

  (test (is-subtype? (numT) (numT) empty)
        #t)
  (test (is-subtype? (numT) (objT 'Object) empty)
        #f)
  (test (is-subtype? (objT 'Object) (numT) empty)
        #f)
  (test (is-subtype? (objT 'A) (objT 'B) (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (objT 'B) (objT 'A) (list a-t-class b-t-class))
        #t))

;; ----------------------------------------

(define typecheck-expr : (ExpI (Listof (Symbol * ClassT)) Type Type -> Type)
  (lambda (expr t-classes this-type arg-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes this-type arg-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [(numT)
                 (type-case Type (recur r)
                   [(numT) (numT)]
                   [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExpI expr
        [(numI n) (numT)]
        [(plusI l r) (typecheck-nums l r)]
        [(multI l r) (typecheck-nums l r)]
        [(newArrayI t size init)
         (let ([initT (recur init)])
           (if (is-subtype? initT t t-classes)
               (arrayT t)
               (type-error expr "array with valid subtype")))]
        [(arrayRefI arr idx)
         (type-case Type (recur arr)
           [(arrayT t)
            (type-case Type (recur idx)
              [(numT) t]
              [else (type-error expr "valid index")])]
           [else (type-error expr "array")])]
        [(arraySetI arr idx ele)
         (type-case Type (recur arr)
           [(arrayT t)
            (if (is-subtype? t (recur ele) t-classes)
                (if (equal? (numT) (recur idx))
                    t
                    (type-error expr "valid index"))
                (type-error expr "array with valid subtype"))]
           [else (type-error expr "array")])]
        [(if0I cond pass fail)
         (let ([cType (recur cond)])
           (if (equal? cType (numT))
               ;;If objects have inheritances that can't be found, make the find error
               ;;from least-upper-bound more meaningful
               (try
                (least-upper-bound pass (recur pass) (recur fail) t-classes)
                (lambda () (error 'typecheck-expr "if0E conditions must have subtype relationship")))
               (type-error cond "num")))]
        [(castI s exp) (type-case Type (recur exp)
                         [(objT name)
                          (if (is-subtype? (objT name) (objT s) t-classes)
                              (objT s)
                              (type-error exp "subtypes"))]
                         [else (type-error exp "object")])]
        [(nullI) (nullT)]
        [(argI) arg-type]
        [(thisI) this-type]
        [(newI class-name exprs)
         (local [(define arg-types (map recur exprs))
                 (define field-types
                   (get-all-field-types class-name t-classes))]
           (if (and (= (length arg-types) (length field-types))
                    (foldl (lambda (b r) (and r b))
                           #t
                           (map2 (lambda (t1 t2) 
                                   (is-subtype? t1 t2 t-classes))
                                 arg-types
                                 field-types)))
               (objT class-name)
               (type-error expr "field type mismatch")))]
        [(getI obj-expr field-name)
         (type-case Type (recur obj-expr)
           [(objT class-name)
            (find-field-in-tree field-name
                                class-name
                                t-classes)]
           [else (type-error obj-expr "object")])]
        [(sendI obj-expr method-name arg-expr)
         (local [(define obj-type (recur obj-expr))
                 (define arg-type (recur arg-expr))]
           (type-case Type obj-type
             [(objT class-name)
              (typecheck-send class-name method-name
                              arg-expr arg-type
                              t-classes)]
             [else
              (type-error obj-expr "object")]))]
        [(superI method-name arg-expr)
         (local [(define arg-type (recur arg-expr))
                 (define this-class
                   (find t-classes (objT-class-name this-type)))]
           (typecheck-send (classT-super-name this-class)
                           method-name
                           arg-expr arg-type
                           t-classes))]))))

(define (typecheck-send [class-name : Symbol]
                        [method-name : Symbol]
                        [arg-expr : ExpI]
                        [arg-type : Type]
                        [t-classes : (Listof (Symbol * ClassT))])
  (type-case MethodT (find-method-in-tree
                      method-name
                      class-name
                      t-classes)
    [(methodT arg-type-m result-type body-expr)
     (if (is-subtype? arg-type arg-type-m t-classes)
         result-type
         (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (Listof (Symbol * ClassT))]) : ()
  (type-case MethodT method
    [(methodT arg-type result-type body-expr)
     (if (is-subtype? (typecheck-expr body-expr t-classes
                                      this-type arg-type)
                      result-type
                      t-classes)
         (values)
         (type-error body-expr (to-string result-type)))]))

(define (check-override [method-name : Symbol]
                        [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (Listof (Symbol * ClassT))])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree method-name
                                  super-name
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string method-name))))))

(define (typecheck-class [class-name : Symbol]
                         [t-class : ClassT]
                         [t-classes : (Listof (Symbol * ClassT))])
  (type-case ClassT t-class
    [(classT super-name fields methods)
     (map (lambda (m)
            (begin
              (typecheck-method (snd m) (objT class-name) t-classes)
              (check-override (fst m) (snd m) t-class t-classes)))
          methods)]))

(define (typecheck [a : ExpI]
                   [t-classes : (Listof (Symbol * ClassT))]) : Type
  (begin
    (map (lambda (tc)
           (typecheck-class (fst tc) (snd tc) t-classes))
         t-classes)
    (typecheck-expr a t-classes (objT 'Object) (numT))))

;; ----------------------------------------
;;Tests for null typechecks
(module+ test
  (define posn-t-class
    (values 'Posn
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'addDist
                                  (methodT (objT 'Posn) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))))))

  (define posn3D-t-class 
    (values 'Posn3D
            (classT 'Posn
                    (list (values 'z (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT)
                                           (plusI (getI (thisI) 'z) 
                                                  (superI 'mdist (argI)))))))))

  (define square-t-class 
    (values 'Square
            (classT 'Object
                    (list (values 'topleft (objT 'Posn)))
                    (list))))

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class)))
  
  (define new-posn27 (newI 'Posn (list (numI 2) (numI 7))))
  (define new-posn531 (newI 'Posn3D (list (numI 5) (numI 3) (numI 1))))

  (define posnArr (newArrayI (objT 'Posn) (numI 5) new-posn27))
  (define posn3DArr (newArrayI (objT 'Posn3D) (numI 5) new-posn531))

  (test (typecheck-posn (newArrayI (nullT) (numI 5) (nullI)))
        (arrayT (nullT)))
  (test (typecheck-posn posnArr)
        (arrayT (objT 'Posn)))
  (test (typecheck-posn (newArrayI (arrayT (objT 'Posn)) (numI 5) posnArr))
        (arrayT (arrayT (objT 'Posn))))
  (test (typecheck-posn (arraySetI posnArr (numI 2) (newI 'Posn (list (numI 3) (numI 4)))))
        (objT 'Posn))
  
  (test/exn (typecheck-posn (arraySetI (nullI) (numI 2) (newI 'Posn (list (numI 3) (numI 4)))))
            "no type")
  (test/exn (typecheck-posn (arraySetI posnArr (nullI) (newI 'Posn (list (numI 3) (numI 4)))))
            "no type")
  (test/exn (typecheck-posn (arraySetI posnArr (numI 2) (numI 7)))
            "no type")
  (test/exn (typecheck-posn (newArrayI (arrayT (objT 'Posn)) (numI 5) (numI 3)))
            "no type")
  (test (typecheck-posn (arrayRefI posnArr (numI 3)))
        (objT 'Posn))
  (test/exn (typecheck-posn (arrayRefI posnArr (nullI)))
            "no type")
  (test/exn (typecheck-posn (arrayRefI (nullI) (numI 3)))
            "no type")
  (test/exn (typecheck-posn (arrayRefI (nullI) (numI 3)))
            "no type")
  (test (typecheck-posn (arrayRefI (newArrayI (arrayT (objT 'Posn)) (numI 5) posnArr) (numI 2)))
        (arrayT (objT 'Posn)))

  (test (least-upper-bound
         (numI -1)
         (arrayT (objT 'Posn))
         (arrayT (objT 'Posn3D))
         (list posn-t-class posn3D-t-class))
        (arrayT (objT 'Posn)))

  (test/exn (least-upper-bound
             (numI -1)
             (arrayT (objT 'Posn))
             (arrayT (numT))
             (list posn-t-class posn3D-t-class))
            "no type")

  (test/exn (least-upper-bound
             (numI -1)
             (arrayT (objT 'Posn))
             (nullT)
             (list posn-t-class posn3D-t-class))
            "no type")
  
  
  
  ;;Objects should always be subtypes of null
  (test (is-subtype? (nullT) (nullT) empty) #t)
  (test (is-subtype? (nullT) (objT 'Object) empty) #t)
  (test (is-subtype? (objT 'Object) (nullT) empty) #t)
  (test (is-subtype? (nullT) (numT) empty) #f)
  (test (is-subtype? (numT) (nullT) empty) #f)

  
  (define testC
    (values 'Test
            (classT 'Object
                    (list (values 'x (objT 'val1)) (values 'y (objT 'val1)))
                    (list (values 'somemethod
                                  (methodT (objT 'val) (numT)
                                           (plusI (numI 1) (numI 1))))))))

  ;You can make objects and pass in null fields
  (test (typecheck-posn (newI 'Square (list (nullI))))
        (objT 'Square))
  (test (typecheck (newI 'Test (list (nullI) (nullI))) (cons testC empty))
        (objT 'Test))

  ;You can pass in null instead of an object if the method doesn't need it
  (test (typecheck (sendI (newI 'Test (list (nullI) (nullI))) 'somemethod (nullI)) (cons testC empty))
        (numT))
  ;You can get a reference to a field even if it was constructed with null
  (test (typecheck (getI (newI 'Test (list (nullI) (nullI))) 'x) (cons testC empty))
        (objT 'val1))
  

  ;realized I had to include LUB cases for null now, the LUB of any object and null is null
  (test (least-upper-bound (nullI) (nullT) (nullT) empty)
        (nullT))
  (test (least-upper-bound (nullI) (objT 'O) (nullT) empty)
        (nullT))
  (test (least-upper-bound (nullI) (nullT) (objT 'O) empty)
        (nullT))
  (test/exn (least-upper-bound (nullI) (nullT) (numT) empty)
            "no type")
  (test/exn (least-upper-bound (nullI) (numT) (nullT) empty)
            "no type")

  ;now if0 can have either branch be null or an object
  (test (typecheck (if0I (numI 0) (nullI) (nullI)) empty)
        (nullT))
  (test (typecheck-posn (if0I (numI 0) (nullI) new-posn27))
        (nullT))
  (test (typecheck-posn (if0I (numI 0) new-posn27 (nullI)))
        (nullT))

  ;but null cannot be used for if0 condition
  (test (typecheck-posn (castI 'Posn new-posn531))
        (objT 'Posn))
  (test/exn (typecheck-posn (castI 'Posn3D new-posn27))
            "no type")

  
  
  (test (typecheck-posn (sendI new-posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI new-posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI new-posn531 'addDist new-posn27))
        (numT))  
  (test (typecheck-posn (sendI new-posn27 'addDist new-posn531))
        (numT))

  (test (typecheck-posn (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1))))))
        (objT 'Square))

  (test (typecheck-posn (newI 'Square (list (newI 'Posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'Square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI new-posn27 'mdist new-posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'Object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class
                             (values 'Other
                                     (classT 'Posn
                                             (list)
                                             (list (values 'mdist
                                                           (methodT (objT 'Object) (numT)
                                                                    (numI 10))))))))
            "bad override")
  (test/exn (typecheck-method (methodT (numT) (objT 'Object) (numI 0)) (objT 'Object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (values 'Cube
                                     (classT 'Square
                                             empty
                                             (list
                                              (values 'm
                                                      (methodT (numT) (numT)
                                                               ;; No such method in superclass:
                                                               (superI 'm (numI 0)))))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [(classT super-name fields methods)
       (classI
        super-name
        (map fst fields)
        (map (lambda (m)
               (values (fst m)
                       (type-case MethodT (snd m)
                         [(methodT arg-type result-type body-expr)
                          body-expr])))
             methods))])))
  
(define interp-t : (ExpI (Listof (Symbol * ClassT)) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map (lambda (c)
                     (values (fst c) (strip-types (snd c))))
                   t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI new-posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI new-posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI new-posn531 'addDist new-posn27))
        (numV 18))
  (test (interp-t-posn (sendI new-posn27 'addDist new-posn531))
        (numV 18)))

(define $$$$$$$$$END$typeclass 'a)