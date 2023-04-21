#lang racket

; Hansi Seitaj, CMPSC 460, Final Project

;; Define a custom `type-of` procedure that returns the type of an argument
(define type-of
  (lambda (x)
    (cond
      ((null? x) 'null)
      ((pair? x) 'pair)
      ((vector? x) 'vector)
      ((symbol? x) 'symbol)
      ((string? x) 'string)
      ((integer? x) 'integer)
      ((real? x) 'real)
      ((char? x) 'char)
      ((boolean? x) 'boolean)
      ((procedure? x) 'procedure)
      (else (error "Unknown type" x)))))

;; Define a method table
(define method-table (make-hash))

;; Define a procedure to add a method to the method table
(define (add-method name types f)
  (let ((key (cons name types)))
    (let ((fun (hash-ref method-table key #f)))
      (if fun
          (error 'add-method
                 "Method already defined for ~a with argument types ~a"
                 name types)
          (hash-set! method-table key f)))))

;; The butlast function takes a list as input and returns a new list
;; that contains all the elements of the original list except for the last element.
(define (butlast lst)
  (take lst (- (length lst) 1)))

;; Test cases for butlast with non-empty lists
(equal? (butlast '(1 2 3 4 5)) '(1 2 3 4))
(equal? (butlast '(1 2 3)) '(1 2))
(equal? (butlast '(a b c d)) '(a b c))


;; Define a procedure to find the best matching method for a given set of arguments
(define (find-method name args)
  (let ((types (map type-of args)))
    (let loop ((types types))
      (let ((key (cons name types)))
        (let ((fun (hash-ref method-table key #f)))
          (if fun
              fun
              (if (null? types)
                  (error 'find-method
                         "No matching method for ~a with argument types ~a"
                         name types)
                  (loop (butlast types)))))))))

;; Define a procedure to implement the generic function
(define (define-generic name)
  (let ((table method-table))
    (lambda args
      (let ((fun (find-method name args)))
        (apply fun args)))))

;; Define the `add` function with multiple dispatch
(define add (define-generic 'add))
(add-method 'add '(integer integer) (lambda (x y) (+ x y)))
(add-method 'add '(real real) (lambda (x y) (+ x y)))

;; Define the `add-str` function with multiple dispatch
;(define add-str (define-generic 'add-str))
(add-method 'add-str '(string string) (lambda (x y) (string-append x y)))
(add-method 'add-str '() (lambda () "")) 

;; Test cases for add with integer arguments
(equal? (add 1 2) 3)
(equal? (add -1 3) 2)
(equal? (add 0 0) 0)

;; Test cases for add with float arguments
(equal? (add 1.1 2.1) 3.2)
(equal? (add -1.0 3.0) 2.0)
(equal? (add 0.0 0.0) 0.0) 

;; Define the `add-str` function with multiple dispatch
(define add-str (define-generic 'add-str))
(add-method 'add '(string string) (lambda (x y) (string-append x y)))

;; Test cases for add-str with string arguments
(equal? (add "hello" "world") "helloworld")
(equal? (add "Racket" " is cool") "Racket is cool")
(equal? (add "" "") "")

;; Test case for type-of with null argument
(equal? (type-of '()) 'null)

;; Test case for type-of with pair argument
(equal? (type-of '(1 2 3)) 'pair)

;; Test case for type-of with vector argument
(equal? (type-of #(1 2 3)) 'vector)

;; Test case for type-of with symbol argument
(equal? (type-of 'a) 'symbol)

;; Test case for type-of with string argument
(equal? (type-of "hello") 'string)

;; Test case for type-of with integer argument
(equal? (type-of 42) 'integer)

;; Test case for type-of with real argument
(equal? (type-of 3.14) 'real)

;; Test case for type-of with char argument
(equal? (type-of #\a) 'char)

;; Test case for type-of with boolean argument
(equal? (type-of #t) 'boolean)

;; Test case for type-of with procedure argument
(equal? (type-of (lambda (x) (+ x 1))) 'procedure)

;; Test case for add with integer arguments
(equal? (add 1 2) 3)
(equal? (add -1 3) 2)
(equal? (add 0 0) 0)

;; Test case for add with float arguments
(equal? (add 1.1 2.1) 3.2)
(equal? (add -1.0 3.0) 2.0)
(equal? (add 0.0 0.0) 0.0)

;; Test case for add-str with string arguments
(equal? (add-str "hello" "world") "helloworld")
(equal? (add-str "Racket" " is cool") "Racket is cool")
(equal? (add-str "" "") "")
;; Test cases for add-str with no arguments
(equal? (add-str) "")

;; Test cases that will fail and return error
;(equal? (add 1.1 "2.1") 3.2)
;(equal? (add 1.1 `2.1) 3.2)