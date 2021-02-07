#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza

EQUIPO: VeryBlueBerries
**** Teresa Becerril Torres 315045132
**** Miguel Angel Torres Sanchez 315300442
**** Maria del Pilar Sanchez Benitez 315239674 
|#

(provide (all-defined-out))

;; Definition of source language
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (list (l))
   (string (s))
   (type (t)))
  (Expr (e body)
    x
    pr
    c
    l
    s
    t
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (lambda ([x* t*] ...) body* ... body)
    (let ([x* t* e*] ...) body* ... body)
    (letrec ([x* t* e*] ...) body* ... body)
    (e0 e1 ...)
    (primapp pr e* ...)
    (list e* ...)
    (define x e)
    (while [e0] e1)
    (for [x e0] e1)))

;; Predicate for the variables
(define (variable? x)
  (and (symbol? x)
       (not (primitive? x))
       (not (constant? x))))

;; Predicate for the types
;; Bool | Int | Char | List | Lambda | List of T | T → T
(define (type? x) (or (b-type? x) (c-type? x)))

;; For Bool, Int, Char and List types
(define (b-type? x) (memq x '(Bool Char Int List Lambda)))

;; For List of T and T → T types
(define (c-type? x) (if (list? x)
        (let* (
                [f (car x)]
                [s (cadr x)]
                [t (caddr x)])
        (or (and (equal? f 'List) (equal? s 'of) (type? t))
            (and (type? f) (equal? s '→) (type? t))))
        #f))

;; Predicate for the constants
(define (constant? x)
  (or (boolean? x) (number? x) (char? x)))

;; Predicate for primitives
(define (primitive? x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/) (equal? x 'and) (equal? x 'or) (equal? x 'not)
      (equal? x 'length) (equal? x 'car) (equal? x 'cdr)))


;; The parser of LF
(define-parser parse-LF LF)

;; -------------------------------------- REMOVE-ONE-ARMED-IF -------------------------------------

;; Language LNI definition 
;; LNI extends LF, removes one-armed if statements
(define-language LNI (extends LF)
  (Expr (e body)
        (- (if e0 e1))
        ))

; LNI parser
(define-parser parse-LNI LNI)

; Pass that removes one-armed if statements
(define-pass remove-one-armed-if : LF(ir) -> LNI()
  (Expr : Expr (ir) -> Expr()
        [(if ,[e0] ,[e1])
         `(if ,e0 ,e1 (void))]))

;; ----------------------------------------- REMOVE-STRING ----------------------------------------

;; LNS extends LNI, removes strings as terminals
(define-language LNS (extends LNI)
  (terminals
   (- (string (s))))
  (Expr (e body)
        (- s)))

;; LNS parser
(define-parser parse-LNS LNS)

;; Pass that defines a preprocess to remove strings as terminals in our language
(define-pass remove-string : LNI(ir) -> LNS()
  (Expr : Expr (ir) -> Expr()
        [(,s) (let ([str (string->list s)])
                (build-list `(list ,(car str)) (cdr str)))]))

;; Auxiliary recursive function for the remove-string pass.
;; Given a list expression from LNS and a list of LNS expressions
;; returns a new list from LNS with all the expressions of both lists.
(define (build-list expr elems)
  (nanopass-case (LNS Expr) expr
                 [(list ,e* ...) (if (empty? elems)
                                     (with-output-language (LNS Expr) `(list ,e* ...)) ;; If there's no elements left to add, return the list expr.
                                     (build-list (with-output-language (LNS Expr) `(list ,e* ... ,(car elems))) (cdr elems)))] ;; Add the first element of elems and recursion.
                 [else (error "Expected list expression.")]))

#| Examples for remove-string
(remove-string (parse-LNI `("hola")))
Answer: (language:LNS '(list #\h #\o #\l #\a))

(remove-string (parse-LNI `("Esto es un string.")))
Answer: (language:LNS '(list #\E #\s #\t #\o #\space #\e #\s #\space #\u #\n #\space #\s #\t #\r #\i #\n #\g #\.))
|#

;; ------------------------------------------ CURRY-LET -------------------------------------------

;; Language L7 definition 
;; L7 extends LNS, removes let and letrec from multiple assignments and 
;; adds let and letrec from a single assignment
(define-language L7
  (extends LNS)
  (Expr (e body)
        (- (let ([x* t* e*] ...) body* ... body)
           (letrec ([x* t* e*] ...) body* ... body))
        (+ (let ([x* t* e*]) body* ... body)
           (letrec ([x* t* e*]) body* ... body))))

;; L7 parser
(define-parser parser-L7 L7)

;; Pass definition for currying let and letrec expressions.
(define-pass curry-let : LF (e) -> L7 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x ,t ,[e]] [,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
         (let f ([x x] [t t] [e e] [x* x*] [t* t*] [e* e*] [body* body*] [body body])
           (if (null? x*)
               `(let ([,x ,t ,e]) ,body* ... ,body)
               `(let ([,x ,t ,e]) ,(f (car x*) (car t*) (car e*) (cdr x*) (cdr t*) (cdr e*) body* body))))]
        [(letrec ([,x ,t ,[e]][,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
         (let f ([x x] [t t] [e e] [x* x*] [t* t*] [e* e*] [body* body*] [body body])
           (if (null? x*)
               `(letrec ([,x ,t ,e]) ,body* ... ,body)
               `(letrec ([,x ,t ,e]) ,(f (car x*) (car t*) (car e*) (cdr x*) (cdr t*) (cdr e*) body* body))))]))

;; --------------------------------------- IDENTIFY-ASSIGMENT -------------------------------------

;; Pass that identifies let expressions used to define functions
;; and replaces them for letrec.
(define-pass identify-assignments : L7 (e) -> L7 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x* ,t* ,[e*]]) ,[body*] ... ,[body])
           (if (equal? t* 'Lambda)
               `(letrec ([,x* ,t* ,e*]) ,(identify-assignments body))
               `(let ([,x* ,t* ,e*]) ,body* ... ,body))]))


;; ------------------------------------------- UN-ANONYMOUS ---------------------------------------

;; Language L8 defintion
;; L8 extends L7, add named functions
(define-language L8
  (extends L7)
  (Expr (e body)
        (+ (letfun ([x* t* e*]) body))))

(define-parser parser-L8 L8)

;; Returns the number of num-letfun expressions that appear in the expression
(define (num-letfun exp n)
  (nanopass-case (L8 Expr) exp
   [(letfun ([,x ,t ,e]) ,body) (num-letfun e (+ n 1))]
   [(lambda ([,x* ,t*] ...) ,body* ... ,body) (num-letfun body n)]
   [else n]))

;;Pass define a lambda as a letfun expression
(define-pass nameLambda : L7 (e) -> L8()
  (Expr : Expr(e) -> Expr()
        [(lambda ([,x* ,t*] ...) ,body* ... ,body)
         `(letfun ([foo0 ,'Lambda (lambda ([,x* ,t*]...) ,(nameLambda body))]) foo0)]))

;;Auxiliary funcion create a new foo
(define (newf x n)
  (string->symbol(string-append "foo" (number->string (+ n 1)))))

;;Pass create a new foo every letfun
(define-pass ecfoo : L8(e) -> L8()
  (Expr : Expr(e) -> Expr()
        [(letfun ([,x ,t ,e]) ,body)
         `(letfun ([,(newf x (num-letfun e 0)) ,t ,(ecfoo e)]),(newf x (num-letfun e 0)))]))

;;Given a lambda it returns a letfun expression
(define-pass un-anonymous : L7(e) -> L8()
  (Expr : Expr (e) -> Expr()
        [(lambda ([,x* ,t*] ...) ,body* ... ,body)
         `,(ecfoo (nameLambda e))]))

;; (un-anonymous (parser-L7 '(lambda ([x Bool]) (if x 1 2))))
;; Desired response (language:L8 '(letfun ((foo1 Lambda (lambda ((x Bool)) (if x 1 2)))) foo1))
;; (un-anonymous (parser-L7 '(lambda ([y Int]) (lambda ([x Bool]) (if x 1 y)))))
;; Desired response (language:L8 '(letfun ((foo2 Lambda (lambda ((y Int)) (letfun ((foo1 Lambda (lambda ((x Bool)) (if x 1 y)))) foo1)))) foo2))
;; (un-anonymous (parser-L7 '(lambda ([x Bool]) (lambda ([y Int]) (lambda ([z Int]) (if x z y))))))
;; Desired response (language:L8 '(letfun ((foo3 Lambda (lambda ((x Bool)) (letfun ((foo2 Lambda (lambda ((y Int)) (letfun ((foo1 Lambda (lambda ((z Int)) (if x z y)))) foo1)))) foo2)))) foo3))

;; ----------------------------------------- VERIFY-ARITY -----------------------------------------

;; Pass that verifies the arity of a primitive operations
;; Checks if arity is at least 2 for binary operations.
;; Checks if airty is 1 for not and list operations.
(define-pass verify-arity : L8 (e) -> L8 ()
  (Expr : Expr (e) -> Expr ()
        [(primapp ,pr ,[e*] ... ,[e])
         (match pr
           ['+ (if (> (length e*) 0)
                   `(primapp + ,e* ... ,e)
                   (error "Arity mismatch in + expression"))]
           ['- (if (> (length e*) 0)
                   `(primapp - ,e* ... ,e)
                   (error "Arity mismatch in - expression"))]
           ['* (if (> (length e*) 0)
                   `(primapp * ,e* ... ,e)
                   (error "Arity mismatch in * expression"))]
           ['/ (if (> (length e*) 0)
                   `(primapp / ,e* ... ,e)
                   (error "Arity mismatch in / expression"))]
           ['and (if (> (length e*) 0)
                   `(primapp and ,e* ... ,e)
                   (error "Arity mismatch in and expression"))]
           ['or (if (> (length e*) 0)
                   `(primapp or ,e* ... ,e)
                   (error "Arity mismatch in or expression"))]
           ['not (if (equal? (length e*) 0)
                     `(primapp not ,e* ... ,e)
                     (error "Arity mismatch in not expression"))]
           ['length (if (equal? (length e*) 0)
                     `(primapp length ,e* ... ,e)
                     (error "Arity mismatch in length expression"))]
           ['car (if (equal? (length e*) 0)
                     `(primapp car ,e* ... ,e)
                     (error "Arity mismatch in car expression"))]
           ['cdr (if (equal? (length e*) 0)
                     `(primapp cdr ,e* ... ,e)
                     (error "Arity mismatch in cdr expression"))])]))

;; ----------------------------------------- VERIFY-VARS ------------------------------------------

;; Function that returns the list of free variables from an expression in L7
(define (free-vars e)
  (nanopass-case (L8 Expr) e
                 [,x (list x)]
                 [(begin ,e* ... ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(primapp ,pr ,e* ... ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(if ,e0 ,e1 ,e2) (append (free-vars e0) (free-vars e1) (free-vars e2))]
                 [(list ,e* ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(lambda ([,x* ,t*] ... ) ,body* ... ,body) (remove* x* (append (append-map free-vars body*) (free-vars body)))]
                 [(let ([,x* ,t* ,e*]) ,body* ... ,body) (remove* (list x*) (append (free-vars e*) (append-map free-vars body*) (free-vars body)))]
                 [(letrec ([,x* ,t* ,e*]) ,body* ... ,body) (remove* (list x*) (append (free-vars e*) (append-map free-vars body*) (free-vars body)))]
                 [(letfun ([,x* ,t* ,e*]) ,body) (remove* (list x*) (append (free-vars e*)(free-vars body)))]
                 [(define ,x ,e) (free-vars e)]
                 [(while [,e0] ,e1) (append (free-vars e0) (free-vars e1))]
                 [(for [,x ,e0] ,e1) (append (free-vars e0) (remove* (list x)(free-vars e1)))]
                 [else '()]))

;; Pass that verifies that expressions in L8 don't contain free variables
(define-pass verify-vars : L8 (e) -> L8 ()
  (Expr : Expr (e) -> Expr ()
        [,x (error "Free variable: " x)]
        [,c c]
        [else (let* ([l (free-vars e)])
                (if (empty? l)
                    e
                    (error "Free variables: " (remove-duplicates l))))]))
