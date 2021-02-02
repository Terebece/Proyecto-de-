#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza

EQUIPO: VeryBlueBerries
**** Teresa Becerril Torres 315045132
**** Miguel Ángel Torres Sánchez 315300442
**** Maria del Pilar Sanchez Benitez 315239674 
|#
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
    (list e* ...)))

;; Predicate for the variables
(define (variable? x)
  (symbol? x))

;; Predicate for the types
;; Bool | Int | Char | List | Lambda
(define (type? x) (or (b-type? x) (c-type? x)))

;; For Bool, Int, Char and List types
(define (b-type? x) (memq x '(Bool Char Int List Lambda)))

;; For Lambda type
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


; The parser of LF
(define-parser parse-LF LF)

#| Language LI definition
   Extends LF
   * removes one-armed if statements
|#
(define-language LNI (extends LF)
  (Expr (e body)
        (- (if e0 e1))
        ))

; LNI parser
(define-parser parse-LNI LNI)

; Paass that removes one-armed if statements
(define-pass remove-one-armed-if : LF(ir) -> LNI()
  (Expr : Expr (ir) -> Expr()
        [(if ,[e0] ,[e1])
         `(if ,e0 ,e1 (void))]))

;--------- REMOVE-STRING-------

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


;--------- CURRY-LET-----------
(define-language L7
  (extends LF)
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

;--------- IDENTIFY-ASSIGMENT--

(define-pass identify-assignments : L7 (e) -> L7 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x* ,t* ,[e*]]) ,[body*] ... ,[body])
           (if (equal? t* 'Lambda)
               `(letrec ([,x* ,t* ,e*]) ,(identify-assignments body))
               `(let ([,x* ,t* ,e*]) ,body* ... ,body))]))


;--------- UN-ANONYMOUS--------

;; L8 defintion
;; Extends L7, add named functions
(define-language L8
  (extends L7)
  (Expr (e body)
        (+ (letfun ([x t e]) body))))

(define-parser parser-L8 L8)

;; Dada una lambda regresa una expresion letfun
(define-pass nameLambda : L8 (e) -> L8()
  (Expr : Expr(e) -> Expr()
        [(lambda ([,x* ,t*] ...) ,body* ... ,body)
         `(letfun [foo ,'Lambda (,e)] foo)]))

;;Funcion auxiliar crear un nuevo foo cada letfun
;;(define-pass newfoo : L8(e) -> L8()
  ;(Expr : Expr (e) -> Expr()
   ;     [(letfun [,x ,t ,e] ,body)
    ;     `(letfun [,(+ x 1) ,t ,e] (newFoo ,body))]))

(define-pass un-anonymous : L8(e) -> L8()
  (Expr : Expr (e) -> Expr ()))


;--------- VERIFY-ARITY -------

;; Pass that verifies the arity of a primitive operations
;; Checks if the arity is at least 2.
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

;--------- VERIFY-VARS --------

;; Function that returns the list of free variables from an expression in L7
(define (free-vars e)
  (nanopass-case (L8 Expr) e
                 [,x (list x)]
                 [(begin ,e* ... ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(primapp ,pr ,e* ... ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(if ,e0 ,e1) (append (free-vars e0) (free-vars e1))]
                 [(if ,e0 ,e1 ,e2) (append (free-vars e0) (free-vars e1) (free-vars e2))]
                 ;[(and ,e ,e* ...) (append (free-vars e) (append-map free-vars e*))]
                 ;[(or ,e ,e* ...) (append (free-vars e) (append-map free-vars e*))]
                 ;[(not ,e) (free-vars e)]
                 [(list ,e* ,e) (append (append-map free-vars e*) (free-vars e))]
                 [(lambda ([,x* ,t*] ... ) ,body* ... ,body) (remove* x* (append (append-map free-vars body*) (free-vars body)))]
                 [(let ([,x* ,t* ,e*]) ,body* ... ,body) (remove* (list x*) (append (free-vars e*) (append-map free-vars body*) (free-vars body)))]
                 [(letrec ([,x* ,t* ,e*]) ,body* ... ,body) (remove* (list x*) (append (free-vars e*) (append-map free-vars body*) (free-vars body)))]
                 ;[(letfun ([,x* ,t* ,e*]) body) ...]
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
