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
;; Este predicado es incorrecto, ya que no corresponde a nuestra gramática actual de tipos.
;; Faltan los tipos función (→) y los tipos lista.
(define (type? x)
  (or (equal? x 'Bool) (equal? x 'Int) (equal? x 'Char) (equal? x 'List) (equal? x 'String) (equal? x 'Lambda)))

;; Predicate for the constants
(define (constant? x)
  (or (boolean? x) (number? x) (char? x)))

;; Predicate for primitives
(define (primitive? x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/) (equal? x 'and) (equal? x 'or) (equal? x 'not)
      (equal? x 'length) (equal? x 'car) (equal? x 'cdr)))


; The parser of LF
(define-parser parse-LF LF)

#| Extencion del lenguaje LF
   * se elimina el if de una sola rama
|#
(define-language LNI (extends LF)
  (Expr (e body)
        (- (if e0 e1))
        ))
;Definimos el parser para el nuevo lenguaje LNI
(define-parser parse-LNI LNI)

; Definimos un preproceso para eliminar el if
(define-pass remove-one-armed-if : LF(ir) -> LNI()
  (Expr : Expr (ir) -> Expr()
        [(if ,[e0] ,[e1])
         `(if ,e0 ,e1 (void))]))

;--------- REMOVE-STRING-------

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

;--------- VERIFY-ARITY -------


(define-pass verify-arity : L7 (e) -> L7 ()
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
  (nanopass-case (L7 Expr) e
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
                 [else '()]))

;; Definition of the pass that verifies that expressions in L7 don't contain free variables
(define-pass verify-vars : L7 (e) -> L7 ()
  (Expr : Expr (e) -> Expr ()
        [,x (error "Free variable: " x)]
        [,c c]
        [else (let* ([l (free-vars e)])
                (if (empty? l)
                    e
                    (error "Free variables: " (remove-duplicates l))))]))
