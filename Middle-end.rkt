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

(require "Front-end.rkt")
(provide (all-defined-out))

;; -------------------------------------------- CURRY ---------------------------------------------

;; Language L9 definition
(define-language L9
  (extends L8)
  (Expr (e body)
        (- c
           (lambda ([x* t*] ...) body* ... body)
           (e0 e1 ...))
        (+ (quot c)
           (lambda ([x t]) body)
           (e0 e1))))

;; Language L9 parser
(define-parser parse-L9 L9)

;; Pass to convert constants c into '(quot c)
(define-pass quote-c : L8 (ir) -> L9 ()
  (Expr : Expr (e) -> Expr ()
        [(,c) `(quot ,c)]))

;; Pass for currying lambda expressions and function applications
(define-pass curry : L8 (ir) -> L9 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x* ,t*] ...) ,[body])
         (let f ([bindingx* x*]
                 [bindingt* t*])
           (if (equal? (length bindingx*) 1)
               `(lambda ([,(car bindingx*) ,(car bindingt*)]) ,body)
               `(lambda ([,(car bindingx*) ,(car bindingt*)]) ,(f (cdr bindingx*) (cdr bindingt*)))))]
        [(,[e0] ,[e1] ...)
         (let f ([be0 e0]
                 [be1 e1])
           (if (equal? (length be1) 0)
               `,be0
               (f `(,be0 ,(car be1)) (cdr be1))))]))

;; ------------------------------------------ TYPE-CONST ------------------------------------------

;; Language L10 definition
(define-language L10
  (extends L9)
  (Expr (e body)
        (- (quot c))
        (+ (const t c))))

;; Language L10 parser
(define-parser parse-L10 L10)

;; Pass that adds type anotations to language's constants and global vars.
(define-pass type-const : L9 (ir) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [(quot ,c)
         (cond
           [(boolean? c) `(const Bool ,c)]
           [(number? c) `(const Int ,c)]
           [(char? c) `(const Char ,c)])]))

;; ------------------------------------------ TYPE-INFER ------------------------------------------

;; Function that verifies if a type t1 is unfiable with a type t2.
;; It doesn't return the unifier, it returns a boolean.
(define (unify t1 t2)
	(if (and (type? t1) (type? t2))
		(cond 
			[(equal? t1 t2) #t]
			[(and (equal? 'List t1) (list? t2)) (equal? (car t2) 'List)]
			[(and (equal? 'List t2) (list? t1)) (equal? (car t1) 'List)]
			[(and (list? t1) (list? t2)) (and (unify (car t1) (car t2)) (unify (caddr t1) (caddr t2)))]
			[else #f])
		(error "Se esperaban 2 tipos")))


;; --------------------------------- J Algorithm ---------------------------------

;; Implementation of function J for L10.
;; Returns the type of a L10 expression.
(define (J expr ctx)
  (nanopass-case (L10 Expr) expr
                 [,x (get x ctx)]
                 [(const ,t ,c) t]
                 [(begin ,e* ... ,e)
                  (let f ([e* e*] [nctx ctx])
                    (let* ([r (car e*)]
                           [s (J r nctx)])
                      ;; If there's a decalaration for a global variable we need to add it to the context
                      (nanopass-case (L10 Expr) r
                                     [(define ,x ,e0)
                                        (if (equal? (length e*) 1)
                                          (J e (set-add nctx (cons x s)))
                                          (f (cdr e*) (set-add nctx (cons x s))))]
                                     [else (if (equal? (length e*) 1)
                                               (J e nctx)
                                               (f (cdr e*) nctx))])))]
                 ;; We check with every primitive operator
                 [(primapp ,pr ,e* ... ,e)
                  (case pr
                    [(car)
                     (let ([t0 (J e ctx)])
                       (if (c-type? t0)
                           (let ([f (car t0)]
                                 [s (cadr t0)]
                                 [t1 (caddr t0)])
                             (if (and (equal? f 'List) (equal? s 'of) (type? t1))
                                 t1
                                 (error "El argumento de 'cdr' no es una lista.")))
                       (error "El argumento de 'cdr' no es una lista.")))]
                    [(cdr)
                     (let ([t0 (J e ctx)])
                       (if (c-type? t0)
                           (let ([f (car t0)]
                                 [s (cadr t0)]
                                 [t1 (caddr t0)])
                             (if (and (equal? f 'List) (equal? s 'of) (type? t1))
                                 t0
                                 (error "El argumento de 'cdr' no es una lista.")))
                       (error "El argumento de 'cdr' no es una lista.")))]
                    [(length)
                     (let ([t0 (J e ctx)])
                       (if (c-type? t0)
                           (let ([f (car t0)]
                                 [s (cadr t0)]
                                 [t1 (caddr t0)])
                             (if (and (equal? f 'List) (equal? s 'of) (type? t1))
                                 'Int
                                 (error "El argumento de 'length' no es una lista.")))
                       (error "El argumento de 'length' no es una lista.")))]
                    [(not)
                     (if (equal? (length e*) 0)
                         (let ([t (J e ctx)])
                           (if (and (type? t) (equal? t 'Bool))
                               t
                               (error "El argumento de 'not' no es booleano.")))
                         (error "Cantidad de argumentos de 'not' invalida."))]
                    [(+ - * /)
                     (if (> (length e*) 0)
                      (let f ([e* e*])
                        (let ([t (J (car e*) ctx)])
                          (if (equal? (length e*) 1)
                              (if (and (type? t) (equal? t 'Int) (equal? t (J e ctx)))
                                  t
                                  (error "El tipo de uno o más argumentos no es Int."))
                              (if (and (type? t) (equal? t 'Int))
                                  (f (cdr e*))
                                  (error "El tipo de uno o más argumentos no es Int.")))))
                       (error "No hay suficientes argumentos."))]
                    [(and or)
                     (if (> (length e*) 0)
                      (let f ([e* e*])
                        (let ([t (J (car e*) ctx)])
                          (if (equal? (length e*) 1)
                              (if (and (type? t) (equal? t 'Bool) (equal? t (J e ctx)))
                                  t
                                  (error "El tipo de uno o más argumentos no es booleano."))
                              (if (and (type? t) (equal? t 'Bool))
                                  (f (cdr e*))
                                  (error "El tipo de uno o más argumentos no es booleano.")))))
                       (error "F: No hay suficientes argumentos."))]
                    [else (error "F: No se está usando un operador primitivo.")])]
                 
                 [(if ,e0 ,e1 ,e2)
                  (let ([t0 (J e0 ctx)]
                        [t1 (J e1 ctx)]
                        [t2 (J e2 ctx)])
                    (if (and (unify t0 'Bool) (unify t1 t2))
                        t1
                        (error "F: Las ramas del if no tienen el mismo tipo y/o la guarda no es booleana.")))]
                 [(lambda ([,x ,t]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [type (J body ctxN)])
                    `(,t → ,type))]
                 [(let ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctx)]
                         [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Los tipos de la varible y su asignación son distintos en 'let'.")))]
                 [(letrec ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctxN)]
                         [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Los tipos de la varible y su asignación son distintos en 'let'.")))]
                 [(letfun ([,x ,t ,e]) ,body)
                  (let* ([ctxN (set-add ctx (cons x t))]
                         [t0 (J e ctx)]
                         [t1 (J body ctxN)])
                    (if (unify t t0)
                        t1
                        (error "Tipos función de 'letfun' son incorrectos.")))]
                 [(list ,e* ...)
                  (if (empty? e*)
                      'List
                      (let ([t (J (car e*) ctx)])
                        (let f ([e* e*])
                          (let ([ti (J (car e*) ctx)])
                            (if (unify t ti)
                                (if (equal? (length e*) 1)
                                    (list 'List 'of t)
                                    (f (cdr e*)))
                                (error "La lista no es homogenea."))))))]
                 [(,e0 ,e1)
                  (let* ([t0 (J e0 ctx)]
                         [t1 (J e1 ctx)])
                    (if (list? t0)
                        (if (unify (car t0) t1)
                            (second t0)
                            (error "F: El dominio y la entrada de la función son distintas. :'v"))
                        (error "F: El primer parámetro no es una función. :c")))]
                 [(define ,x ,e) (J e ctx)]
                 [(while [,e0] ,e1) (J e1 ctx)]
                 [(for [,x ,e0] ,e1)
                  (let ([t (typeof e0)])
                    (J e1 (set-add ctx (cons x t))))]
                 ))


;; Function 'get' that gets the type of a variable given a context.
(define (get x ctx)
  (if (empty? ctx)
      (error "No existe una declaración de la variable dentro del contexto.")
      (let ([d (car ctx)])
        (if (equal? x (car d))
            (cdr d)
            (get x (cdr d))))))

;; Function that given a list expression in L10
;; returns the type of the elements of the list.
(define (typeof expr)
  (let ([t (J expr '())]) 
    (if (c-type? t)
        (let* ([l (car t)]
               [s (cadr t)]
               [lt (caddr t)])
          (if (and (equal? l 'List) (equal? s 'of))
              lt
              (error "No es una lista.")))
        (error "No es una lista."))))

;; -------- Examples for the J algorithm -------------
;; (J (parse-L10 '(primapp or (primapp not (const Bool #t)) (const Bool #f))) '())
;; Returns: 'Bool

;; (J (parse-L10 '(primapp car (list (const Int 6) (const Bool #t)))) '())
;; Returns: "La lista no es homogenea."

;; (J (parse-L10 '(primapp cdr (list (const Int 6) (const Int 7)))) '())
;; Returns: '(List of Int)

;; (J (parse-L10 '(letfun ([foo (Bool → Int) (lambda ([x Bool]) (if x (const Int 1) (const Int 2)))]) foo)) '())
;; Returns: '(Bool → Int)

;; (J (parse-L10 '(begin (const Int 2) (primapp car (list (const Int 6) (const Bool #t))) (const Bool #t))) '())
;; Returns: "La lista no es homogenea."

;; (J (parse-L10 `(for [x (list (const Int 4) (const Int 7) (const Int 5))] (primapp + x (const Int 1)))) '())
;; Returns: 'Int
;; ---------------------------------------------------

;; Pass for L10 that substitutes type Lambda references for type T->T
;; and type List references for type (List of T) if necessary.
(define-pass type-infer : L10 (ir) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [(letrec ([,x ,t ,e]) ,body)
         (let ([s (J e '())])
           `(letrec ([,x ,s ,e]) ,body))]
        [(letfun ([,x ,t ,e]) ,body)
         (let ([s (J e '())])
           `(letfun ([,x ,s ,e]) ,body))]
        [(let ([,x ,t ,e]) ,body)
         (if (and (b-type? t) (equal? t 'List))
             (let ([s (J e '())])
               `(let ([,x ,s ,e]) ,body))
             `(let ([,x ,t ,e]) ,body))]))

;; ------------------------------------------- UNCURRY --------------------------------------------

;; Language L11 definition
(define-language L11
  (extends L10)
  (Expr (e body)
        (- (lambda ([x t]) body))
        (+ (lambda ([x* t*] ...) body))))

;; Predicate for lambda expressions.
(define-pass lambda? : (L10 Expr) (e) -> * (bool)
  (Expr : Expr (e) -> * (bool)
    [(lambda ([,x ,t]) ,body) #t]
    [else #f])
  (Expr e))

;; Language L11 parser
(define-parser parse-L11 L11)

;; Pass that uncurryfies lambda expressions from L10.
(define-pass uncurry : L10 (ir) -> L11 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x ,t]) ,body)
         (if (lambda? body)
             (let* ([asignaciones (car (saca-cosas body))]
                    [ax (car asignaciones)]
                    [at (cdr asignaciones)]
                    [puerquito (cdr (saca-cosas body))])
               `(lambda ([,x ,t] [,ax ,at]) ,(uncurry puerquito)))
             `(lambda ([,x ,t]) ,(uncurry body)))]))

;; Function that given a lambda expression from L10 returns a pair
;; made from the assignments and the body of the expression.
(define (saca-cosas expr)
        (nanopass-case (L10 Expr) expr
                       [(lambda ([,x ,t]) ,body) (cons (cons x t) body)]
                       [else (error "Expected lambda expression.")]))
