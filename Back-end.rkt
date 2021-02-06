#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solis
Laboratorio: Fernando Abigail Galicia Mendoza

EQUIPO: VeryBlueBerries
**** Teresa Becerril Torres 315045132
**** Miguel Angel Torres Sanchez 315300442
**** Maria del Pilar Sanchez Benitez 315239674 
|#

(require "Middle-end.rkt")
(require "Front-end.rkt")
(provide (all-defined-out))

;; ----------------------------------------- LIST-TO-ARRAY ----------------------------------------

;; L12 defintion
;; (array len type [e* ... ])
(define-language L12
  (extends L11)
  (Expr (e body)
        (- (list e* ...))
        (+ (array c t [e* ...]))))

;; L12 parser
(define-parser parse-L12 L12)

;; Pass that transforms a list into an array with length, the elements' type,
;; and elements of the list.
(define-pass list-to-array : L11 (ir) -> L12 ()
  (Expr : Expr (ir) -> Expr()
        [(list ,e* ...) `(array ,(length e*) ,(typeof ir) [,e*])]))

;; Function that given a list expression in L11
;; returns the type of the elements of the list.
(define (typeof expr)
  (let ([t (J (go-back expr) '())]) 
    (if (c-type? t)
        (let* ([l (car t)]
               [s (cadr t)]
               [lt (caddr t)])
          (if (and (equal? l 'List) (equal? s 'of))
              lt
              (error "No lista.")))
        (error "No lista."))))

;; Pass that changes an expression of L11 back to L10.
;; Curry lambda expressions.
(define-pass go-back : L11 (ir) -> L10 ()
  (Expr : Expr (ir) -> Expr ()
        [(lambda ([,x* ,t*] ...) ,[body])
         (let f ([bindingx* x*]
                 [bindingt* t*])
           (if (equal? (length bindingx*) 1)
               `(lambda ([,(car bindingx*) ,(car bindingt*)]) ,body)
               `(lambda ([,(car bindingx*) ,(car bindingt*)]) ,(f (cdr bindingx*) (cdr bindingt*)))))]))


;;(list-to-array (parse-L11'(list (const Int 0) (const Int 0) (const Int 0) (const Int 0))))

;; ---------------------------------------------- C -----------------------------------------------

(define (c expr)
  (nanopass-case (L12 Expr) expr
                 [(const ,t ,c) (match t
                                  ['Int (number->string c)]
                                  ['Bool (match c
                                          ['#t (string-append "true")]
                                          ['#f (string-append "false")])]
                                  ['Char (string c)])]
                 [(primapp ,pr ,e* ...) (match pr
                                          ['+ (string-append (c (first e*)) "+" (c (second e*)))]
                                          ['- (string-append (c (first e*)) "-" (c (second e*)))]
                                          ['* (string-append (c (first e*)) "*" (c (second e*)))]
                                          ['/ (string-append (c (first e*)) "/" (c (second e*)))]
                                          ['not ("!" (string-append (c (first e*))))]
                                          ['and (string-append (c (first e*)) "&&" (c (second e*)))]
                                          ['or (string-append (c (first e*)) "||" (c (second e*)))]
                                          ['length (let ([e (first e*)])
                                                     (string-append "sizeof(" (c e) ")/sizeof(" (c (parse-L12 `(car e))) ")"))]
                                          ['car (let ([e (first e*)])
                                                  (nanopass-case (L12 Expr) e
                                                                 [,x (string-append (symbol->string x) "[0]")]
                                                                 [(array ,c0 ,t [,e*]) (string-append (c e) "[0]")]))]
                                          ;['cdr ()])]
                                          )]
                 [(begin ,[e*] ... ,e) ((let f ([e* e*])
                                            (if (null? e*)
                                                `((string-append "{""}"))
                                                `((string-append "{"(c(first e*))"}" "\n" "{"(f (rest e*))"}")))))]
                 [(if ,e0 ,e1 ,e2) (if (void? e2)
                                       `((string-append "if" "("(c (e0))")" (c (e1)) ";"))
                                       `((string-append "if" "("(c (e0))")" (c (e1)) ";" "\n" "else"(c (e2))";")))]
                 [(let ([,x ,t ,e]) ,body) ((string-append (c (const x t)) ";" "\n" (c (e)) ";"))]
                 [(letrec ([,x ,t ,e]) ,body) ((string-append (c (t)) (c (x)) "("(c (e))")"";") (c(body)))]
                 [(letfun ([,x ,t ,e]) ,body) ((string-append (c (t)) (c (x)) "("(c (e))")"";") (c(body)))]
                 [(array ,c0 ,t [,[e*] ...]) (let f ([e* e*])
                                            (if (null? e*)
                                              (string-append (c t)  "[" (c c0)"]" "=" "{""}")
                                              (string-append (c t)  "[" (c c0)"]" "=" "{"(c (first e*)) ","(f (rest e*))"}")))
                                           ]
                 [(,e0 ,e1) ((string-append (c (e0))";" "\n" (c(e1))";"))]))
                                      
                                        