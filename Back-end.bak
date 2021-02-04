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
        [(list ,[e*] ...) `(array ,(length e*) ,(typeof ir) [,e*])]
        ))

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


(define (c expr)
  (nanopass-case (L12 Expr) expr
                 [(const ,t ,c) (match t
                                  [Int (number->string c)])]
                 [(primapp ,pr ,e* ...) (match pr
                                          ['+ (string-append (c (first e*)) "+" (c (second e*)))]
                                          ['- (string-append (c (first e*)) "-" (c (second e*)))])]))