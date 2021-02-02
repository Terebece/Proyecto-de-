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

(require "Middle-end.rkt")

;; (array len type [e* ... ])
(define-language L12
  (extends L11)
  (Expr (e body)
        (- (list e* ...))
        (+ (array c t [e* ...]))))

(define-parser parse-L12 L12)


(define-pass list-to-array : L11 (ir) -> L12 ()
  (Expr : Expr (ir) -> Expr()
        [(list ,e* ...) `(array ,(len e*) ,(J (list e*) '()) [,e*])]))


(define (c expr)
  (nanopass-case (L12 Expr) expr
                 [(const ,t ,c) (match t
                                  [Int (number->string c)])]
                 [(primapp ,pr ,e* ...) (match pr
                                          ['+ (string-append (c (first e*)) "+" (c (second e*)))]
                                          ['- (string-append (c (first e*)) "-" (c (second e*)))])]))