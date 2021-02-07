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
        [(list ,[e*] ...) `(array ,(length e*) ,(typeof (go-back ir)) [,e*])]))

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


;;(list-to-array (parse-L11'(list (const Int 0) (const Int 1) (const Int 2) (const Int 3))))

;; ---------------------------------------------- C -----------------------------------------------

(define (c expr)
  (nanopass-case (L12 Expr) expr
                 [(const ,t ,c) (match t
                                  ['Int (number->string c)]
                                  ['Bool (match c
                                          ['#t (string-append "true")]
                                          ['#f (string-append "false")])]
                                  ['Char (string c)])]
                 [,x (string-append (symbol->string x))]
                 [,t (match t
                       ['Int (string-append "int")]
                       ['Bool (string-append "bool")]
                       ['Char (string-append "char")]
                       ['List (string-append "array")])]
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
                                          ;['cdr ()]
                                          )]
                 [(begin ,[e*] ... ,e) (display (string-append "{"(c(first e*))"} \n {"((rest e*))"}"))]
                 [(if ,e0 ,e1 ,e2) (if (void? e2)
                                       (string-append "if" "("(c e0)")" (c e1) ";")
                                       (display (string-append "if" (string #\space) "("(c e0)")" (string #\space) (c e1) ";"
                                         "\n" "else" (string #\space)(c e2)";")))]
                 [(let ([,x ,t ,e]) ,body) (string-append (symbol->string t)(string #\space)(symbol->string (parse-L12 x)) "="(string #\space)  (c e) ";   "
                                            (c body))]
                 [(letrec ([,x ,t ,e]) ,body) (display (string-append (symbol->string t)(string #\space)(symbol->string (parse-L12 x)) "("(c e)")" ";\n"
                                                                     "{" (c body) "}"))]
                 [(letfun ([,x ,t ,e]) ,body) (display (string-append (symbol->string t) (symbol->string (parse-L12 x)) "("(c e) ")" ";\n"
                                                             "{"  (c body) "}"))]
                 #|[(array ,c0 ,t [,e* ...]) (string-append
                                            (symbol->string t) "["(number->string c0)"]"(string #\space) "=" (string #\space) "{"
                                            (for* ([j e*])
                                             (c (first e*)) ",") "};")] |#
  
                [(array ,c0 ,t [,e* ...]) (string-append
                                            (symbol->string t) "["(number->string c0)"]"(string #\space) "=" (string #\space) "{"
                                                          (let f ([e* e*]) 
                                                            (if (null? e*)
                                                                ""
                                                                 (string-append (c (first e*)) ","(f (rest e*))  )) )"};")]
                 [(,e0 ,e1) (display (string-append (c e0)";\n"(c e1)";"))]))

;----------- Examples for C--------------
;;(c (parse-L12 '(if (const Bool #t) (primapp + (const Int 2) (const Int 2)) (primapp - (const Int 2) (const Int 2)))))
; Return : "if(true)2+2;
;           else2-2;"
;; (c (parse-L12 '(let ([x Int (const Int 3)]) (primapp + x (const Int 4)))))
; RReturn : "Int  x=3;    x+4"
;; (c (parse-L12 '(array 4 Int [(const Int 1) (const Int 1) (const Int 1)(const Int 1)])))
; return "Int[4] = {1,1,1,1};"
                                      
                                        