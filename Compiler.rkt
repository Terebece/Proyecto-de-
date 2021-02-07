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

(require racket/pretty)

;; File with Front-end processes
(require "Front-end.rkt")
;; File with Middle-end processes
(require "Middle-end.rkt")
;; File with Back-end processes
(require "Back-end.rkt")

;; Ruta del archivo que se va a compilar
;(define path "ejemplo1")
(define path "ejemplo2")
;(define path "ejemplo3")

;; Lee el archivo especificado
(define read-file
  (read (open-input-file (string-append path ".mt"))))

;; Crea y escribe en un archivo lo que obtuvo de aplicar los procesos
;; correspondientes a una etapa de compilación
(define (write-file exp path)
  (with-output-to-file path
   (lambda () (printf "~a" exp))
   #:mode 'text #:exists 'replace))

;; Parsea el contenido del archivo
(define exp-lf (parse-LF read-file))

;; Compila el archivo que se le especifica.
(define (compilador exp)
  (write-file (pretty-format (processes-Front-end exp)) (string-append path ".fe"))
  (write-file (pretty-format (processes-Middle-end exp)) (string-append path ".me"))
  ;(write-file (pretty-format ((processes-Back-end exp)) (string-append path ".c"))
  )

;; Aplica los procesos de la etapa de Front-end
(define (processes-Front-end exp)
  (verify-vars
   (verify-arity
    (un-anonymous
     (identify-assignments
      (curry-let
       (remove-string
         (remove-one-armed-if exp))))))))

;; Aplica los procesos de la etapa de Middle-end
(define (processes-Middle-end exp)
  (uncurry
   (type-infer
    (type-const
     (curry
      (quote-c (processes-Front-end exp)))))))


;; Aplica los procesos de la etapa de Back-end
(define (processes-Back-end exp)
  (c (list-to-array (processes-Middle-end exp))))

;; Imprime en la consola los pasos del compilador
(compilador exp-lf)
(println "Entrada:")
exp-lf
(println "Front-end:")
(processes-Front-end exp-lf)
(println "Middle-end:")
(processes-Middle-end exp-lf)
;;(println "Back-end:")
;;(processes-Back-end exp-lf)
