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

;; File with Front-end processes
(require "Front-end.rkt")
;; File with Middle-end processes
(require "Middle-end.rkt")
;; File with Back-end processes
(require "Back-end.rkt")

;; Ruta del archivo que se va a compilar
(define path "ejemplo1.mt")
;(define path "ejemplo2.mt")
;(define path "ejemplo3.mt")

;; Lee el archivo especificado
(define read-file
  (read (open-input-file path)))

;; Escribe en un archivo lo que obtuvo de aplicar los procesos
;; correspondientes a una etapa de compilación
(define (write-file exp path)
  (with-output-to-file path
   (lambda () (printf "~a" exp))
   #:mode 'text #:exists 'replace))
  

