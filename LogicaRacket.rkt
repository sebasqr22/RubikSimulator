#lang racket

(require "c2.rkt" "c1.rkt")

(define (filas matriz contador)
  (cond [(null? matriz) contador]
        [else (filas (cdr matriz) (+ contador 1))]))

(define (columnas matriz contador veces cantidad1)
  (cond [(null? matriz) cantidad1]
        [(equal? veces 0) (columnas (cdr matriz) (+ 1 contador) 1 (filas (car matriz) 0))]
        [else
         (cond [(equal? (filas (car matriz) 0) cantidad1) (columnas (cdr matriz) (+ 1 contador) 1 cantidad1)]
               [else #f])]))
;;---------------------------------
;; ((0 0 0 0 0 0 0 0 0) blanco 
;; (1 1 1 1 1 1 1 1 1) naranja 
;; (2 2 2 2 2 2 2 2 2) amarillo 
;; (3 3 3 3 3 3 3 3 3) rojo
;; (4 4 4 4 4 4 4 4 4) verde
;; (5 5 5 5 5 5 5 5 5)) azul

;; 3x3 '((0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1) (2 2 2 2 2 2 2 2 2) (3 3 3 3 3 3 3 3 3) (4 4 4 4 4 4 4 4 4) (5 5 5 5 5 5 5 5 5))
;; 2x2 '((0 0 0 0) (1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5))
;; 4x4 '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3) (4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4) (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5))


(define (validarCubo filas columnas)
  (cond [(equal? filas columnas) #t]
        [else #f]))


(define (movimientosAux estadoIni listaMovs filas columnas)
  (cond [(equal? (validarCubo filas columnas)
                 #t)]
        [else #f]))
  
         
(define (movimientos estadoIni listaMovs)
  (cond [(equal? (columnas estadoIni 0 0 0) #f) #f]
        [else (movimientosAux estadoIni (filas estadoIni 0) (columnas estadoIni 0 0 0))]))