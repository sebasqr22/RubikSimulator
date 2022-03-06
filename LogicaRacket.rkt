#lang racket

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

(define (position matriz i j)
  (list-ref (list-ref matriz i) j))

(define (c1b matriz nueva contador type);; se mueve 0, 3, 6
  (cond [(equal? type 3)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list (position matriz 3 0) (position matriz 0 1) (position matriz 0 2) (position matriz 3 3) (position matriz 0 4) (position matriz 0 5) (position matriz 3 6) (position matriz 0 7) (position matriz 0 8)) nueva) (+ 1 contador) type)] ;; blanca
               [(equal? contador 1)
                (c1b matriz (cons (list (position matriz 0 0) (position matriz 1 1) (position matriz 1 2) (position matriz 0 3) (position matriz 1 4) (position matriz 1 5) (position matriz 0 6) (position matriz 1 7) (position matriz 1 8)) nueva) (+ 1 contador) type)];; naranja
               [(equal? contador 2)
                (c1b matriz (cons (list (position matriz 1 0) (position matriz 2 1) (position matriz 2 2) (position matriz 1 3) (position matriz 2 4) (position matriz 2 5) (position matriz 1 6) (position matriz 2 7) (position matriz 2 8)) nueva) (+ 1 contador) type)];; amarillo
               [(equal? contador 3)
                (c1b matriz (cons (list (position matriz 2 0) (position matriz 3 1) (position matriz 3 2) (position matriz 2 3) (position matriz 3 4) (position matriz 3 5) (position matriz 2 6) (position matriz 3 7) (position matriz 3 8)) nueva) (+ 1 contador) type)];; rojo
               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)];; verde
               [(equal? contador 5)
                (c1b matriz (cons (list (position matriz 5 6) (position matriz 5 3) (position matriz 5 0) (position matriz 5 7) (position matriz 5 4) (position matriz 5 1) (position matriz 5 8) (position matriz 5 5) (position matriz 5 2)) nueva) (+ 1 contador) type)];;azul
               [else (reverse nueva)])]
        
        [(equal? type 2)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list (position matriz 3 0) (position matriz 0 1) (position matriz 3 2) (position matriz 0 3)) nueva) (+ 1 contador) type)] ;;blanco
               [(equal? contador 1)
                (c1b matriz (cons (list (position matriz 0 0) (position matriz 1 1) (position matriz 0 2) (position matriz 1 3)) nueva) (+ 1 contador) type)] ;;naranja
               [(equal? contador 2)
                (c1b matriz (cons (list (position matriz 1 0) (position matriz 2 1) (position matriz 1 2) (position matriz 2 3)) nueva) (+ 1 contador) type)] ;amarillo
               [(equal? contador 3)
                (c1b matriz (cons (list (position matriz 2 0) (position matriz 3 1) (position matriz 2 2) (position matriz 3 3)) nueva) (+ 1 contador) type)] ;;roja
               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde
               [(equal? contador 5)
                (c1b matriz (cons (list (position matriz 5 2) (position matriz 5 0) (position matriz 5 3) (position matriz 5 1)) nueva) (+ 1 contador) type)] ;;azul
               [else (reverse nueva)])]

        [(equal? type 4)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list (position matriz 3 0) (position matriz 0 1) (position matriz 0 2) (position matriz 0 3) (position matriz 3 4) (position matriz 0 5) (position matriz 0 6) (position matriz 0 7) (position matriz 3 8) (position matriz 0 9) (position matriz 0 10) (position matriz 0 11) (position matriz 3 12) (position matriz 0 13) (position matriz 0 14) (position matriz 0 15)) nueva) (+ 1 contador) type)] ;;blanco
               [(equal? contador 1)
                (c1b matriz (cons (list (position matriz 0 0) (position matriz 1 1) (position matriz 1 2) (position matriz 1 3) (position matriz 0 4) (position matriz 1 5) (position matriz 1 6) (position matriz 1 7) (position matriz 0 8) (position matriz 1 9) (position matriz 1 10) (position matriz 1 11) (position matriz 0 12) (position matriz 1 13) (position matriz 1 14) (position matriz 1 15)) nueva) (+ 1 contador) type)] ;;naranja
               [(equal? contador 2)
                (c1b matriz (cons (list (position matriz 1 0) (position matriz 2 1) (position matriz 2 2) (position matriz 2 3) (position matriz 1 4) (position matriz 2 5) (position matriz 2 6) (position matriz 2 7) (position matriz 1 8) (position matriz 2 9) (position matriz 2 10) (position matriz 2 11) (position matriz 1 12) (position matriz 2 13) (position matriz 2 14) (position matriz 2 15)) nueva) (+ 1 contador) type)] ;;amarillo
               [(equal? contador 3)
                (c1b matriz (cons (list (position matriz 2 0) (position matriz 3 1) (position matriz 3 2) (position matriz 3 3) (position matriz 2 4) (position matriz 3 5) (position matriz 3 6) (position matriz 3 7) (position matriz 2 8) (position matriz 3 9) (position matriz 3 10) (position matriz 3 11) (position matriz 2 12) (position matriz 3 13) (position matriz 3 14) (position matriz 3 15)) nueva) (+ 1 contador) type )] ;;rojo
               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde
               [(equal? contador 5)
                (c1b matriz (cons (list (position matriz 5 12) (position matriz 5 8) (position matriz 5 4) (position matriz 5 0) (position matriz 5 13) (position matriz 5 9) (position matriz 5 5) (position matriz 5 1) (position matriz 5 14) (position matriz 5 10) (position matriz 5 6) (position matriz 5 2) (position matriz 5 15) (position matriz 5 11) (position matriz 5 7) (position matriz 5 3)) nueva) (+ 1 contador) type )] ;;azul
               [else (reverse nueva)])]))


(define (c1a matriz nueva contador type);; se mueve 0, 3, 6
  (cond [(equal? type 3)
         (cond [(equal? contador 0)
                (c1a matriz (cons (list (position matriz 1 0) (position matriz 0 1) (position matriz 0 2) (position matriz 1 3) (position matriz 0 4) (position matriz 0 5) (position matriz 1 6) (position matriz 0 7) (position matriz 0 8)) nueva) (+ 1 contador) type)] ;; blanca
               [(equal? contador 1)
                (c1a matriz (cons (list (position matriz 2 0) (position matriz 1 1) (position matriz 1 2) (position matriz 2 3) (position matriz 1 4) (position matriz 1 5) (position matriz 2 6) (position matriz 1 7) (position matriz 1 8)) nueva) (+ 1 contador) type)];; naranja
               [(equal? contador 2)
                (c1a matriz (cons (list (position matriz 3 0) (position matriz 2 1) (position matriz 2 2) (position matriz 3 3) (position matriz 2 4) (position matriz 2 5) (position matriz 3 6) (position matriz 2 7) (position matriz 2 8)) nueva) (+ 1 contador) type)];; amarillo
               [(equal? contador 3)
                (c1a matriz (cons (list (position matriz 0 0) (position matriz 3 1) (position matriz 3 2) (position matriz 0 3) (position matriz 3 4) (position matriz 3 5) (position matriz 0 6) (position matriz 3 7) (position matriz 3 8)) nueva) (+ 1 contador) type)];; rojo
               [(equal? contador 4)
                (c1a matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)];; verde
               [(equal? contador 5)
                (c1a matriz (cons (list (position matriz 5 2) (position matriz 5 5) (position matriz 5 8) (position matriz 5 1) (position matriz 5 4) (position matriz 5 7) (position matriz 5 0) (position matriz 5 3) (position matriz 5 6)) nueva) (+ 1 contador) type)];;azul
               [else (reverse nueva)])]
        
        [(equal? type 2)
         (cond [(equal? contador 0)
                (c1a matriz (cons (list (position matriz 1 0) (position matriz 0 1) (position matriz 1 2) (position matriz 0 3)) nueva) (+ 1 contador) type)] ;;blanco
               [(equal? contador 1)
                (c1a matriz (cons (list (position matriz 2 0) (position matriz 1 1) (position matriz 2 2) (position matriz 1 3)) nueva) (+ 1 contador) type)] ;;naranja
               [(equal? contador 2)
                (c1a matriz (cons (list (position matriz 3 0) (position matriz 2 1) (position matriz 3 2) (position matriz 2 3)) nueva) (+ 1 contador) type)] ;amarillo
               [(equal? contador 3)
                (c1a matriz (cons (list (position matriz 0 0) (position matriz 3 1) (position matriz 0 2) (position matriz 3 3)) nueva) (+ 1 contador) type)] ;;roja
               [(equal? contador 4)
                (c1a matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde
               [(equal? contador 5)
                (c1a matriz (cons (list (position matriz 5 1) (position matriz 5 3) (position matriz 5 0) (position matriz 5 2)) nueva) (+ 1 contador) type)] ;;azul
               [else (reverse nueva)])]

        [(equal? type 4)
         (cond [(equal? contador 0)
                (c1a matriz (cons (list (position matriz 1 0) (position matriz 0 1) (position matriz 0 2) (position matriz 0 3) (position matriz 1 4) (position matriz 0 5) (position matriz 0 6) (position matriz 0 7) (position matriz 1 8) (position matriz 0 9) (position matriz 0 10) (position matriz 0 11) (position matriz 1 12) (position matriz 0 13) (position matriz 0 14) (position matriz 0 15)) nueva) (+ 1 contador) type)] ;;blanco
               [(equal? contador 1)
                (c1a matriz (cons (list (position matriz 2 0) (position matriz 1 1) (position matriz 1 2) (position matriz 1 3) (position matriz 2 4) (position matriz 1 5) (position matriz 1 6) (position matriz 1 7) (position matriz 2 8) (position matriz 1 9) (position matriz 1 10) (position matriz 1 11) (position matriz 2 12) (position matriz 1 13) (position matriz 1 14) (position matriz 1 15)) nueva) (+ 1 contador) type)] ;;naranja
               [(equal? contador 2)
                (c1a matriz (cons (list (position matriz 3 0) (position matriz 2 1) (position matriz 2 2) (position matriz 2 3) (position matriz 3 4) (position matriz 2 5) (position matriz 2 6) (position matriz 2 7) (position matriz 3 8) (position matriz 2 9) (position matriz 2 10) (position matriz 2 11) (position matriz 3 12) (position matriz 2 13) (position matriz 2 14) (position matriz 2 15)) nueva) (+ 1 contador) type)] ;;amarillo
               [(equal? contador 3)
                (c1a matriz (cons (list (position matriz 0 0) (position matriz 3 1) (position matriz 3 2) (position matriz 3 3) (position matriz 0 4) (position matriz 3 5) (position matriz 3 6) (position matriz 3 7) (position matriz 0 8) (position matriz 3 9) (position matriz 3 10) (position matriz 3 11) (position matriz 0 12) (position matriz 3 13) (position matriz 3 14) (position matriz 3 15)) nueva) (+ 1 contador) type )] ;;rojo
               [(equal? contador 4)
                (c1a matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde
               [(equal? contador 5)
                (c1a matriz (cons (list (position matriz 5 3) (position matriz 5 7) (position matriz 5 11) (position matriz 5 15) (position matriz 5 2) (position matriz 5 6) (position matriz 5 10) (position matriz 5 14) (position matriz 5 1) (position matriz 5 5) (position matriz 5 9) (position matriz 5 13) (position matriz 5 0) (position matriz 5 4) (position matriz 5 8) (position matriz 5 12)) nueva) (+ 1 contador) type )] ;;azul
               [else (reverse nueva)])]))

 
  


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