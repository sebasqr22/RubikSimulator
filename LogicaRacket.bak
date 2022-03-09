#lang racket

(require "c2.rkt" "c1.rkt" "c3.rkt" "c4.rkt" "c5.rkt" "c6.rkt" "f1.rkt" "f2.rkt" "f3.rkt" "f4.rkt" "f5.rkt" "f6.rkt")

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
;; 5x5 '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3) (4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4) (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5))
;; 6x6 '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) (3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3) (4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4) (5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5))

(define (separador listas aux contador)
  (cond [(null? listas) aux]
        [(equal? contador 5)
         (separador (cdr listas) (cons (list (car listas)) aux ) 0)]
        [else (separador (cdr listas) (cons (car listas) aux) (+ 1 contador))]))

(define (separador2 listas aux largo)
  (cond [(null? listas) (reverse aux)]
        [else

         (separador2 (cddddr(cddr listas)) (cons (list (list-ref listas 0)
                                                            (list-ref listas 1)
                                                            (list-ref listas 2)
                                                            (list-ref listas 3)
                                                            (list-ref listas 4)
                                                            (list-ref listas 5)) aux) largo)]))
        

(define (validarCubo columnas)
  (cond [(equal? columnas 4) #t]
        [(equal? columnas 9) #t]
        [(equal? columnas 16) #t]
        [(equal? columnas 25) #t]
        [(equal? columnas 36) #t]
        [else #f]))


(define (movimientosAux estadoIni listaMovs filas columnas)
  (cond [(equal? (validarCubo filas columnas)
                 #t)]
        [else #f]))
  
         
(define (movimientos estadoIni listaMovs)
  (cond [(equal? (columnas estadoIni 0 0 0) #f) #f]
        [else (movimientosAux estadoIni (filas estadoIni 0) (columnas estadoIni 0 0 0))]))

(define (validaMov mov)
  (cond [(equal? mov #f) #f]
        [else #t]))

(define (movs lista cubo largo final)
  (cond [(null? lista) (separador2 final '() largo)]

        [(equal? (car lista) "c1a")
         (cond [(equal? (validaMov (c1a cubo '() 0 largo)) #t)
                
                (movs (cdr lista) (c1a cubo '() 0 largo) largo (append final (c1a cubo '() 0 largo)) )]
               
               [else (movs (cdr lista) cubo largo final)])]
        
        [(equal? (car lista) "c1b")
         (cond [(equal? (validaMov (c1b cubo '() 0 largo)) #t)
                (movs (cdr lista) (c1b cubo '() 0 largo) largo (append final (c1b cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "c2a")
         (cond [(equal? (validaMov (c2a cubo '() 0 largo)) #t)
                (movs (cdr lista) (c2a cubo '() 0 largo) largo (append final (c2a cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]
        
        [(equal? (car lista) "c2b")
         (cond [(equal? (validaMov (c2b cubo '() 0 largo)) #t)
                (movs (cdr lista) (c2b cubo '() 0 largo) largo (append final (c2b cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        
        [(equal? (car lista) "c3a")
         (cond [(equal? (validaMov (c3a cubo '() 0 largo)) #t)
                (movs (cdr lista) (c3a cubo '() 0 largo) largo (append final (c3a cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]
        
        [(equal? (car lista) "c3b")
         (cond [(equal? (validaMov (c3b cubo '() 0 largo)) #t)
                (movs (cdr lista) (c3b cubo '() 0 largo) largo (append final (c3b cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "c4a")
         (cond [(equal? (validaMov (c4a cubo '() 0 largo)) #t)
                (movs (cdr lista) (c4a cubo '() 0 largo) largo (append final (c4a cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "c4b")
         (cond [(equal? (validaMov (c4b cubo '() 0 largo)) #t)
               (movs (cdr lista) (c4b cubo '() 0 largo) largo (append final (c4b cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]
        
        [(equal? (car lista) "c5a")
         (cond [(equal? (validaMov (c5a cubo '() 0 largo)) #t)
                (movs (cdr lista) (c5a cubo '() 0 largo) largo (append final (c5a cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "c5b")
         (cond [(equal? (validaMov (c5b cubo '() 0 largo)) #t)
                (movs (cdr lista) (c5b cubo '() 0 largo) largo (append final (c5b cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "c6a")
         (cond [(equal? (validaMov (c6a cubo '() 0 largo)) #t)
                (movs (cdr lista) (c6a cubo '() 0 largo) largo (append final (c6a cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        
        [(equal? (car lista) "c6b")
         (cond [(equal? (validaMov (c6b cubo '() 0 largo)) #t)
                (movs (cdr lista) (c6b cubo '() 0 largo) largo (append final (c6b cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f1d")
         (cond [(equal? (validaMov (f1d cubo '() 0 largo)) #t)
                (movs (cdr lista) (f1d cubo '() 0 largo) largo (append final (f1d cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f1i")
         (cond [(equal? (validaMov (f1i cubo '() 0 largo)) #t)
                (movs (cdr lista) (f1i cubo '() 0 largo) largo (append final (f1i cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f2d")
         (cond [(equal? (validaMov (f2d cubo '() 0 largo)) #t)
                (movs (cdr lista) (f2d cubo '() 0 largo) largo (append final (f2d cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f2i")
         (cond [(equal? (validaMov (f2i cubo '() 0 largo)) #t)
                (movs (cdr lista) (f1i cubo '() 0 largo) largo (append final (f1i cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f3d")
         (cond [(equal? (validaMov (f3d cubo '() 0 largo)) #t)
                (movs (cdr lista) (f3d cubo '() 0 largo) largo (append final (f3d cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f3i")
         (cond [(equal? (validaMov (f3i cubo '() 0 largo)) #t)
                (movs (cdr lista) (f3i cubo '() 0 largo) largo (append final (f3i cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f4d")
         (cond [(equal? (validaMov (f4d cubo '() 0 largo)) #t)
                (movs (cdr lista) (f4d cubo '() 0 largo) largo (append final (f4d cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f4i")
         (cond [(equal? (validaMov (f4i cubo '() 0 largo)) #t)
                (movs (cdr lista) (f4i cubo '() 0 largo) largo (append final (f4i cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f5d")
         (cond [(equal? (validaMov (f5d cubo '() 0 largo)) #t)
                (movs (cdr lista) (f5d cubo '() 0 largo) largo (append final (f5d cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f5i")
         (cond [(equal? (validaMov (f5i cubo '() 0 largo)) #t)
                (movs (cdr lista) (f5i cubo '() 0 largo) largo (list final (f5i cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f6d")
         (cond [(equal? (validaMov (f6d cubo '() 0 largo)) #t)
                (movs (cdr lista) (f6d cubo '() 0 largo) largo (list final (f6d cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [(equal? (car lista) "f6i")
         (cond [(equal? (validaMov (f6i cubo '() 0 largo)) #t)
                (movs (cdr lista) (f6i cubo '() 0 largo) largo (list final (f6i cubo '() 0 largo)))]
               [else (movs (cdr lista) cubo largo final)])]

        [else (movs (cdr lista) cubo largo final)]))

        
(define (getLargo columnas)
  (cond [(equal? columnas 4) 2]
        [(equal? columnas 9) 3]
        [(equal? columnas 16) 4]
        [(equal? columnas 25) 5]
        [(equal? columnas 36) 6]))




(define (RS cubo mov)
  (cond [(list? mov)
         (cond [(equal? (validarCubo (columnas cubo 0 0 0)) #t)
                (movs mov cubo (getLargo (columnas cubo 0 0 0)) cubo )]
               
               [else #t])]
        [else #f]))

       
