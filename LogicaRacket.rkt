#lang racket

(define (filas matriz contador)
  (cond [(null? matriz) contador]
        [else (filas (cdr matriz) (+ contador 1))]))

(define (cantElementosLista lista contador)
  (cond [(null? lista) contador]
        [else (cantElementosLista (cdr lista) (+ contador 1))]))

(define (columnasAux matriz contador cantFilas)
  (cond [(null? matriz) #f]
        [(equal? (cantElementosLista (car matriz))
                 (cond [(equal? 0 cantFilas) 

(define (columnas matriz lista contador cantFilas)
  (cond [(null? lista) (columnasAux matriz contador)]
        [else (columnas matriz (cdr lista) (+ contador 1))]))