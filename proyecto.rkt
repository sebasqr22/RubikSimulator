#lang racket/gui
(require 2htdp/image) ; Se importa esta libreria para proyectar imagenes
(require lang/posn) ; Se importa esta libreria para usar posiciones en x/y con poligonos
;(require 2htdp/universe)



; Funcion para definir el color de cualquier cuadrado
(define (color num)
  (cond
  ((eq? num 1)(string-append "red"))
  ((eq? num 2)(string-append "blue"))
  ((eq? num 3)(string-append "darkgreen"))
  ))

; Funciones para colocar cada cuadrado

; Cara izquierda
(define (left num)
  (underlay(polygon (list (make-posn 0 0)
                (make-posn 50 20)
                (make-posn 50 70)
                (make-posn 0 50))
           "solid" (color num))
      (polygon (list (make-posn 0 0)
                (make-posn 50 20)
                (make-posn 50 70)
                (make-posn 0 50))
           "outline" "white")))

;Cara derecha
(define (right num)
  (underlay(polygon (list (make-posn 0 0)
                (make-posn 50 -20)
                (make-posn 50 30)
                (make-posn 0 50))
           "solid" (color num))
      (polygon (list (make-posn 0 0)
                (make-posn 50 -20)
                (make-posn 50 30)
                (make-posn 0 50))
           "outline" "white")))

;Cara arriba
(define (up num)
  (underlay(polygon (list (make-posn 0 0)
                (make-posn 50 -20)
                (make-posn 0 -40)
                (make-posn -50 -20))
           "solid" (color num))
      (polygon (list (make-posn 0 0)
                (make-posn 50 -20)
                (make-posn 0 -40)
                (make-posn -50 -20))
           "outline" "white")))

;Funciones para crear cada cuadrado independiente

; Cara izquierda
(define r1c1 (left 1))
(define r1c2 (left 1))
(define r1c3 (left 1))

(define r2c1 (left 1))
(define r2c2 (left 1))
(define r2c3 (left 1))

(define r3c1 (left 1))
(define r3c2 (left 1))
(define r3c3 (left 1))

; Funcion para juntar 3 cuadrados en 1 sola fila (izquierda a derecha)
(define r1(overlay/xy (overlay/xy r1c1 50 20 r1c2) 100 40 r1c3))
(define r2(overlay/xy (overlay/xy r2c1 50 20 r2c2) 100 40 r2c3))
(define r3(overlay/xy (overlay/xy r3c1 50 20 r3c2) 100 40 r3c3))

;Cara derecha
(define r4c1 (right 2))
(define r4c2 (right 2))
(define r4c3 (right 2))

(define r5c1 (right 2))
(define r5c2 (right 2))
(define r5c3 (right 2))

(define r6c1 (right 2))
(define r6c2 (right 2))
(define r6c3 (right 2))

; Funcion para juntar 3 cuadrados en 1 sola fila (izquierda a derecha)
(define r4(overlay/xy (overlay/xy r4c1 50 -20 r4c2) 100 -20 r4c3))
(define r5(overlay/xy (overlay/xy r5c1 50 -20 r5c2) 100 -20 r5c3))
(define r6(overlay/xy (overlay/xy r6c1 50 -20 r6c2) 100 -20 r6c3))

;Cara arriba
(define r7c1 (up 3))
(define r7c2 (up 3))
(define r7c3 (up 3))


(define r8c1 (up 3))
(define r8c2 (up 3))
(define r8c3 (up 3))

(define r9c1 (up 3))
(define r9c2 (up 3))
(define r9c3 (up 3))

; Funcion para juntar 3 cuadrados en 1 sola fila (izquierda a derecha)
(define r7(overlay/xy (overlay/xy r7c1 50 -20 r7c2) 100 -20 r7c3))
(define r8(overlay/xy (overlay/xy r8c1 50 -20 r8c2) 100 -20 r8c3))
(define r9(overlay/xy (overlay/xy r9c1 50 -20 r9c2) 100 -20 r9c3))

; Funciones para juntar todas las filas en una sola cara
(define leftFace (overlay/xy (overlay/xy r1 0 50 r2) 0 100 r3)) 
(define rightFace (overlay/xy (overlay/xy r4 0 50 r5) 0 100 r6))
(define upperFace (overlay/xy(overlay/xy r7 50 20 r8) 100 40 r9))


; Funcion para proyectar cada cara con sus colocaciones correctas, de modo que se proyecte el cubo correctamente
(overlay/xy (overlay/xy leftFace 150 0 rightFace) 0 -60 upperFace)
