#lang racket/gui
(require 2htdp/image) ; Se importa esta libreria para proyectar imagenes
(require lang/posn) ; Se importa esta libreria para usar posiciones en x/y con poligonos
(provide Puente)

(define cuboInicial '())
(define movimientos '())

;(caadr(RS '((0 0 0 0 0 0 0 0 0) (1 1 1 1 1 1 1 1 1) (2 2 2 2 2 2 2 2 2) (3 3 3 3 3 3 3 3 3) (4 4 4 4 4 4 4 4 4) (5 5 5 5 5 5 5 5 5)) '("f1d")))
(define (Puente listaMovs)
  (set! cuboInicial (car listaMovs))
  (display cuboInicial)
  (cond [(null? (cdr listaMovs)) (set! movimientos '() )]
        [else (set! movimientos (cdr listaMovs))])
  (display "\n")
  (display movimientos))

; Funcion para definir el color de cualquier cuadrado
(define (color num)
  (cond
  ((eq? num 0)(string-append "white"))
  ((eq? num 1)(string-append "darkorange"))
  ((eq? num 2)(string-append "gold"))
  ((eq? num 3)(string-append "firebrick"))
  ((eq? num 4)(string-append "darkgreen"))
  ((eq? num 5)(string-append "cornflowerblue"))
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
           "outline" "black")))

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
           "outline" "black")))

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
           "outline" "black")))
; Funcion que permite representar cualquier fila de cuadrados, de cualquier tamano

(define (overlays num face A B C D E F)
  (cond
  ((eq? num 3)
   (if (equal? face "left")(overlay/xy (overlay/xy A 50 20 B) 100 40 C)
       (if (equal? face "right")(overlay/xy (overlay/xy A 50 -20 B) 100 -20 C)
           (overlay/xy (overlay/xy A 50 -20 B) 100 -20 C))))
  
  ((eq? num 4)
   (if (equal? face "left")(overlay/xy (overlay/xy (overlay/xy A 50 20 B) 100 40 C) 150 60 D)
       (if (equal? face "right")(overlay/xy (overlay/xy (overlay/xy A 50 -20 B) 100 -20 C) 150 -20 D)
           (overlay/xy (overlay/xy (overlay/xy A 50 -20 B) 100 -20 C) 150 -20 D))))
  
  ((eq? num 5)
   (if (equal? face "left")(overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 20 B) 100 40 C) 150 60 D) 200 80 E)
       (if (equal? face "right")(overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 -20 B) 100 -20 C) 150 -20 D) 200 -20 E)
           (overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 -20 B) 100 -20 C) 150 -20 D) 200 -20 E))))

  ((eq? num 6)
   (if (equal? face "left")(overlay/xy (overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 20 B) 100 40 C) 150 60 D) 200 80 E) 250 100 F)
       (if (equal? face "right")(overlay/xy (overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 -20 B) 100 -20 C) 150 -20 D) 200 -20 E) 250 -20 F)
           (overlay/xy (overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 -20 B) 100 -20 C) 150 -20 D) 200 -20 E) 250 -20 F))))
 
  (else (cube face A B C D E F))))

; Funcion que muestra el cubo, dependiendo de cuantas filas y columnas sea el mismo
(define (cube val A B C D E F)
  (cond
    ((eq? val "threeL")(overlay/xy (overlay/xy A 0 50 B) 0 100 C))
    ((eq? val "fourL")(overlay/xy (overlay/xy (overlay/xy A 0 50 B) 0 100 C) 0 150 D))
    ((eq? val "fiveL")(overlay/xy (overlay/xy (overlay/xy (overlay/xy A 0 50 B)0 100 C) 0 150 D) 0 200 E))
    ((eq? val "sixL")(overlay/xy (overlay/xy (overlay/xy (overlay/xy (overlay/xy A 0 50 B)0 100 C) 0 150 D) 0 200 E) 0 250 F))
    ((eq? val "threeU")(overlay/xy (overlay/xy A 50 20 B) 100 40 C))
    ((eq? val "fourU")(overlay/xy (overlay/xy (overlay/xy A 50 20 B) 100 40 C) 150 60 D))
    ((eq? val "fiveU")(overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 20 B) 100 40 C) 150 60 D) 200 80 E))
    ((eq? val "sixU")(overlay/xy (overlay/xy (overlay/xy (overlay/xy (overlay/xy A 50 20 B) 100 40 C) 150 60 D) 200 80 E) 250 100 F))))


;Funciones para crear cada cuadrado independiente

; Cara izquierda
(define r1c1 (left 5))
(define r1c2 (left 5))
(define r1c3 (left 5))
(define r1c4 (left 5))
(define r1c5 (left 5))
(define r1c6 (left 5))

(define r2c1 (left 5))
(define r2c2 (left 5))
(define r2c3 (left 5))
(define r2c4 (left 5))
(define r2c5 (left 5))
(define r2c6 (left 5))

(define r3c1 (left 5))
(define r3c2 (left 5))
(define r3c3 (left 5))
(define r3c4 (left 5))
(define r3c5 (left 5))
(define r3c6 (left 5))

(define r4c1 (left 5))
(define r4c2 (left 5))
(define r4c3 (left 5))
(define r4c4 (left 5))
(define r4c5 (left 5))
(define r4c6 (left 5))

(define r5c1 (left 5))
(define r5c2 (left 5))
(define r5c3 (left 5))
(define r5c4 (left 5))
(define r5c5 (left 5))
(define r5c6 (left 5))

(define r6c1 (left 5))
(define r6c2 (left 5))
(define r6c3 (left 5))
(define r6c4 (left 5))
(define r6c5 (left 5))
(define r6c6 (left 5))

; Funciones para crear las caras izquierdas de diferentes tamanos

; Cubo 2x2
(define Aleft2x2(overlay/xy r1c1 50 20 r1c2))
(define Bleft2x2(overlay/xy r2c1 50 20 r2c2))

; Cubo 3x3
(define Aleft3x3(overlays 3 "left" r1c1 r1c2 r1c3 0 0 0))
(define Bleft3x3(overlays 3 "left" r2c1 r2c2 r2c3 0 0 0))
(define Cleft3x3(overlays 3 "left" r3c1 r3c2 r3c3 0 0 0))

; Cubo 4x4
(define Aleft4x4(overlays 4 "left" r1c1 r1c2 r1c3 r1c4 0 0))
(define Bleft4x4(overlays 4 "left" r2c1 r2c2 r2c3 r2c4 0 0))
(define Cleft4x4(overlays 4 "left" r3c1 r3c2 r3c3 r3c4 0 0))
(define Dleft4x4(overlays 4 "left" r4c1 r4c2 r4c3 r4c4 0 0))
  

; Cubo 5x5
(define Aleft5x5(overlays 5 "left" r1c1 r1c2 r1c3 r1c4 r1c5 0))
(define Bleft5x5(overlays 5 "left" r2c1 r2c2 r2c3 r2c4 r2c5 0))
(define Cleft5x5(overlays 5 "left" r3c1 r3c2 r3c3 r3c4 r3c5 0))
(define Dleft5x5(overlays 5 "left" r4c1 r4c2 r4c3 r4c4 r4c5 0))
(define Eleft5x5(overlays 5 "left" r5c1 r5c2 r5c3 r5c4 r5c5 0))

; Cubo 6x6
(define Aleft6x6(overlays 6 "left" r1c1 r1c2 r1c3 r1c4 r1c5 r1c6))
(define Bleft6x6(overlays 6 "left" r2c1 r2c2 r2c3 r2c4 r2c5 r2c6))
(define Cleft6x6(overlays 6 "left" r3c1 r3c2 r3c3 r3c4 r3c5 r3c6))
(define Dleft6x6(overlays 6 "left" r4c1 r4c2 r4c3 r4c4 r4c5 r4c6))
(define Eleft6x6(overlays 6 "left" r5c1 r5c2 r5c3 r5c4 r5c5 r5c6))
(define Fleft6x6(overlays 6 "left" r5c1 r5c2 r5c3 r5c4 r5c5 r6c6))


;Cara derecha
(define r7c1 (right 0))
(define r7c2 (right 0))
(define r7c3 (right 0))
(define r7c4 (right 0))
(define r7c5 (right 0))
(define r7c6 (right 0))

(define r8c1 (right 0))
(define r8c2 (right 0))
(define r8c3 (right 0))
(define r8c4 (right 0))
(define r8c5 (right 0))
(define r8c6 (right 0))

(define r9c1 (right 0))
(define r9c2 (right 0))
(define r9c3 (right 0))
(define r9c4 (right 0))
(define r9c5 (right 0))
(define r9c6 (right 0))

(define r10c1 (right 0))
(define r10c2 (right 0))
(define r10c3 (right 0))
(define r10c4 (right 0))
(define r10c5 (right 0))
(define r10c6 (right 0))

(define r11c1 (right 0))
(define r11c2 (right 0))
(define r11c3 (right 0))
(define r11c4 (right 0))
(define r11c5 (right 0))
(define r11c6 (right 0))

(define r12c1 (right 0))
(define r12c2 (right 0))
(define r12c3 (right 0))
(define r12c4 (right 0))
(define r12c5 (right 0))
(define r12c6 (right 0))

; Funciones para crear las caras derechas de diferentes tamanos

; Cubo 2x2
(define Aright2x2(overlay/xy r7c1 50 -20 r7c2))
(define Bright2x2(overlay/xy r8c1 50 -20 r8c2))

; Cubo 3x3
(define Aright3x3(overlays 3 "right" r7c1 r7c2 r7c3 0 0 0))
(define Bright3x3(overlays 3 "right" r8c1 r8c2 r8c3 0 0 0))
(define Cright3x3(overlays 3 "right" r9c1 r9c2 r9c3 0 0 0))

; Cubo 4x4
(define Aright4x4(overlays 4 "right" r7c1 r7c2 r7c3 r7c4 0 0))
(define Bright4x4(overlays 4 "right" r8c1 r8c2 r8c3 r8c4 0 0))
(define Cright4x4(overlays 4 "right" r9c1 r9c2 r9c3 r9c4 0 0))
(define Dright4x4(overlays 4 "right" r10c1 r10c2 r10c3 r10c4 0 0))
  

; Cubo 5x5
(define Aright5x5(overlays 5 "right" r7c1 r7c2 r7c3 r7c4 r7c5 0))
(define Bright5x5(overlays 5 "right" r8c1 r8c2 r8c3 r8c4 r8c5 0))
(define Cright5x5(overlays 5 "right" r9c1 r9c2 r9c3 r9c4 r9c5 0))
(define Dright5x5(overlays 5 "right" r10c1 r10c2 r10c3 r10c4 r10c5 0))
(define Eright5x5(overlays 5 "right" r11c1 r11c2 r11c3 r11c4 r11c5 0))


; Cubo 6x6
(define Aright6x6(overlays 6 "right" r7c1 r7c2 r7c3 r7c4 r7c5 r7c6))
(define Bright6x6(overlays 6 "right" r8c1 r8c2 r8c3 r8c4 r8c5 r8c6))
(define Cright6x6(overlays 6 "right" r9c1 r9c2 r9c3 r9c4 r9c5 r9c6))
(define Dright6x6(overlays 6 "right" r10c1 r10c2 r10c3 r10c4 r10c5 r10c6))
(define Eright6x6(overlays 6 "right" r11c1 r11c2 r11c3 r11c4 r11c5 r11c6))
(define Fright6x6(overlays 6 "right" r12c1 r12c2 r12c3 r12c4 r12c5 r12c6))


;Cara arriba
(define r13c1 (up 3))
(define r13c2 (up 3))
(define r13c3 (up 3))
(define r13c4 (up 3))
(define r13c5 (up 3))
(define r13c6 (up 3))

(define r14c1 (up 3))
(define r14c2 (up 3))
(define r14c3 (up 3))
(define r14c4 (up 3))
(define r14c5 (up 3))
(define r14c6 (up 3))

(define r15c1 (up 3))
(define r15c2 (up 3))
(define r15c3 (up 3))
(define r15c4 (up 3))
(define r15c5 (up 3))
(define r15c6 (up 3))

(define r16c1 (up 3))
(define r16c2 (up 3))
(define r16c3 (up 3))
(define r16c4 (up 3))
(define r16c5 (up 3))
(define r16c6 (up 3))

(define r17c1 (up 3))
(define r17c2 (up 3))
(define r17c3 (up 3))
(define r17c4 (up 3))
(define r17c5 (up 3))
(define r17c6 (up 3))

(define r18c1 (up 3))
(define r18c2 (up 3))
(define r18c3 (up 3))
(define r18c4 (up 3))
(define r18c5 (up 3))
(define r18c6 (up 3))

; Funciones para crear las caras de arriba de diferentes tamanos

; Cubo 2x2
(define Aup2x2(overlay/xy r13c1 50 -20 r13c2))
(define Bup2x2(overlay/xy r14c1 50 -20 r14c2))

; Cubo 3x3
(define Aup3x3(overlays 3 "up" r13c1 r13c2 r13c3 0 0 0))
(define Bup3x3(overlays 3 "up" r14c1 r14c2 r14c3 0 0 0))
(define Cup3x3(overlays 3 "up" r15c1 r15c2 r15c3 0 0 0))

; Cubo 4x4
(define Aup4x4(overlays 4 "up" r13c1 r13c2 r13c3 r13c4 0 0))
(define Bup4x4(overlays 4 "up" r14c1 r14c2 r14c3 r14c4 0 0))
(define Cup4x4(overlays 4 "up" r15c1 r15c2 r15c3 r15c4 0 0))
(define Dup4x4(overlays 4 "up" r15c1 r15c2 r15c3 r15c4 0 0))
  

; Cubo 5x5
(define Aup5x5(overlays 5 "up" r13c1 r13c2 r13c3 r13c4 r13c5 0))
(define Bup5x5(overlays 5 "up" r14c1 r14c2 r14c3 r14c4 r14c5 0))
(define Cup5x5(overlays 5 "up" r15c1 r15c2 r15c3 r15c4 r15c5 0))
(define Dup5x5(overlays 5 "up" r16c1 r16c2 r16c3 r16c4 r16c5 0))
(define Eup5x5(overlays 5 "up" r17c1 r17c2 r17c3 r17c4 r17c5 0))


; Cubo 6x6
(define Aup6x6(overlays 6 "up" r13c1 r13c2 r13c3 r13c4 r13c5 r13c6))
(define Bup6x6(overlays 6 "up" r14c1 r14c2 r14c3 r14c4 r14c5 r14c6))
(define Cup6x6(overlays 6 "up" r15c1 r15c2 r15c3 r15c4 r15c5 r15c6))
(define Dup6x6(overlays 6 "up" r16c1 r16c2 r16c3 r16c4 r16c5 r16c6))
(define Eup6x6(overlays 6 "up" r17c1 r17c2 r17c3 r17c4 r17c5 r17c6))
(define Fup6x6(overlays 6 "up" r18c1 r18c2 r18c3 r18c4 r18c5 r18c6))



; Funciones para juntar todas las filas en una sola cara

; Cubo 3x3
(define leftFace3 (overlays 0 "threeL" Aleft3x3 Bleft3x3 Cleft3x3 Dleft4x4 Eleft5x5 Fleft6x6))
(define rightFace3 (overlays 0 "threeL" Aright3x3 Bright3x3 Cright3x3 Dleft4x4 Eleft5x5 Fleft6x6))
(define upFace3 (overlays 0 "threeU" Aup3x3 Bup3x3 Cup3x3 Dleft4x4 Eleft5x5 Fleft6x6))

;Cubo 4x4
(define leftFace4 (overlays 0 "fourL" Aleft4x4 Bleft4x4 Cleft4x4 Dleft4x4 Eleft5x5 Fleft6x6))
(define rightFace4 (overlays 0 "fourL" Aright4x4 Bright4x4 Cright4x4 Dright4x4 Eleft5x5 Fleft6x6))
(define upFace4 (overlays 0 "fourU" Aup4x4 Bup4x4 Cup4x4 Dup4x4 Eleft5x5 Fleft6x6))

;Cubo 5x5
(define leftFace5 (overlays 0 "fiveL" Aleft5x5 Bleft5x5 Cleft5x5 Dleft5x5 Eleft5x5 Fleft6x6))
(define rightFace5 (overlays 0 "fiveL" Aright5x5 Bright5x5 Cright5x5 Dright5x5 Eright5x5 Fleft6x6))
(define upFace5 (overlays 0 "fiveU" Aup5x5 Bup5x5 Cup5x5 Dup5x5 Eup5x5 Fleft6x6))

;Cubo 6x6
(define leftFace6 (overlays 0 "sixL" Aleft6x6 Bleft6x6 Cleft6x6 Dleft6x6 Eleft6x6 Fleft6x6))
(define rightFace6 (overlays 0 "sixL" Aright6x6 Bright6x6 Cright6x6 Dright6x6 Eright6x6 Fright6x6))
(define upFace6 (overlays 0 "sixU" Aup6x6 Bup6x6 Cup6x6 Dup6x6 Eup6x6 Fup6x6))


; Funcion para proyectar cada cara con sus colocaciones correctas, de modo que se proyecte el cubo correctamente
(define (cubo val)
  (cond
    ((if(eq? val 2)(overlay/xy (overlay/xy (overlay/xy (overlay/xy (overlay/xy Aleft2x2 0 50 Bleft2x2) 100 0 Aright2x2) 100 50 Bright2x2) 0 -40 Aup2x2) 50 20 Bup2x2)
        (if (eq? val 3)(overlay/xy (overlay/xy leftFace3 150 0 rightFace3) 0 -60 upFace3)
            (if (eq? val 4)(overlay/xy (overlay/xy leftFace4 200 0 rightFace4) 0 -80 upFace4)
                (if (eq? val 5)(overlay/xy (overlay/xy leftFace5 250 0 rightFace5) 0 -100 upFace5)
                    (overlay/xy (overlay/xy leftFace6 300 0 rightFace6) 0 -120 upFace6))))))))


;(set! r1c1 (left num))
;(set! Aleft3x3(overlays 3 "left" r1c1 r1c2 r1c3 0 0 0))
