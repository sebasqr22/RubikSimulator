#lang racket

(define (position matriz i j)
  (list-ref (list-ref matriz i) j))


(define (c1b matriz nueva contador type);; se mueve 0, 3, 6
  (cond [(equal? type 3)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list (position matriz 3 0) (position matriz 0 1) (position matriz 0 2)
                                        (position matriz 3 3) (position matriz 0 4) (position matriz 0 5)
                                        (position matriz 3 6) (position matriz 0 7) (position matriz 0 8))
                                  nueva) (+ 1 contador) type)] ;; blanca
               [(equal? contador 1)
                (c1b matriz (cons (list (position matriz 0 0) (position matriz 1 1) (position matriz 1 2)
                                        (position matriz 0 3) (position matriz 1 4) (position matriz 1 5)
                                        (position matriz 0 6) (position matriz 1 7) (position matriz 1 8))
                                  nueva) (+ 1 contador) type)];; naranja
               [(equal? contador 2)
                (c1b matriz (cons (list (position matriz 1 0) (position matriz 2 1) (position matriz 2 2)
                                        (position matriz 1 3) (position matriz 2 4) (position matriz 2 5)
                                        (position matriz 1 6) (position matriz 2 7) (position matriz 2 8))
                                  nueva) (+ 1 contador) type)];; amarillo
               [(equal? contador 3)
                (c1b matriz (cons (list (position matriz 2 0) (position matriz 3 1) (position matriz 3 2)
                                        (position matriz 2 3) (position matriz 3 4) (position matriz 3 5)
                                        (position matriz 2 6) (position matriz 3 7) (position matriz 3 8))
                                  nueva) (+ 1 contador) type)];; rojo
               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)];; verde
               [(equal? contador 5)
                (c1b matriz (cons (list (position matriz 5 6) (position matriz 5 3) (position matriz 5 0)
                                        (position matriz 5 7) (position matriz 5 4) (position matriz 5 1)
                                        (position matriz 5 8) (position matriz 5 5) (position matriz 5 2))
                                  nueva) (+ 1 contador) type)];;azul
               [else (reverse nueva)])]
        
        [(equal? type 2)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list (position matriz 3 0) (position matriz 0 1)
                                        (position matriz 3 2) (position matriz 0 3))
                                  nueva) (+ 1 contador) type)] ;;blanco
               [(equal? contador 1)
                (c1b matriz (cons (list (position matriz 0 0) (position matriz 1 1)
                                        (position matriz 0 2) (position matriz 1 3))
                                  nueva) (+ 1 contador) type)] ;;naranja
               [(equal? contador 2)
                (c1b matriz (cons (list (position matriz 1 0) (position matriz 2 1)
                                        (position matriz 1 2) (position matriz 2 3))
                                  nueva) (+ 1 contador) type)] ;amarillo
               [(equal? contador 3)
                (c1b matriz (cons (list (position matriz 2 0) (position matriz 3 1)
                                        (position matriz 2 2) (position matriz 3 3))
                                  nueva) (+ 1 contador) type)] ;;roja
               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde
               [(equal? contador 5)
                (c1b matriz (cons (list (position matriz 5 2) (position matriz 5 0)
                                        (position matriz 5 3) (position matriz 5 1))
                                  nueva) (+ 1 contador) type)] ;;azul
               [else (reverse nueva)])]

        [(equal? type 4)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list (position matriz 3 0) (position matriz 0 1) (position matriz 0 2) (position matriz 0 3)
                                        (position matriz 3 4) (position matriz 0 5) (position matriz 0 6) (position matriz 0 7)
                                        (position matriz 3 8) (position matriz 0 9) (position matriz 0 10) (position matriz 0 11)
                                        (position matriz 3 12) (position matriz 0 13) (position matriz 0 14) (position matriz 0 15))
                                  nueva) (+ 1 contador) type)] ;;blanco
               [(equal? contador 1)
                (c1b matriz (cons (list (position matriz 0 0) (position matriz 1 1) (position matriz 1 2) (position matriz 1 3)
                                        (position matriz 0 4) (position matriz 1 5) (position matriz 1 6) (position matriz 1 7)
                                        (position matriz 0 8) (position matriz 1 9) (position matriz 1 10) (position matriz 1 11)
                                        (position matriz 0 12) (position matriz 1 13) (position matriz 1 14) (position matriz 1 15))
                                  nueva) (+ 1 contador) type)] ;;naranja
               [(equal? contador 2)
                (c1b matriz (cons (list (position matriz 1 0) (position matriz 2 1) (position matriz 2 2) (position matriz 2 3)
                                        (position matriz 1 4) (position matriz 2 5) (position matriz 2 6) (position matriz 2 7)
                                        (position matriz 1 8) (position matriz 2 9) (position matriz 2 10) (position matriz 2 11)
                                        (position matriz 1 12) (position matriz 2 13) (position matriz 2 14) (position matriz 2 15))
                                  nueva) (+ 1 contador) type)] ;;amarillo
               [(equal? contador 3)
                (c1b matriz (cons (list (position matriz 2 0) (position matriz 3 1) (position matriz 3 2) (position matriz 3 3)
                                        (position matriz 2 4) (position matriz 3 5) (position matriz 3 6) (position matriz 3 7)
                                        (position matriz 2 8) (position matriz 3 9) (position matriz 3 10) (position matriz 3 11)
                                        (position matriz 2 12) (position matriz 3 13) (position matriz 3 14) (position matriz 3 15))
                                  nueva) (+ 1 contador) type )] ;;rojo
               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde
               [(equal? contador 5)
                (c1b matriz (cons (list (position matriz 5 12) (position matriz 5 8) (position matriz 5 4) (position matriz 5 0)
                                        (position matriz 5 13) (position matriz 5 9) (position matriz 5 5) (position matriz 5 1)
                                        (position matriz 5 14) (position matriz 5 10) (position matriz 5 6) (position matriz 5 2)
                                        (position matriz 5 15) (position matriz 5 11) (position matriz 5 7) (position matriz 5 3))
                                  nueva) (+ 1 contador) type )] ;;azul
               [else (reverse nueva)])]

        [(equal? type 5)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list (position matriz 3 0) (position matriz 0 1) (position matriz 0 2) (position matriz 0 3) (position matriz 0 4) (position matriz 3 5)
                                        (position matriz 0 6) (position matriz 0 7) (position matriz 0 8) (position matriz 0 9) (position matriz 3 10) (position matriz 0 11)
                                        (position matriz 0 12) (position matriz 0 13) (position matriz 0 14) (position matriz 3 15) (position matriz 0 16) (position matriz 0 17)
                                        (position matriz 0 18) (position matriz 0 19) (position matriz 3 20) (position matriz 0 21) (position matriz 0 22) (position matriz 0 23)
                                        (position matriz 0 24)) nueva) (+ 1 contador) type)]
               
               [(equal? contador 1)
               (c1b matriz (cons (list (position matriz 0 0) (position matriz 1 1) (position matriz 1 2) (position matriz 1 3) (position matriz 1 4) (position matriz 0 5)
                                       (position matriz 1 6) (position matriz 1 7) (position matriz 1 8) (position matriz 1 9) (position matriz 0 10) (position matriz 1 11)
                                       (position matriz 1 12) (position matriz 1 13) (position matriz 1 14) (position matriz 0 15) (position matriz 1 16) (position matriz 1 17)
                                       (position matriz 1 18) (position matriz 1 19) (position matriz 0 20) (position matriz 1 21) (position matriz 1 22) (position matriz 1 23)
                                       (position matriz 1 24)) nueva) (+ 1 contador) type)]

               [(equal? contador 2)
                (c1b matriz (cons (list (position matriz 1 0) (position matriz 2 1) (position matriz 2 2) (position matriz 2 3) (position matriz 2 4) (position matriz 1 5)
                                        (position matriz 2 6) (position matriz 2 7) (position matriz 2 8) (position matriz 2 9) (position matriz 1 10) (position matriz 2 11)
                                        (position matriz 2 12) (position matriz 2 13) (position matriz 2 14) (position matriz 1 15) (position matriz 2 16) (position matriz 2 17)
                                        (position matriz 2 18) (position matriz 2 19) (position matriz 1 20) (position matriz 2 21) (position matriz 2 22) (position matriz 2 23)
                                        (position matriz 2 24)) nueva) (+ 1 contador) type)]

               [(equal? contador 3)
                (c1b matriz (cons (list (position matriz 2 0) (position matriz 3 1) (position matriz 3 2) (position matriz 3 3) (position matriz 3 4) (position matriz 2 5)
                                        (position matriz 3 6) (position matriz 3 7) (position matriz 3 8) (position matriz 3 9) (position matriz 2 10) (position matriz 3 11)
                                        (position matriz 3 12) (position matriz 3 13) (position matriz 3 14) (position matriz 2 15) (position matriz 3 16) (position matriz 3 17)
                                        (position matriz 3 18) (position matriz 3 19) (position matriz 2 20) (position matriz 3 21) (position matriz 3 22) (position matriz 3 23)
                                        (position matriz 3 24)) nueva) (+ 1 contador) type)]

               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)];; verde

               [(equal? contador 5)
                (c1b matriz (cons (list (position matriz 5 20) (position matriz 3 15) (position matriz 3 10) (position matriz 3 5) (position matriz 3 0)
                                        (position matriz 3 21) (position matriz 3 16) (position matriz 3 11) (position matriz 3 6) (position matriz 3 1)
                                        (position matriz 3 22) (position matriz 3 17) (position matriz 3 12) (position matriz 3 7) (position matriz 3 2)
                                        (position matriz 3 23) (position matriz 3 18) (position matriz 3 13) (position matriz 3 8) (position matriz 3 3)
                                        (position matriz 3 24) (position matriz 3 19) (position matriz 3 14) (position matriz 3 9) (position matriz 3 4)) nueva) (+ 1 contador) type)]
               [else (reverse nueva)])]
        
        [(equal? type 6)
         (cond [(equal? contador 0)
                (c1b matriz (cons (list
                                   (position matriz 3 0) (position matriz 0 1) (position matriz 0 2) (position matriz 0 3) (position matriz 0 4) (position matriz 0 5)
                                   (position matriz 3 6) (position matriz 0 7) (position matriz 0 8) (position matriz 0 9) (position matriz 0 10) (position matriz 0 11)
                                   (position matriz 3 12) (position matriz 0 13) (position matriz 0 14) (position matriz 0 15) (position matriz 0 16) (position matriz 0 17)
                                   (position matriz 3 18) (position matriz 0 19) (position matriz 0 20) (position matriz 0 21) (position matriz 0 22) (position matriz 0 23)
                                   (position matriz 3 24) (position matriz 0 25) (position matriz 0 26) (position matriz 0 27) (position matriz 0 28) (position matriz 0 29)
                                   (position matriz 3 30) (position matriz 0 31) (position matriz 0 32) (position matriz 0 33) (position matriz 0 34) (position matriz 0 35))
                                  nueva) (+ 1 contador) type)]
               
               [(equal? contador 1)
                (c1b matriz (cons (list
                                   (position matriz 0 0) (position matriz 1 1) (position matriz 1 2) (position matriz 1 3) (position matriz 1 4) (position matriz 1 5)
                                   (position matriz 0 6) (position matriz 1 7) (position matriz 1 8) (position matriz 1 9) (position matriz 1 10) (position matriz 1 11)
                                   (position matriz 0 12) (position matriz 1 13) (position matriz 1 14) (position matriz 1 15) (position matriz 1 16) (position matriz 1 17)
                                   (position matriz 0 18) (position matriz 1 19) (position matriz 1 20) (position matriz 1 21) (position matriz 1 22) (position matriz 1 23)
                                   (position matriz 0 24) (position matriz 1 25) (position matriz 1 26) (position matriz 1 27) (position matriz 1 28) (position matriz 1 29)
                                   (position matriz 0 30) (position matriz 1 31) (position matriz 1 32) (position matriz 1 33) (position matriz 1 34) (position matriz 1 35))
                                  nueva) (+ 1 contador) type)]

               [(equal? contador 2)
                (c1b matriz (cons (list
                                   (position matriz 1 0) (position matriz 2 1) (position matriz 2 2) (position matriz 2 3) (position matriz 2 4) (position matriz 2 5)
                                   (position matriz 1 6) (position matriz 2 7) (position matriz 2 8) (position matriz 2 9) (position matriz 2 10) (position matriz 2 11)
                                   (position matriz 1 12) (position matriz 2 13) (position matriz 2 14) (position matriz 2 15) (position matriz 2 16) (position matriz 2 17)
                                   (position matriz 1 18) (position matriz 2 19) (position matriz 2 20) (position matriz 2 21) (position matriz 2 22) (position matriz 2 23)
                                   (position matriz 1 24) (position matriz 2 25) (position matriz 2 26) (position matriz 2 27) (position matriz 2 28) (position matriz 2 29)
                                   (position matriz 1 30) (position matriz 2 31) (position matriz 2 32) (position matriz 2 33) (position matriz 2 34) (position matriz 2 35))
                                  nueva) (+ 1 contador) type)]
               [(equal? contador 3)
                (c1b matriz (cons (list
                                   (position matriz 2 0) (position matriz 3 1) (position matriz 3 2) (position matriz 3 3) (position matriz 3 4) (position matriz 3 5)
                                   (position matriz 2 6) (position matriz 3 7) (position matriz 3 8) (position matriz 3 9) (position matriz 3 10) (position matriz 3 11)
                                   (position matriz 2 12) (position matriz 3 13) (position matriz 3 14) (position matriz 3 15) (position matriz 3 16) (position matriz 3 17)
                                   (position matriz 2 18) (position matriz 3 19) (position matriz 3 20) (position matriz 3 21) (position matriz 3 22) (position matriz 3 23)
                                   (position matriz 2 24) (position matriz 3 25) (position matriz 3 26) (position matriz 3 27) (position matriz 3 28) (position matriz 3 29)
                                   (position matriz 2 30) (position matriz 3 31) (position matriz 3 32) (position matriz 3 33) (position matriz 3 34) (position matriz 3 35))
                                  nueva) (+ 1 contador) type)]

               [(equal? contador 4)
                (c1b matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde

               [(equal? contador 5)
                (c1b matriz (cons (list
                                   (position matriz 5 30) (position matriz 5 24) (position matriz 5 18) (position matriz 5 12) (position matriz 5 6) (position matriz 5 0)
                                   (position matriz 5 31) (position matriz 5 25) (position matriz 5 19) (position matriz 5 13) (position matriz 5 7) (position matriz 5 1)
                                   (position matriz 5 32) (position matriz 5 26) (position matriz 5 20) (position matriz 5 14) (position matriz 5 8) (position matriz 5 2)
                                   (position matriz 5 33) (position matriz 5 27) (position matriz 5 21) (position matriz 5 15) (position matriz 5 9) (position matriz 5 3)
                                   (position matriz 5 34) (position matriz 5 28) (position matriz 5 22) (position matriz 5 16) (position matriz 5 10) (position matriz 5 4)
                                   (position matriz 5 35) (position matriz 5 29) (position matriz 5 23) (position matriz 5 17) (position matriz 5 11) (position matriz 5 5))
                                  nueva) (+ 1 contador) type)]

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
               [else (reverse nueva)])]

        [(equal? type 5)
         (cond [(equal? contador 0)
                (c1a matriz (cons (list (position matriz 1 0) (position matriz 0 1) (position matriz 0 2) (position matriz 0 3) (position matriz 0 4)
                                        (position matriz 1 5) (position matriz 0 6) (position matriz 0 7) (position matriz 0 8) (position matriz 0 9)
                                        (position matriz 1 10) (position matriz 0 11) (position matriz 0 12) (position matriz 0 13) (position matriz 0 14)
                                        (position matriz 1 15) (position matriz 0 16) (position matriz 0 17) (position matriz 0 18) (position matriz 0 19)
                                        (position matriz 1 20) (position matriz 0 21) (position matriz 0 22) (position matriz 0 23) (position matriz 0 24))
 nueva) (+ 1 contador) type)]
               
               [(equal? contador 1)
               (c1a matriz (cons (list (position matriz 2 0) (position matriz 1 1) (position matriz 1 2) (position matriz 1 3) (position matriz 1 4)
                                       (position matriz 2 5) (position matriz 1 6) (position matriz 1 7) (position matriz 1 8) (position matriz 1 9)
                                       (position matriz 2 10) (position matriz 1 11)(position matriz 1 12) (position matriz 1 13) (position matriz 1 14)
                                       (position matriz 2 15) (position matriz 1 16) (position matriz 1 17)(position matriz 1 18) (position matriz 1 19)
                                       (position matriz 2 20) (position matriz 1 21) (position matriz 1 22) (position matriz 1 23) (position matriz 1 24))
                                 nueva) (+ 1 contador) type)]

               [(equal? contador 2)
                (c1a matriz (cons (list (position matriz 3 0) (position matriz 2 1) (position matriz 2 2) (position matriz 2 3) (position matriz 2 4)
                                        (position matriz 3 5)(position matriz 2 6) (position matriz 2 7) (position matriz 2 8) (position matriz 2 9)
                                        (position matriz 3 10) (position matriz 2 11) (position matriz 2 12) (position matriz 2 13) (position matriz 2 14)
                                        (position matriz 3 15) (position matriz 2 16) (position matriz 2 17)(position matriz 2 18) (position matriz 2 19)
                                        (position matriz 3 20) (position matriz 2 21) (position matriz 2 22) (position matriz 2 23)(position matriz 2 24))
                                  nueva) (+ 1 contador) type)]

               [(equal? contador 3)
                (c1a matriz (cons (list (position matriz 0 0) (position matriz 3 1) (position matriz 3 2) (position matriz 3 3) (position matriz 3 4)
                                        (position matriz 0 5) (position matriz 3 6) (position matriz 3 7) (position matriz 3 8) (position matriz 3 9)
                                        (position matriz 0 10) (position matriz 3 11)(position matriz 3 12) (position matriz 3 13) (position matriz 3 14)
                                        (position matriz 0 15) (position matriz 3 16) (position matriz 3 17) (position matriz 3 18) (position matriz 3 19)
                                        (position matriz 0 20) (position matriz 3 21) (position matriz 3 22) (position matriz 3 23)(position matriz 3 24))
                                  nueva) (+ 1 contador) type)]

               [(equal? contador 4)
                (c1a matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)];; verde

               [(equal? contador 5)
                (c1a matriz (cons (list (position matriz 5 4) (position matriz 3 9) (position matriz 3 14) (position matriz 3 19) (position matriz 3 24)
                                        (position matriz 3 3) (position matriz 3 8) (position matriz 3 13) (position matriz 3 18) (position matriz 3 23)
                                        (position matriz 3 2) (position matriz 3 7) (position matriz 3 12) (position matriz 3 17) (position matriz 3 22)
                                        (position matriz 3 1) (position matriz 3 6) (position matriz 3 11) (position matriz 3 16) (position matriz 3 21)
                                        (position matriz 3 0) (position matriz 3 5) (position matriz 3 10) (position matriz 3 15) (position matriz 3 20))
                                  nueva) (+ 1 contador) type)]
               [else (reverse nueva)])]

        [(equal? type 6)
         (cond [(equal? contador 0)
                (c1a matriz (cons (list
                                   (position matriz 1 0) (position matriz 0 1) (position matriz 0 2) (position matriz 0 3) (position matriz 0 4) (position matriz 0 5)
                                   (position matriz 1 6) (position matriz 0 7) (position matriz 0 8) (position matriz 0 9) (position matriz 0 10) (position matriz 0 11)
                                   (position matriz 1 12) (position matriz 0 13) (position matriz 0 14) (position matriz 0 15) (position matriz 0 16) (position matriz 0 17)
                                   (position matriz 1 18) (position matriz 0 19) (position matriz 0 20) (position matriz 0 21) (position matriz 0 22) (position matriz 0 23)
                                   (position matriz 1 24) (position matriz 0 25) (position matriz 0 26) (position matriz 0 27) (position matriz 0 28) (position matriz 0 29)
                                   (position matriz 1 30) (position matriz 0 31) (position matriz 0 32) (position matriz 0 33) (position matriz 0 34) (position matriz 0 35))
                                  nueva) (+ 1 contador) type)]
               
               [(equal? contador 1)
                (c1a matriz (cons (list
                                   (position matriz 2 0) (position matriz 1 1) (position matriz 1 2) (position matriz 1 3) (position matriz 1 4) (position matriz 1 5)
                                   (position matriz 2 6) (position matriz 1 7) (position matriz 1 8) (position matriz 1 9) (position matriz 1 10) (position matriz 1 11)
                                   (position matriz 2 12) (position matriz 1 13) (position matriz 1 14) (position matriz 1 15) (position matriz 1 16) (position matriz 1 17)
                                   (position matriz 2 18) (position matriz 1 19) (position matriz 1 20) (position matriz 1 21) (position matriz 1 22) (position matriz 1 23)
                                   (position matriz 2 24) (position matriz 1 25) (position matriz 1 26) (position matriz 1 27) (position matriz 1 28) (position matriz 1 29)
                                   (position matriz 2 30) (position matriz 1 31) (position matriz 1 32) (position matriz 1 33) (position matriz 1 34) (position matriz 1 35))
                                  nueva) (+ 1 contador) type)]

               [(equal? contador 2)
                (c1a matriz (cons (list
                                   (position matriz 3 0) (position matriz 2 1) (position matriz 2 2) (position matriz 2 3) (position matriz 2 4) (position matriz 2 5)
                                   (position matriz 3 6) (position matriz 2 7) (position matriz 2 8) (position matriz 2 9) (position matriz 2 10) (position matriz 2 11)
                                   (position matriz 3 12) (position matriz 2 13) (position matriz 2 14) (position matriz 2 15) (position matriz 2 16) (position matriz 2 17)
                                   (position matriz 3 18) (position matriz 2 19) (position matriz 2 20) (position matriz 2 21) (position matriz 2 22) (position matriz 2 23)
                                   (position matriz 3 24) (position matriz 2 25) (position matriz 2 26) (position matriz 2 27) (position matriz 2 28) (position matriz 2 29)
                                   (position matriz 3 30) (position matriz 2 31) (position matriz 2 32) (position matriz 2 33) (position matriz 2 34) (position matriz 2 35))
                                  nueva) (+ 1 contador) type)]
               [(equal? contador 3)
                (c1a matriz (cons (list
                                   (position matriz 0 0) (position matriz 3 1) (position matriz 3 2) (position matriz 3 3) (position matriz 3 4) (position matriz 3 5)
                                   (position matriz 0 6) (position matriz 3 7) (position matriz 3 8) (position matriz 3 9) (position matriz 3 10) (position matriz 3 11)
                                   (position matriz 0 12) (position matriz 3 13) (position matriz 3 14) (position matriz 3 15) (position matriz 3 16) (position matriz 3 17)
                                   (position matriz 0 18) (position matriz 3 19) (position matriz 3 20) (position matriz 3 21) (position matriz 3 22) (position matriz 3 23)
                                   (position matriz 0 24) (position matriz 3 25) (position matriz 3 26) (position matriz 3 27) (position matriz 3 28) (position matriz 3 29)
                                   (position matriz 0 30) (position matriz 3 31) (position matriz 3 32) (position matriz 3 33) (position matriz 3 34) (position matriz 3 35))
                                  nueva) (+ 1 contador) type)]

               [(equal? contador 4)
                (c1a matriz (cons (list-ref matriz 4) nueva) (+ 1 contador) type)] ;;verde

               [(equal? contador 5)
                (c1a matriz (cons (list
                                   (position matriz 5 5) (position matriz 5 11) (position matriz 5 17) (position matriz 5 23) (position matriz 5 29) (position matriz 5 35)
                                   (position matriz 5 4) (position matriz 5 10) (position matriz 5 16) (position matriz 5 22) (position matriz 5 28) (position matriz 5 34)
                                   (position matriz 5 3) (position matriz 5 9) (position matriz 5 15) (position matriz 5 21) (position matriz 5 27) (position matriz 5 33)
                                   (position matriz 5 2) (position matriz 5 8) (position matriz 5 14) (position matriz 5 20) (position matriz 5 26) (position matriz 5 32)
                                   (position matriz 5 1) (position matriz 5 7) (position matriz 5 13) (position matriz 5 19) (position matriz 5 25) (position matriz 5 31)
                                   (position matriz 5 0) (position matriz 5 6) (position matriz 5 12) (position matriz 5 18) (position matriz 5 24) (position matriz 5 30))
                                  nueva) (+ 1 contador) type)]

              [else (reverse nueva)])]))