matrizCorrecta = [
    [4,4,4,4,4,4,4,4,4],
    [5,5,5,5,5,5,5,5,5],
    [0,0,0,0,0,0,0,0,0],
    [6,6,6,6,6,6,6,6,6],
    [1,1,1,1,1,1,1,1,1],
    [3,3,3,3,3,3,3,3,3]]
"""
            [5,5,5,
            5,5,5,
            5,5,5]               
                        
[3,3,3,     [0,0,0,     [4,4,4,
 3,3,3,     0,0,0,      4,4,4,
 3,3,3]     0,0,0]      4,4,4]

            [6,6,6,
             6,6,6,
             6,6,6]

            [1,1,1,
             1,1,1,
             1,1,1]

matrizCorrecta = [
    [4,4,4,4,4,4,4,4,4],
    [5,5,5,5,5,5,5,5,5],
    [0,0,0,0,0,0,0,0,0],
    [6,6,6,6,6,6,6,6,6],
    [1,1,1,1,1,1,1,1,1],
    [3,3,3,3,3,3,3,3,3]]


#0 = blanco
#1 = amarillo
#3 = azul
#4 = verde
#5 = rojo
#6 = naranja

[4,4,4,4,4,4,4,4,4]#0
[5,5,5,5,5,5,5,5,5]#1
[0,0,0,0,0,0,0,0,0]#2
[6,6,6,6,6,6,6,6,6]#3
[1,1,1,1,1,1,1,1,1]#4
[3,3,3,3,3,3,3,3,3]#5


"""

def imprimir():
    n = 0
    for i in matrizCorrecta:
        color = ""
        if n == 0:
            color = "verde"
        elif n== 1:
            color = "rojo"
        elif n==2 :
            color = "blanco"
        elif n==3:
            color = "naranja"
        elif n==4:
            color = "amarillo"

        else:
            color = "azul"
        l1 = [i[0],i[1],i[2]]
        l2 = [i[3],i[4],i[5]]
        l3 = [i[6],i[7],i[8]]
        print(l1)
        print(str(l2) + "      " + color)
        print(l3)
        print("\n")
        n += 1

    
def LogicaCubo(mov):
    global matrizCorrecta
    matriz = matrizCorrecta
    if mov == "f1d":
        matriz[2][0] = matrizCorrecta[5][0]
        matriz[2][1] = matrizCorrecta[5][1]
        matriz[2][2] = matrizCorrecta[5][2]

        matriz[5][0] = matrizCorrecta[4][0]
        matriz[5][1] = matrizCorrecta[4][1]
        matriz[5][2] = matrizCorrecta[4][2]

        matriz[4][0] = matrizCorrecta[0][0]
        matriz[4][1] = matrizCorrecta[0][1]
        matriz[4][2] = matrizCorrecta[0][2]

        matriz[0][0] = matrizCorrecta[2][0]
        matriz[0][1] = matrizCorrecta[2][1]
        matriz[0][2] = matrizCorrecta[2][2]


        #cara de arriba
        matriz[1][0] = matrizCorrecta[1][2]
        matriz[1][3] = matrizCorrecta[1][1]
        matriz[1][6] = matrizCorrecta[1][0]
        matriz[1][7] = matrizCorrecta[1][3]
        matriz[1][8] = matrizCorrecta[1][6]
        matriz[1][5] = matrizCorrecta[1][7]
        matriz[1][2] = matrizCorrecta[1][8]
        matriz[1][1] = matrizCorrecta[1][5]

        matrizCorrecta = matriz

    elif mov == "f2d":
        matriz[2,3] = matrizCorrecta[5,3]
        matriz[2,4] = matrizCorrecta[5,4]
        matriz[2,5] = matrizCorrecta[5,5]

        matriz[5,3] = matrizCorrecta[4,3]
        matriz[5,4] = matrizCorrecta[4,4]
        matriz[5,5] = matrizCorrecta[4,5]

        matriz[4,3] = matrizCorrecta[0,3]
        matriz[4,4] = matrizCorrecta[0,4]
        matriz[4,5] = matrizCorrecta[0,5]

        matriz[0,3] = matrizCorrecta[2,3]
        matriz[0,4] = matrizCorrecta[2,4]
        matriz[0,5] = matrizCorrecta[2,5]
        
        matrizCorrecta = matriz

    elif mov == "f3d":
        matriz[2,6] = matrizCorrecta[5,6]
        matriz[2,7] = matrizCorrecta[5,7]
        matriz[2,8] = matrizCorrecta[5,8]

        matriz[5,6] = matrizCorrecta[4,6]
        matriz[5,7] = matrizCorrecta[4,7]
        matriz[5,8] = matrizCorrecta[4,8]

        matriz[4,6] = matrizCorrecta[0,6]
        matriz[4,7] = matrizCorrecta[0,7]
        matriz[4,8] = matrizCorrecta[0,8]

        matriz[0,6] = matrizCorrecta[2,6]
        matriz[0,7] = matrizCorrecta[2,7]
        matriz[0,8] = matrizCorrecta[2,8]

        #cara abajo:
        matriz[3,0] = matrizCorrecta[3,2]
        matriz[3,3] = matrizCorrecta[3,1]
        matriz[3,6] = matrizCorrecta[3,0]
        matriz[3,7] = matrizCorrecta[3,3]
        matriz[3,8] = matrizCorrecta[3,6]
        matriz[3,5] = matrizCorrecta[3,7]
        matriz[3,2] = matrizCorrecta[3,8]
        matriz[3,1] = matrizCorrecta[3,5]

        matrizCorrecta = matriz


    elif mov == "c1a":
        matriz[2,0] = matrizCorrecta[3,0]
        matriz[2,3] = matrizCorrecta[3,3]
        matriz[2,6] = matrizCorrecta[3,6]

        matriz[3,0] = matrizCorrecta[4,0]
        matriz[3,3] = matrizCorrecta[4,3]
        matriz[3,6] = matrizCorrecta[4,6]

        matriz[4,0] = matrizCorrecta[1,0]
        matriz[4,3] = matrizCorrecta[1,3]
        matriz[4,6] = matrizCorrecta[1,6]

        matriz[1,0] = matrizCorrecta[2,0]
        matriz[1,3] = matrizCorrecta[2,3]
        matriz[1,6] = matrizCorrecta[2,6]

        #cara azul del lado
        matriz[5,0] = matrizCorrecta[5,6]
        matriz[5,1] = matrizCorrecta[5,3]
        matriz[5,2] = matrizCorrecta[5,0]
        matriz[5,5] = matrizCorrecta[5,1]
        matriz[5,8] = matrizCorrecta[5,2]
        matriz[5,7] = matrizCorrecta[5,5]
        matriz[5,6] = matrizCorrecta[5,8]
        matriz[5,3] = matrizCorrecta[5,7]

        matrizCorrecta = matriz

    elif mov == "c2a":
        matriz[2,1] = matrizCorrecta[3,1]
        matriz[2,4] = matrizCorrecta[3,4]
        matriz[2,7] = matrizCorrecta[3,7]

        matriz[3,1] = matrizCorrecta[4,1]
        matriz[3,4] = matrizCorrecta[4,4]
        matriz[3,7] = matrizCorrecta[4,7]

        matriz[4,1] = matrizCorrecta[1,1]
        matriz[4,4] = matrizCorrecta[1,4]
        matriz[4,7] = matrizCorrecta[1,7]

        matriz[1,1] = matrizCorrecta[2,1]
        matriz[1,4] = matrizCorrecta[2,4]
        matriz[1,7] = matrizCorrecta[2,7]

        matrizCorrecta = matriz

    elif mov == "c3a":
        matriz[2,2] = matrizCorrecta[3,2]
        matriz[2,5] = matrizCorrecta[3,5]
        matriz[2,8] = matrizCorrecta[3,8]

        matriz[3,2] = matrizCorrecta[4,2]
        matriz[3,5] = matrizCorrecta[4,5]
        matriz[3,8] = matrizCorrecta[4,8]

        matriz[4,2] = matrizCorrecta[1,2]
        matriz[4,5] = matrizCorrecta[1,5]
        matriz[4,8] = matrizCorrecta[1,8]

        matriz[1,2] = matrizCorrecta[2,2]
        matriz[1,5] = matrizCorrecta[2,5]
        matriz[1,8] = matrizCorrecta[2,8]

        #movimiento cara verde derecha
        matriz[0,0] = matrizCorrecta[0,2]
        matriz[0,1] = matrizCorrecta[0,5]
        matriz[0,2] = matrizCorrecta[0,8]
        matriz[0,5] = matrizCorrecta[0,7]
        matriz[0,8] = matrizCorrecta[0,6]
        matriz[0,7] = matrizCorrecta[0,3]
        matriz[0,6] = matrizCorrecta[0,0]
        matriz[0,3] = matrizCorrecta[0,1]

        matrizCorrecta = matriz

    imprimir()







        
