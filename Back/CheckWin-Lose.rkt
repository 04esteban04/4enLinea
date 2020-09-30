#lang racket


#|
*********************************************************************************************
***********************************    CÓDIGO MATRIZ   **************************************
*********************************************************************************************
|#


; Función que crea una matriz del tamaño deseado
; E: dos numeros enteros positivos con la cantidad de filas y columnas deseadas
; S: matriz mxn
(provide createMatrix)
(define (createMatrix row column)
    (cmAux row column 0 '())
    
)

; Función auxiliar de createMatrix que de forma recursiva crea las filas y columnas de la matriz
; E: dos numeros enteros positivos con la cantidad de filas y columnas deseadas
; S: matriz mxn
(define (cmAux row column cont matrix)
    (cond 
        ((= row cont) 
            matrix)
        (else
            (cmAux row 
                   column 
                   (+ cont 1) 
                   (append matrix (list (createList1 column '()))))
        )
    )
)

; Función que crea una lista del tamano deseado
; E: numero entero positivo (tamaño de la lista deseada)
; S: lista de largo n
(define (createList1 size lista)
    (cond
        ((zero? size)
            lista)
        (else
            (createList1 (- size 1) (append lista (list 0))) ))
)

; Función principal para reemplazar un valor de una matriz
; E: valor a cambiar, fila y columna como valores enteros, matriz 
; S: matriz
(provide remplaceValue)
(define (remplaceValue value row column matriz)
    (cond ((>= row (length matriz))
                matriz)
          ((>= column (length (car matriz)))
                matriz)
          (else
                (remplaceValueAux value column row matriz '())
          )
    )     
)

; Función auxiliar que realiza la recursion para reemplazar un valor en una matriz
; E: valor a cambiar, fila, columna (numeros enteros) y una matriz
; S: matriz
(define (remplaceValueAux value row column matriz newLista)
    (cond 
        ((zero? column)
            (append newLista (list (remplaceValueList value row (car matriz) '())) (cdr matriz))
        )
        (
            (remplaceValueAux value row (- column 1) (cdr matriz) (append newLista (list (car matriz))))
        ))
)

;Funcion que reemplaza un valor de una lista
;E: valor a cambiar, posición y una lista
;S: lista con valor reemplazado
(define (remplaceValueList value pos lista newLista)
    (cond
        ((zero? pos)
            (append newLista (list value) (cdr lista))
        )
        (else
            (remplaceValueList value (- pos 1) (cdr lista) (append newLista (list (car lista))))
        ))
)


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#



#|
*********************************************************************************************
******************************    CÓDIGO CHECK WIN-LOSE    **********************************
*********************************************************************************************
|#

; Función para verificar si algún jugador ganó
; E: matriz
; S: número del jugador ganador (0 = no hay ganador aún, 1 = jugador1, 2 = greedyAlgorithm, 3 = empate)
(provide checkWinLose)
(define (checkWinLose matrix)

    (cond  ; Caso en donde gana el jugador 1
            ((equal? (fourInLine matrix (- (length matrix) 1)  (- (length (car matrix)) 1)  '()) '1)
                '1
            )
            ; Caso en donde gana el algoritmo
            ((equal? (fourInLine matrix (- (length matrix) 1)  (- (length (car matrix)) 1)  '()) '2)
                '2
            )
            ; Caso en donde se da un empate
            ((equal? (verificarMatrizLlena matrix) '3)
                '3
            )
            ; Caso en donde nadie ha ganado y todavía se puede seguir jugando 
            (else
                '0
            )
    )

)

; Función que verifica cuatro fichas en línea horizontal, vertical y diagonal 
; E: matriz, número de filas de la matriz, número de columnas de la matriz, lista vacía
; S: lista con los resultados de checkHorizontal, checkVertical y checkDiagonal
(define (fourInLine matrix numFilas numColumnas lista)
    (fourInLineAux (append lista
                            (list (checkHorizontal matrix numFilas))
                            (list (checkVertical matrix numColumnas))
                            (list (checkDiagonal matrix)))
    )
)

; Función auxiliar de fourInLine
; E: lista
; S: elemento de la lista de los resultados de checkHorizontal, checkVertical y checkDiagonal
(define (fourInLineAux lista)  
    (cond ((not (zero? (car lista)))
                (car lista)
            )
           ((not (zero? (cadr lista)))
                (cadr lista)
            )
           ((not (zero? (caddr lista)))
                (caddr lista)
            )   
           (else
                '0)     
    )
)

; Función que verifica si aún hay espacios disponibles en la matriz
; E: matriz
; S: número que indica si hay espacios vacíos (0 = si, 3 = no)
(define (verificarMatrizLlena matrix)
    (verificarMatrizLlenaAux matrix (car matrix))
)

; Función auxiliar de verificarMatrizLlena
; E: matriz, fila de la matriz
; S: número que indica si hay espacios vacíos (0 = si, 3 = no)
(define (verificarMatrizLlenaAux matrix fila)
    (cond ((null? (cdr matrix))
                (cond  ((null? fila)
                            '3
                        )
                        ((equal? (car fila) '0)
                                '0
                        )
                        ((not (equal? (car fila) '0))
                                (verificarMatrizLlenaAux matrix (cdr fila))
                        )
                )
          )
          ((null? fila)
                (verificarMatrizLlenaAux (cdr matrix) (cadr matrix))
          )
          ((equal? (car fila) '0)
                '0
          )
          ((not (equal? (car fila) '0))
                (verificarMatrizLlenaAux matrix (cdr fila))
          )
    
    )
)

#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#




#|
*********************************************************************************************
******************************    CÓDIGO CHECK HORIZONTAL    ********************************
*********************************************************************************************
******************************    CÓDIGO CHECK VERTICAL      ********************************
*********************************************************************************************
|#



; Función para verificar la repetición de elementos en la matriz horizontalmente 
; E: matriz, número de filas (comenzando a contar en 0)
; S: elemento que se repita 4 veces, caso contrario un 0
(define (checkHorizontal matrix size)
    (cond 
        ((zero? size)
            (checkList (caar matrix) (cdar matrix) 0)
        )
        (else
            (cond
                ((zero? (checkList (caar matrix) (cdar matrix) 0))
                    (checkHorizontal (cdr matrix) (- size 1))
                )
                (else
                    (checkList (caar matrix) (cdar matrix) 0)
                )
            )
        )
    )
)

; Función para verificar la repetición de elementos en una lista
; E: elemento a buscar, lista, número de repeticiones
; S: elemento que se repita 4 veces, caso contrario un 0
(define (checkList elemento lista repe)
    (cond
        ((equal? repe 3)
            elemento
        )
        ((null? lista)
            0
        )
        ((zero? (car lista))
            (checkList (car lista) (cdr lista) 0)
        )
        
        ((equal? elemento (car lista))
            (checkList elemento (cdr lista) (+ repe 1))
        )
        (else
            (checkList (car lista) (cdr lista) 0)
        )
    )
)

; Función para verificar la repetición de elementos en la matriz verticalmente 
; E: matriz, número de columnas (comenzando a contar en 0)
; S: elemento que se repita 4 veces
(define (checkVertical matrix size)
    (checkHorizontal (columnToRow matrix)
                     (- (length (car matrix)) 1))
)

; Función para obtener la matriz transpuesta de la matriz ingresada
; E: matriz
; S: matriz original transpuesta
(provide columnToRow)
(define (columnToRow matrix)
    (columnToRowAux matrix (- (length (car matrix)) 1) 0 '())
)

; Función auxiliar de columnToRow
; E: matriz, número de filas, contador, lista donde se almacena la nueva matriz 
; S: matriz original transpuesta
(define (columnToRowAux matrix size tempColumn newMatrix)
    (cond 
        ((equal? tempColumn size)
            (append newMatrix (list (createList matrix tempColumn '())))
        )
        (else
            (columnToRowAux matrix size (+ tempColumn 1 ) (append newMatrix (list (createList matrix tempColumn '()))))
        )
    )
)

; Función para obtener una fila de la matriz
; E: matriz, número de fila a obtener
; S: fila deseada de la matriz ingresada
(define (getRow matrix pos)
    (cond
        ((zero? pos)
            (car matrix)
        )
        (else
            (getRow (cdr matrix) (- pos 1))
        )
    )
)

; Función para obtener un elemento de una lista
; E: lista, posición del elemento a obtener
; S: elemento deseado de la lista ingresada
(define (getElement lista pos)
    (cond
        ((zero? pos)
            (car lista)
        )
        (else
            (getElement (cdr lista) (- pos 1))
        )
    )

)

; Función para crear una nueva lista con los valores de posición dados 
; E: matriz, posición de elemento a ingresar en lista, lista vacía
; S: lista con los elementos deseados
(define (createList matrix pos newList)
    (cond
        ((null? matrix)
            newList
        )
        (else
            (createList (cdr matrix) pos (append newList (list (getElement (car matrix) pos))))
        )
    )

)



#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#



#|
*********************************************************************************************
******************************    CÓDIGO CHECK - DIAGONAL   *********************************
*********************************************************************************************
|#


; Función para generar una lista de contadores
; E: largo de la matriz -1, contador, lista vacía
; S: lista con los contadores desde cero hasta el largo de la matriz -1
(define (listOfCounters largo cont lista)
    (cond ((zero? largo)
                '())
            (else
                (append lista 
                        (list cont) 
                        (listOfCounters (- largo 1) (+ cont 1) lista))
            )
    )
)


; Función que verifica la matriz diagonalmente
; E: matriz
; S: elemento repetido 4 veces, en caso de no repetirse devuelve 0
(define (checkDiagonal matrix)
    (cond ((zero? (- (length matrix) 1))
                matrix)
           (else
                (checkDiagonalAux 
                                (list (getDiagonal matrix (length matrix) (length (car matrix))))
                                (rellenarMatriz (getDiagonalInferior matrix (length matrix) (length (car matrix)))
                                                #f)
                                (rellenarMatriz (getDiagonalSuperior matrix (length matrix) (length (car matrix)))
                                                #t)
                                (list (getDiagonal (invertirFilasMatriz matrix) (length matrix) (length (car matrix))))
                                (rellenarMatriz (getDiagonalInferior (invertirFilasMatriz matrix) (length matrix) (length (car matrix)))
                                                #f)
                                (rellenarMatriz (getDiagonalSuperior (invertirFilasMatriz matrix) (length matrix) (length (car matrix)))
                                                #t))

           ) 
    )
)

; Función auxiliar de checkDiagonal
; E: diagonal principal de la matriz, diagonales inferiores, diagonales superiores
; S: número repetido 4 veces,  en caso de no repetirse devuelve 0
(define (checkDiagonalAux diagonal1 inferiores1 superiores1 diagonal2 inferiores2 superiores2)
    (checkDiagonalAux2 (checkHorizontal diagonal1 '0)
                       (checkVertical inferiores1 (- (length inferiores1) 1))
                       (checkVertical superiores1 (- (length superiores1) 1))
                       (checkHorizontal diagonal2 '0)
                       (checkVertical inferiores2 (- (length inferiores2) 1))
                       (checkVertical superiores2 (- (length superiores2) 1)) 
    )
)

; Función auxiliar de checkDiagonalAux 
; E: número repetido en diagonal principal, número repetido en diagonales inferiores, número repetido en diagonales superiores
;    número repetido en diagonal principal invertida, número repetido en diagonales inferiores invertidas, número repetido en diagonales superiores invertidas
; S: número repetido 4 veces,  en caso de no repetirse devuelve 0
(define (checkDiagonalAux2 num1 num2 num3 num4 num5 num6)
    (cond ((not (zero? num1))
                num1)
           ((not (zero? num2))
                num2)
           ((not (zero? num3))
                num3)
           ((not (zero? num4))
                num4)
           ((not (zero? num5))
                num5)
           ((not (zero? num6))
                num6)    
           (else
                '0)     
    )
)

; Función para invertir las filas de una matriz
; E: matriz
; S: matriz con filas invertidas
(define (invertirFilasMatriz matriz)
    (cond ((zero? (length matriz))
                matriz)
            (else
                (invertirFilasMatrizAux matriz '())
            )
    )
)

; Función auxiliar de invertirFilasMatriz
; E: matriz, matriz vacía
; S: matriz con filas invertidas
(define (invertirFilasMatrizAux matriz matrizNueva)
    (cond ((null? (cdr matriz))
                (append matrizNueva
                        (list (invertirLista (car matriz))))
            )
            (else
                (append matrizNueva
                            (list (invertirLista (car matriz)))
                            (invertirFilasMatrizAux (cdr matriz) matrizNueva))
            )
    )
)


; Función para obtener la diagonal de una matriz
; E: matriz, número de filas, número de columnas
; S: lista con los elementos de la diagonal de la matriz ingresada
(define (getDiagonal matrix row column)
    (cond ((and (zero? row) (zero? column))
                matrix)
            ((> row column)
                (getDiagonalAux matrix
                                (car matrix) 
                                row
                                '0
                                (listOfCounters (+ row 1) '0 '())
                                '()))
            ((< row column)
                (getDiagonalAux matrix
                                (car matrix) 
                                column
                                '0
                                (listOfCounters (+ column 1) '0 '())
                                '()))
           (else
                (getDiagonalAux matrix
                                (car matrix) 
                                row
                                '0
                                (listOfCounters (+ row 1) '0 '())
                                '())
            ) 
    )
)

; Función auxiliar de getDiagonal para obtener la diagonal de una matriz
; E: matriz, fila de la matriz, tamaño de filas o columnas, contador para iterar, 
;    lista de contadores, lista con elementos de la diagonal 
; S: lista con los elementos de la diagonal de la matriz ingresada
(define (getDiagonalAux matrix fila size cont listaContadores listaDiagonal)
    (cond  ; Se valida si hay más filas que columnas     
           ((= cont (length (car matrix)))
                '())   
            ; Se valida si hay más columnas que filas    
           ((and (= cont 0) (null? (cdr matrix)))
                (append listaDiagonal 
                        (list (car fila))))
            ; Se valida si el elmento pertenece a la diagonal de la matriz            
           ((and (= cont 0) (>= (length (cdr listaContadores)) 1))
                (append listaDiagonal 
                        (list (car fila)) 
                        (getDiagonalAux (cdr matrix) 
                                        (cadr matrix)
                                        (- size 1)
                                        (cadr listaContadores)
                                        (cdr listaContadores)
                                        listaDiagonal)))
            (else
                (getDiagonalAux matrix
                                (cdr fila) 
                                size
                                (- cont 1) 
                                listaContadores 
                                listaDiagonal)
            )            
    )
)

; Función para obtener las diagonales inferiores de una matriz
; E: matriz, número de filas, número de columnas
; S: lista con los elementos de las diagonales inferiores de la matriz ingresada
(define (getDiagonalInferior matrix row column)
    (cond ((and (zero? row) (zero? column))
                matrix)
            ;Caso en donde hay más filas que columnas
            ((> row column)
                (getDiagonalInferiorAux matrix
                                        (car matrix) 
                                        row
                                        '0
                                        #f
                                        (listOfCounters (+ row 1) '0 '())
                                        '()
                                        '()))
            ;Caso en donde hay más columnas que filas
            ((< row column)
                (getDiagonalInferiorAux matrix
                                        (car matrix) 
                                        column
                                        '0
                                        #f
                                        (listOfCounters (+ column 1) '0 '())
                                        '()
                                        '()))
           (else
                (getDiagonalInferiorAux matrix
                                        (car matrix) 
                                        row
                                        '0
                                        #t
                                        (listOfCounters (+ row 1) '0 '())
                                        '()
                                        '())
            ) 
    )
)

; Función auxiliar de getDiagonalInferior
; E: matriz, fila, tamaño de las filas o columnas, contador, booleano, lista de contadores, lista vacía, lista vacía
; S: lista con los elementos de las diagonales inferiores de la matriz ingresada
(define (getDiagonalInferiorAux matrix fila size cont bool listaContadores listaTemp listaDiagonal)
    (cond  ; Caso matriz cuadrada
            ((and (equal? bool #t) (null? (cdr matrix)))
                (append listaDiagonal
                        (list (getListaTemp fila 
                                            cont 
                                            listaContadores 
                                            listaTemp
                                            #f))) 
            )
            ; Caso donde hay más columnas que filas
            ((and (equal? bool #f) (null? (cdr matrix)))
                (append listaDiagonal
                        (list (getListaTemp fila 
                                            cont 
                                            listaContadores 
                                            listaTemp
                                            #f))) 
            )
            ; Caso en donde se encuentre en la última fila de una matriz con más filas que columnas
            ((and (= cont (length (car matrix))) (null? (cdr matrix)))
                (append listaDiagonal
                        (list (car matrix)))
            )
            ; Caso donde se tienen más filas que columnas y se encuentran filas después de la diagonal
            ((= cont (length (car matrix)))
                (append listaDiagonal
                        (list (car matrix))
                        (getDiagonalInferiorAux (cdr matrix) 
                                                (cadr matrix) 
                                                (- size 1) 
                                                cont
                                                bool 
                                                listaContadores
                                                listaTemp 
                                                listaDiagonal))
            )
            ; Caso donde se tiene la última fila de una matriz cuadrada
            ((and (= cont 0) (null? (cdr matrix)))
                (append listaDiagonal
                        listaTemp)
            )
            ; Caso donde se agregan elementos de las listas intermedias inferiores                
            ((= cont 0)
                (getDiagonalInferiorAux (cdr matrix) 
                                        (cadr matrix) 
                                        (- size 1)
                                        (cadr listaContadores)
                                        bool 
                                        (cdr listaContadores)
                                        '()
                                        listaDiagonal))            
            ; Caso en donde se agregan los elementos de cada diagonal a una lista temporal                                    
            ((> cont 0)
                (append listaDiagonal
                        (list (getListaTemp fila 
                                            cont 
                                            listaContadores 
                                            listaTemp
                                            #f)) 
                        (getDiagonalInferiorAux (cdr matrix) 
                                                (cadr matrix) 
                                                (- size 1)
                                                (cadr listaContadores)
                                                bool 
                                                (cdr listaContadores)
                                                '()
                                                listaDiagonal))
            )                                             
    )
)


; Función para obtener las diagonales superiores de una matriz
; E: matriz, número de filas, número de columnas
; S: lista con los elementos de las diagonales superiores de la matriz ingresada
(define (getDiagonalSuperior matrix row column)
    (cond ((and (zero? row) (zero? column))
                matrix)
            ;Caso en donde hay más filas que columnas
            ((> row column)
                (getDiagonalSuperiorAux matrix
                                        (car matrix) 
                                        row
                                        '0
                                        #f
                                        (listOfCounters (+ row 1) '0 '())
                                        '()
                                        '()))
            ;Caso en donde hay más columnas que filas
            ((< row column)
                (getDiagonalSuperiorAux matrix
                                        (car matrix) 
                                        column
                                        '0
                                        #f
                                        (listOfCounters (+ column 1) '0 '())
                                        '()
                                        '()))
           (else
                (getDiagonalSuperiorAux matrix
                                        (car matrix) 
                                        row
                                        '0
                                        #t
                                        (listOfCounters (+ row 1) '0 '())
                                        '()
                                        '())
            ) 
    )
)

; Función auxiliar de getDiagonalSuperior
; E: matriz, fila, tamaño de las filas o columnas, contador, booleano, lista de contadores, lista vacía, lista vacía
; S: lista con los elementos de las diagonales superiores de la matriz ingresada
(define (getDiagonalSuperiorAux matrix fila size cont bool listaContadores listaTemp listaDiagonal)
    (cond  ; Caso en donde se encuentre la última fila de una matriz cuadrada
            ((and (null? (cdr matrix)) (equal? bool #t))
                listaTemp
            )
            ; Caso en donde ya se recorrieron las filas hasta la diagonal
            ((= cont (- (length fila) 1))
                 listaTemp
            )
            ; Caso en donde se encuentre más columnas
            ((and (null? (cdr matrix)) (equal? bool #f))
                (append listaDiagonal
                        (list (getListaTemp fila 
                                            cont 
                                            listaContadores 
                                            listaTemp
                                            #t))) 
            )
            
            ; Caso en donde se agregan los elementos de cada diagonal a una lista temporal                                    
            (else
                (append listaDiagonal
                        (list (getListaTemp fila 
                                            cont 
                                            listaContadores 
                                            listaTemp
                                            #t)) 
                        (getDiagonalSuperiorAux (cdr matrix) 
                                                (cadr matrix) 
                                                (- size 1)
                                                (cadr listaContadores)
                                                bool 
                                                (cdr listaContadores)
                                                '()
                                                listaDiagonal))
            )                                            
    )
)

; Función para obtener una lista que contiene los elementos de la fila que forma una diagonal inferior de una matriz
; E: fila, contador, lista de contadores, lista vacía, boolean(indica si se buscan diagonales inferiores o superiores)
; S: lista con los elementos de la diagonale inferiore de la matriz ingresada
(define (getListaTemp fila cont listaContadores listaTemp bool)
    (cond  ; Caso en donde se estén buscando diagonales inferiores
            ((equal? bool #f)
                (cond ((= cont 0)
                            listaTemp)
                      ((not(= cont 0))
                            (append listaTemp 
                                    (list (car fila))
                                    (getListaTemp (cdr fila)  
                                                (- cont 1) 
                                                listaContadores
                                                listaTemp
                                                bool)))
                )
            )
            ; Caso en donde se estén buscando diagonales superiores                
            ((equal? bool #t)
                (cond ((null? fila)
                            listaTemp
                       )
                      ((= cont 0)
                           (append listaTemp 
                                    (cdr fila))
                       )
                      ((= cont (- (length fila) 1))
                            listaTemp
                        )
                       (else
                            (getListaTemp (cdr fila)  
                                          (- cont 1) 
                                          listaContadores
                                          listaTemp
                                          bool)
                        )
                )
            )
    )
)

; Función para invertir una lista 
; E: lista
; S: lista invertida
(define (invertirLista lista)
    (cond ((null? lista)
                '())
           (else
                (append (invertirLista (cdr lista))
                        (list (car lista)))
           )     
    )
)

; Función para irellenar una matriz y volverla una matriz cuadrada
; E: matriz no cuadrada, boolean que indica si se buscan diagonales inferiores o superiores
; S: matriz cuadrada de la matriz ingresada
(define (rellenarMatriz matrix bool)
    (cond   ; Se buscan diagonales inferiores
            ((equal? bool #f)
                (cond
                    ((null? matrix)
                        matrix
                    )
                    ; Caso en donde se tienen más filas que columnas    
                    ((< (length (car matrix)) (length matrix))
                            (rellenarMatrizAux matrix 
                                            (createMatrix (length matrix) (length matrix))
                                            '()
                                            '0
                                            (listOfCounters (- (length matrix) 1) '0 '())
                                            #f)
                    )
                    ; Caso en donde se tiene más columnas que filas
                    ((> (length (car matrix)) (length matrix))
                        (rellenarMatrizAux matrix 
                                        (createMatrix (length (car matrix)) (length (car matrix)))
                                        '()
                                        '0
                                        (listOfCounters (- (length (car matrix)) 1) '0 '())
                                        #t)
                    ) 
                    (else
                        (rellenarMatrizAux matrix 
                                        (createMatrix (length matrix) (length matrix))
                                        '()
                                        '0
                                        (listOfCounters (- (length matrix) 1) '0 '())
                                        #f)
                    )
                )
            )
            ; se buscan diagonales superiores  
            ((equal? bool #t)
                (cond
                    ((null? matrix)
                        matrix
                    )
                    ; Caso en donde se tienen más filas que columnas    
                    ((< (length (car matrix)) (length matrix))
                            (rellenarMatrizAux2 matrix 
                                            (createMatrix (length matrix) (length matrix))
                                            '()
                                            '0
                                            (listOfCounters (- (length matrix) 1) '0 '())
                                            #f)
                    )
                    ; Caso en donde se tiene más columnas que filas
                    ((> (length (car matrix)) (length matrix))
                        (rellenarMatrizAux2 matrix 
                                        (createMatrix (length (car matrix)) (length (car matrix)))
                                        '()
                                        '0
                                        (listOfCounters (- (length (car matrix)) 1) '0 '())
                                        #t)
                    ) 
                    (else
                        (rellenarMatrizAux2 matrix 
                                        (createMatrix (length matrix) (length matrix))
                                        '()
                                        '0
                                        (listOfCounters (- (length matrix) 1) '0 '())
                                        #f)
                    )
                )
            )                        
    )
)

; Función auxiliar de rellenarMatriz que rellena las diagonales inferiores
; E: matrixActual, matrixCuadrada, matrix vacía, contador,  lista de contadores,  boolean que indica que se trata de diagonales inferiores
; S: matriz cuadrada de la matriz ingresada
(define (rellenarMatrizAux matrixActual matrixCuadrada matrixNueva cont listaCont bool)
    (cond   ; Caso en donde hay más filas o es matriz cuadrada
            ((equal? bool #f)
                (cond   
                    ; Caso en donde ya no hay elementos a procesar 
                    ((null? matrixActual)
                            matrixNueva
                    )
                    ; Caso en donde se encuentra la última fila
                    ((null? (cdr matrixActual))
                            (append matrixNueva
                            (list (pegarListas (invertirLista (car matrixActual))
                                                                (car matrixCuadrada)
                                                                '()
                                                                (length (car matrixActual))
                                                                (length (car matrixCuadrada))
                                                                (+ (car listaCont) 1)
                                                                '0
                                                                #f)))
                    )
                    ; Caso en donde hay dos filas con el mismo tamaño
                    ((and (= (length (car matrixActual)) (length (cadr matrixActual))) (= cont 0))
                            (append matrixNueva
                                    (list (pegarListas (invertirLista (car matrixActual))
                                                                    (car matrixCuadrada)
                                                                    '()
                                                                    (length (car matrixActual))
                                                                    (length (car matrixCuadrada))
                                                                    (+ (car listaCont) 1)
                                                                    '0
                                                                    #f))
                                    (rellenarMatrizAux (cdr matrixActual)
                                                    (cdr matrixCuadrada)
                                                    matrixNueva
                                                    '1
                                                    (cdr listaCont)
                                                    #f))
                    ) 
                    ; Caso en donde hay dos filas con el mismo tamaño después de encontra la primera
                    ((and (= (length (car matrixActual)) (length (cadr matrixActual))) (= cont 1))
                            (append matrixNueva
                                    (list (pegarListas (invertirLista (car matrixActual))
                                                                    (car matrixCuadrada)
                                                                    '()
                                                                    (length (car matrixActual))
                                                                    (length (car matrixCuadrada))
                                                                    (+ (car listaCont) 1)
                                                                    '0
                                                                    #f))
                                    (rellenarMatrizAux (cdr matrixActual)
                                                    (cdr matrixCuadrada)
                                                    matrixNueva
                                                    '1
                                                    (cdr listaCont)
                                                    #f))
                    )  
                    (else
                        (append matrixNueva
                                (list (pegarListas (invertirLista (car matrixActual))
                                                                (car matrixCuadrada)
                                                                '()
                                                                (length (car matrixActual))
                                                                (length (car matrixCuadrada))
                                                                '0
                                                                '0
                                                                #f))
                                (rellenarMatrizAux (cdr matrixActual)
                                                (cdr matrixCuadrada)
                                                matrixNueva
                                                '0
                                                listaCont
                                                #f))
                    )
                )
            )
            ; Caso en donde hay más columnas
            ((equal? bool #t)
                (cond ((and (null? matrixActual) (= cont (+ (length matrixCuadrada) 1)))
                            matrixNueva
                        )
                        ((and (null? matrixActual) (not(= cont (+ (length matrixCuadrada) 1))))
                            (append matrixNueva
                                    (list (pegarListas matrixActual
                                                        (car matrixCuadrada)
                                                        '()
                                                        (length matrixActual)
                                                        (length (car matrixCuadrada))
                                                        (+ (car listaCont) 1)
                                                        '0
                                                        #t))
                                    (rellenarMatrizAux matrixActual
                                                        (cdr matrixCuadrada)
                                                        matrixNueva
                                                        (+ (car listaCont) 1)
                                                        listaCont
                                                        #t))
                        )
                        (else
                            (append matrixNueva
                                    (list (pegarListas (car matrixActual)
                                                        (car matrixCuadrada)
                                                        '()
                                                        (length (car matrixActual))
                                                        (length (car matrixCuadrada))
                                                        (+ (car listaCont) 1)
                                                        '0
                                                        #t))
                                    (rellenarMatrizAux (cdr matrixActual)
                                                        (cdr matrixCuadrada)
                                                        matrixNueva
                                                        (+ (car listaCont) 1)
                                                        listaCont
                                                        #t))
                        )
                )
            )            
    )
)

; Función auxiliar de rellenarMatriz que rellena las diagonales superiores
; E: matrixActual, matrixCuadrada, matrix vacía, contador,  lista de contadores,  boolean que indica que se trata de diagonales superiores
; S: matriz cuadrada de la matriz ingresada
(define (rellenarMatrizAux2 matrixActual matrixCuadrada matrixNueva cont listaCont bool)
    (cond 
            ; Caso en donde hay más filas o es matriz cuadrada
            ((equal? bool #f)

                (cond  
                    ; Caso en donde ya no hay elementos a procesar 
                    ((null? matrixActual)
                            matrixNueva
                    )
                    ; Caso en donde se encuentra la última fila
                    ((null? (cdr matrixActual))
                            (append matrixNueva
                            (list (pegarListas (car matrixActual)
                                                (car matrixCuadrada)
                                                '()
                                                (length (car matrixActual))
                                                (length (car matrixCuadrada))
                                                (+ (car listaCont) 1)
                                                '0
                                                #f)))
                    )
                    ; Caso en donde hay dos filas con el mismo tamaño
                    ((and (= (length (car matrixActual)) (length (cadr matrixActual))) (= cont 0))
                            (append matrixNueva
                                    (list (pegarListas (car matrixActual)
                                                        (car matrixCuadrada)
                                                        '()
                                                        (length (car matrixActual))
                                                        (length (car matrixCuadrada))
                                                        '0
                                                        '0
                                                        #f))
                                    (rellenarMatrizAux2 (cdr matrixActual)
                                                    (cdr matrixCuadrada)
                                                    matrixNueva
                                                    '1
                                                    (cdr listaCont)
                                                    #f))
                    ) 
                    ; Caso en donde hay dos filas con el mismo tamaño después de encontra la primera
                    ((and (= (length (car matrixActual)) (length (cadr matrixActual))) (= cont 1))
                            (append matrixNueva
                                    (list (pegarListas (car matrixActual)
                                                        (car matrixCuadrada)
                                                        '()
                                                        (length (car matrixActual))
                                                        (length (car matrixCuadrada))
                                                        (+ (car listaCont) 1)
                                                        '0
                                                        #f))
                                    (rellenarMatrizAux2 (cdr matrixActual)
                                                    (cdr matrixCuadrada)
                                                    matrixNueva
                                                    '1
                                                    (cdr listaCont)
                                                    #f))
                    )  
                    (else
                        (append matrixNueva
                                (list (pegarListas (car matrixActual)
                                                    (car matrixCuadrada)
                                                    '()
                                                    (length (car matrixActual))
                                                    (length (car matrixCuadrada))
                                                    '0
                                                    '0
                                                    #f))
                                (rellenarMatrizAux2 (cdr matrixActual)
                                                (cdr matrixCuadrada)
                                                matrixNueva
                                                '0
                                                listaCont
                                                #f))
                    )
                )
            )
            ; Caso en donde hay más columnas
            ((equal? bool #t)
                (cond ((and (null? matrixActual) (= cont (+ (length matrixCuadrada) 1)))
                            matrixNueva
                        )
                        ((and (null? matrixActual) (not(= cont (+ (length matrixCuadrada) 1))))
                            (append matrixNueva
                                    (list (pegarListas matrixActual
                                                        (car matrixCuadrada)
                                                        '()
                                                        (length matrixActual)
                                                        (length (car matrixCuadrada))
                                                        (+ (car listaCont) 1)
                                                        '0
                                                        #t))
                                    (rellenarMatrizAux matrixActual
                                                        (cdr matrixCuadrada)
                                                        matrixNueva
                                                        (+ (car listaCont) 1)
                                                        listaCont
                                                        #t))
                        )
                        (else
                            (append matrixNueva
                                    (list (pegarListas (car matrixActual)
                                                        (car matrixCuadrada)
                                                        '()
                                                        (length (car matrixActual))
                                                        (length (car matrixCuadrada))
                                                        (+ (car listaCont) 1)
                                                        '0
                                                        #t))
                                    (rellenarMatrizAux (cdr matrixActual)
                                                        (cdr matrixCuadrada)
                                                        matrixNueva
                                                        (+ (car listaCont) 1)
                                                        listaCont
                                                        #t))
                        )
                )
            ) 
    )
)

; Función para concatenar dos listas
; E: lista1, lista2, lista vacía, largo de lista 1, largo de lista 2, contador, largo total, boolean que indica cuando se terminan de agregar elementos)
; S: listas ingresadas concatenadas
(define (pegarListas lista1 lista2 nuevaLista largoLista1 largoLista2 cont largoTotal bool)
    (cond   ; Casos de matriz cuadrada y con más filas 
            ((equal? bool #f)
                (cond   ((and (null? lista1) (not (= largoLista2 largoTotal)))
                            (append nuevaLista
                                    (list (car lista2))
                                    (pegarListas lista1 (cdr lista2) nuevaLista largoLista1 largoLista2 '0 (+ largoTotal 1) bool))
                        )
                        ((= largoLista2 largoTotal)
                            (append nuevaLista
                                    '())
                        )
                        ; Caso en donde se tienen filas por debajo de la diagonal
                        ((= cont 0)
                            (append nuevaLista
                                    (list (car lista1))
                                    (pegarListas (cdr lista1) (cdr lista2) nuevaLista largoLista1 largoLista2 '0 (+ largoTotal 1) bool))
                        )
                        ; Caso en donde se tienen filas del mismo tamaño 
                        ((= cont 1)
                            (append nuevaLista
                                        (list (car lista1))
                                        (pegarListas (cdr lista1) lista2 nuevaLista largoLista1 largoLista2 '0 (+ largoTotal 1) bool))
                        )
                        ; Caso en donde se debe rellenar con ceros           
                        ((and (> cont 0) (not(equal? cont 1)))
                                (append nuevaLista
                                        (list (car lista2))
                                        (pegarListas lista1 (cdr lista2) nuevaLista largoLista1 largoLista2 (- cont 1) (+ largoTotal 1) bool))
                        )
                )
            )
            ; Caso de matriz con más columnas
            ((equal? bool #t)
                (cond ((and (null? lista1) (not (= largoLista2 largoTotal)))
                            (append nuevaLista
                                    (list (car lista2))
                                    (pegarListas lista1 (cdr lista2) nuevaLista largoLista1 largoLista2 cont (+ largoTotal 1) bool))
                       )
                       ((= largoLista2 largoTotal)
                            (append nuevaLista
                                    '())
                        )
                       (else
                            (append nuevaLista
                                    (list (car lista1))
                                    (pegarListas (cdr lista1) (cdr lista2) nuevaLista largoLista1 largoLista2 cont (+ largoTotal 1) #t))
                        )
                )
            )                  
    )
)


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#



#|
*********************************************************************************************
****************************    CÓDIGO GREEDY ALGORITHM    **********************************
*********************************************************************************************
|#


; Se define el conjunto de candidatos: posiciones 0, 1 ,2 en una matriz

; Función que reemplaza un elemento por otro dado en un lista específica
; E: elemento a reemplazar, elemento deseado, lista
; S: lista con el elemento deseado en la posición del elemento a reemplazar
(define (remplace element newElement lista)
    (remplaceAux element newElement lista '())
)

; Función auxiliar de remplace
; E: elemento a reemplazar, elemento deseado, lista
; S: lista con el elemento deseado en la posición del elemento a reemplazar
(define (remplaceAux element newElement lista res)
    (cond
        ((null? lista)
            res
        )
        ((equal? (car lista) element)
            (remplaceAux
         element newElement (cdr lista) (append res (list newElement)))
        )
        (else
        (remplaceAux
     element newElement (cdr lista) (append res (list (car lista)))))

    )
)

; Función para reemplazar un elemento en una matriz por el elemento deseado 
; E: elemento a reemplazar, elemento deseado, matriz
; S: matriz con el elemento a cambiar sustituido por el elemento deseado
(define (remplaceMatrix element newElement matrix)
    (remplaceMatrixAux element newElement matrix '())
)

; Función auxiliar de remplaceMatrixAux 
; E: elemento a reemplazar, elemento deseado, matriz, matriz vacía
; S: matriz con el elemento a cambiar sustituido por el elemento deseado
(define (remplaceMatrixAux element newElement matrix newMatrix)
    (cond
        ((null? matrix)
            newMatrix
        )
        (else
            (remplaceMatrixAux element newElement (cdr matrix) (append newMatrix (list (remplace element newElement (car matrix)))))
        )
    
    )

)

; Función para invertir la matriz
; E: matriz
; S: matriz con la última fila de primero
(define (turnMatrix matrix)
    (turnMatrixAux matrix '())
)

; Función auxiliar de turnMatrix
; E: matriz, matriz vacía
; S: matriz con la última fila de primero
(define (turnMatrixAux matrix newMatrix)
    (cond
        ((null? matrix)
            newMatrix
        )
        (else
            (turnMatrixAux (cdr matrix) (append (list (car matrix)) newMatrix))
        )
    )
)

;Funciones para encontrar siguiente lugar en horizontal
;------------------------------------------------------
;------------------------------------------------------
;------------------------------------------------------

; Función que cuenta las repeticiones de un elemento en una lista
; E: elemento a buscar, lista
; S: cantidad de repeticiones del elemento ingresado
(define (contList element lista)
    (contListAux element lista 0)
)

; Función auxiliar de contList que relaiza la recursión
; E: elemento a buscar, lista, cero
; S: cantidad de repeticiones del elemento ingresado
(define (contListAux element lista res)
    (cond
        ((null? lista) res)

        ((equal? (car lista) element)
            (contListAux element (cdr lista) (+ res 1))
        )
        (else (contListAux element (cdr lista) res))
    )
)

; Función que determina si existe algun campo en una lista para insertar ficha
; E: lista
; S: boolean que indica si se puede insertar la ficha o no
(define (canAdd? lista)
    (cond
        ((zero? (contList 0 lista))
            #f
        )
        (else
            #t
        )
    )
)

; Función que retorna el máximo de repeticiones consecutivas en una lista
; E: elemento a buscar, lista
; S: cantidad de repeticiones consecutivas en la lista dada
(define (maxRepe element lista)
    (maxRepeAux element lista 0 0)
)

; Función auxiliar de maxRepe que realiza toda la recursión
; E: elemento a buscar, lista, cantidad de repeticiones, repeticiones temporales
; S: cantidad de repeticiones consecutivas en la lista dada
(define (maxRepeAux element lista repe tempRepe)
    (cond
        ((and (> tempRepe repe) (null? lista))
            tempRepe
        )
        ((null? lista)
            repe
        )
        ((> tempRepe repe)
            (maxRepeAux element lista tempRepe tempRepe)
        )
        ((equal? element (car lista))
            (maxRepeAux element (cdr lista) repe (+ tempRepe 1)) 

        )
        ((not (equal? element (car lista)))
            (maxRepeAux element (cdr lista) repe 0)
        )
    )
)

; Función que encuentra la posición para colocar la ficha en la mejor posicion
; E: elemento a colocar, lista
; S: posición temporal del elemento en la lista
(define (findPlace element lista)
    (findPlaceAux element lista lista 0 0)
)

; Función auxiliar de findPlace que realiza toda la recursividad
; E: elemento a colocar, lista, lista, cantidad de repeticiones, posición temporal
; S: posición temporal del elemento en la lista
(define (findPlaceAux element lista listaPerm repe tempPos)
    (cond
        ((equal? repe (maxRepe element listaPerm))
            (cond
                ((>= tempPos (- (length listaPerm) 1)) 
                    (- tempPos (maxRepe element listaPerm) 1)
                )
                ((not (zero? (car lista)))
                    (- tempPos (maxRepe element listaPerm) 1)
                )
                (else
                tempPos)
                
            )
        )
        ((equal? (car lista) element)
            (findPlaceAux element (cdr lista) listaPerm (+ repe 1) (+ tempPos 1))
        )
        (else
            (findPlaceAux element (cdr lista) listaPerm 0 (+ tempPos 1))
        )
    )
)
; Función que determina las posiciones de los campos con 0
; E: lista
; S: lista con las posibles posibles posiciones para colocar una ficha
(define (availableField lista)
    (availableFieldAux lista '() 0)
)

;funcion auxiliar de availableField
; E: lista, lista vacía, contador en cero
; S: lista con las posibles posibles posiciones para colocar una ficha
(define (availableFieldAux lista res tempPos)
    (cond
        ((null? lista)
            res
        )
        ((zero? (car lista))
            (availableFieldAux (cdr lista) (append res (list tempPos)) (+ tempPos 1))
        )
        (else
            (availableFieldAux (cdr lista) res (+ tempPos 1))
        )
    )
)

; Función que obtiene el elemento en la posicion determinada
; E: posición del elemento, lista
; S: elemento en la posición deseada de la lista
(define (getElement2 pos lista)
    (cond
        ((zero? pos)
            (car lista)
        )
        (else (getElement2 (- pos 1) (cdr lista)))
    )

)

; Función que inserta/remplaza un elemento en una lista
; E: posición en la cual se insertará el elemento, elemento, lista
; S: lista con el elemento insertado en la posición indicada
(define (put pos element lista)
    (putAux pos element lista '())
)

; Función auxiliar de put
; E: posición en la cual se insertará el elemento, elemento, lista, lista vacía
; S: lista con el elemento insertado en la posición indicada
(define (putAux pos element lista newLista)
    (cond
        ((zero? pos)
            (append newLista (list element) (cdr lista))
        )
        (else
            (putAux (- pos 1) element (cdr lista) (append newLista (list (car lista))))
        )
    
    )
)
; Función que determina el mejor espacio para colocar la siguiente ficha en una lista
; E: elemento, lista, elemento alterno
; S: lista con el elemento insertado o en un su defecto un #f en caso de no poder insertarlo
(define (selectPlace element lista altElement)
    (cond
        ((canAdd? lista)
            (cond
                ;caso cuando inserta fuera de la lista a la izquierda
                ((< (findPlace element lista) 0)
                    (put (car (availableField lista)) (+ element altElement) lista) 
                )
                ;Cuando no hay campo disponible en el mayor numero de repeticiones
                ((not (zero? (getElement2 (findPlace element lista) lista)))
                    (put (car (availableField lista)) (+ element altElement) lista) 
                )
                ;cuando encuentra campo optimo en la lista horizontal
                (else
                    (put (findPlace element lista) (+ element altElement) lista)
                )
            )
        )
        (else
            #f
        )
    )
)

; Función que ejecuta el algoritmo codicioso
; E: matriz
; S: matriz con el elemento insertado en la mejor posición encontrada por el algoritmo
(provide greedyAlgorithm)
(define (greedyAlgorithm matrix)
    (findBestSolution matrix)
)

; Función que busca un solución temporal al algoritmo
; E: elemento a buscar, matriz
; S: matriz con una solución parcial
(define (findTemporal element matrix)
    (findTemporalAux element matrix 0 '() '())
)

; Función auxiliar de findTemporalAux
; E: elemento a buscar, matriz, número de repeticiones, matriz vacía, primer fila de la matriz
; S: matriz con una solución parcial
(define (findTemporalAux element matrix repe newMatrix tempList)

    (cond
        ((and (null? matrix) (null? tempList))
            (remplaceMatrix 4 0 newMatrix)
        )
        ((canAdd? tempList)
            (cond
                ((< repe (maxRepe element (selectPlace element tempList 0)))
                    (findTemporalAux element matrix (maxRepe element (selectPlace element tempList 0)) (remplaceMatrix 3 4 newMatrix) (selectPlace element (remplace 3 4 tempList) 1))
                )
                (else
                    (findTemporalAux element matrix repe newMatrix (selectPlace element tempList 2))
                )
            )
        )

        ((not (canAdd? tempList))
            (cond
                ((null? matrix)
                    (findTemporalAux element matrix repe (append newMatrix (list tempList)) '())
                )
                ((null? tempList)
                    (findTemporalAux element (cdr matrix) repe newMatrix (car matrix))
                )
                (else
                    (findTemporalAux element (cdr matrix) repe (append newMatrix (list tempList)) (car matrix))
                )
            
            )
        )
    )
)

; Función para obtener la solución más óptima
; E: matriz
; S: matriz con la solución final
(define (findBestSolution matrix)
    (findBestSolutionAux (findTemporal '2 matrix)
                         (findTemporal '2 (columnToRow matrix))
                         (getTempRow (findTemporal '2 matrix) (car (findTemporal '2 matrix)))
                         (getTempRow (findTemporal '2 (columnToRow matrix)) (car (findTemporal '2 (columnToRow matrix))))
    )
)

; Función auxiliar de findBestSolution
; E: matriz horizontal, matriz vertical, fila de solución parcial de matriz horizontal,
;    fila de solución parcial de matriz vertical
; S: matriz con la solución final
(define (findBestSolutionAux matrix1 matrix2 fila1 fila2)
    (cond ; Caso en donde es mejor vertical
          ((< (maxRepe '2 fila1) (maxRepe '2 fila2))
                (checkCorrectSolution (columnToRow matrix2))  
          )
          ; Caso en donde es mejor horizontal
          ((>= (maxRepe '2 fila1) (maxRepe '2 fila2))
                (checkCorrectSolution matrix1)
          )      
    )
)

; Función para obtener la fila de la solución parcial
; E: matriz, fila de la matriz ingresada
; S: fila de la matriz con la solución parcial
(define (getTempRow matrix fila)
    (cond
        ((null? (cdr matrix))
           (cond
                ((null? fila)
                    '()
                )
                ((not (equal? (car fila) '3))
                    (getTempRow matrix (cdr fila))
                )
                (else
                    (car matrix)
                )
           ) 
        )
        ((null? fila)
            (getTempRow (cdr matrix) (cadr matrix))
        )
        ((not (equal? (car fila) '3))
            (getTempRow matrix (cdr fila))
        )
        (else
            (car matrix)
        )
    )
)

; Función solución que verifica la posición correcta de la ficha ingresada
; E: matriz
; S: matriz con la posición deseada cambiada por un 2
(define (checkCorrectSolution matrix)
    (cond ((null? matrix)
                '())
           (else
                (checkCorrectSolutionAux matrix matrix (car matrix) (cadr matrix) '0 '0)
           )
    )
)
 
; Función auxiliar de checkCorrectSolution
; E: matriz, matriz, primera fila de matriz, segunda fila de matriz, número de fila, número de columna
; S: matriz con la posición deseada cambiada por un 2
(define (checkCorrectSolutionAux matrix matrixOriginal fila1 fila2 numFila numPos)
    (cond   ; Caso en donde se tiene solo una fila en la matrixOriginal
            ((null? (cdr matrixOriginal))
                (cond   ((null? fila1)
                            matrix
                        )
                        ; Caso en donde se busca en la última fila
                        ((not(equal? (car fila1) '3))
                            (checkCorrectSolutionAux matrix matrixOriginal (cdr fila1) fila2 numFila (+ numPos 1))
                        )
                        (else   
                            (remplaceMatrix '3 '2 matrix)
                        )
                )
            )
            ; Se realiza la búsqueda en la siguiente fila de la matriz
            ((and (null? fila1) (>= (length matrixOriginal) 3))
                (checkCorrectSolutionAux matrix (cdr matrixOriginal) (cadr matrixOriginal) (caddr matrixOriginal) (+ numFila 1) '0)
            )
            ; Se realiza la búsqueda en la siguiente fila de la matriz cuando solo se tienen 2 filas
            ((and (null? fila1) (= (length matrixOriginal) 2))
                (checkCorrectSolutionAux matrix (cdr matrixOriginal) (cadr matrixOriginal) '() (+ numFila 1) '0)
            )
            ; Caso en donde se tienen más de 2 filas
            ((and (not(equal? (car fila1) '3)) (>= (length matrixOriginal) 2))
                (checkCorrectSolutionAux matrix matrixOriginal (cdr fila1) (cdr fila2) numFila (+ numPos 1))
            )
            ; Caso en donde se encuentra la posición deseada
            ((and (equal? (car fila1) '3) (equal? (car fila2) '0))
                (checkCorrectSolutionAux (changeTempValue matrix numFila numPos '0 '() #f)
                                         (changeTempValue matrix numFila numPos '0 '() #f)
                                         (car (changeTempValue matrix numFila numPos '0 '() #f))
                                         (cadr (changeTempValue matrix numFila numPos '0 '() #f))
                                         '0
                                         '0)
            )
            (else
                (remplaceMatrix '3 '2 matrix)
            )
    
    )
)

; Función que cambia un valor temporal en una fila de la matriz dada
; E: matriz, posición de fila, posición de columna, contador, matriz vacía, boolean que indica si ya se cambió el elemento de la matriz 
; S: matriz con el elemento buscado cambiado de posición
(define (changeTempValue matrix rowPos columnPos cont newMatrix bool)
    (cond   ((null? matrix)
                newMatrix
            )
            ; Caso en donde se debe cambiar el 3
            ((equal? cont rowPos)
                (append newMatrix
                        (list (put columnPos '0 (car matrix)))
                        (changeTempValue (cdr matrix)
                                          rowPos
                                          columnPos
                                          (+ cont 1)
                                          newMatrix
                                          #t))
            )
            ; Caso en donde se cambia el 0 por un 3
            ((equal? bool #t)
                (append newMatrix
                        (list (put columnPos '3 (car matrix)))
                        (changeTempValue (cdr matrix)
                                          rowPos
                                          columnPos
                                          (+ cont 1)
                                          newMatrix
                                          #f))
            )
            (else
                (append newMatrix
                        (list (car matrix))
                        (changeTempValue (cdr matrix)
                                          rowPos
                                          columnPos
                                          (+ cont 1)
                                          newMatrix
                                          #f))
            )
    )
)


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#

