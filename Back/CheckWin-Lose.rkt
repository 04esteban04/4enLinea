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
; E: valor a cambiar, fila y columna como valores enteros. Matriz es una lista que contiene una matriz
; S: matriz
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

; 
; E:
; S:
(define (checkWin-Lose matrix)

    (cond 
        ((equal? (4inLine matrix (length matrix) 0 0) 1)
            ;codigo del ganador 1
            1
        )
        ((equal? (4inLine matrix (length matrix) 0 0) 2)
            ;codigo del ganador 2
            2
        )
        (else
            ;codigo de empate
            0
        )
    )

)

; 
; E:
; S:
(define (4inLine matrix size temp cont)
    (display '" ")
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
(checkDiagonal '(   (1 1 0 0 0)
                    (2 1 1 1 2)
                    (0 0 1 2 2)
                    (0 1 2 0 1)
                    (0 0 1 2 0)
                    (0 0 0 0 0)
                    (0 0 0 0 0)))


(rellenarMatriz (getDiagonalSuperior '( (1 0 0 0 0)
                                        (2 1 2 3 1)
                                        (3 2 1 0 2)
                                        (4 3 2 3 1)
                                        (5 4 3 2 1)) '4 '4) #t)

(rellenarMatriz (getDiagonalSuperior '( (1 0 0 0 0)
                                        (2 1 2 3 1)
                                        (3 2 1 0 8)
                                        (4 3 2 3 5)
                                        (5 4 3 2 1)
                                        (6 5 4 3 2)
                                        (7 6 5 4 3)
                                        (8 7 6 5 4)) '7 '4) #t)


(rellenarMatriz (getDiagonalSuperior '( (1 0 0 0 0 0 0 0 0 0)
                                        (2 1 3 2 1 4 5 0 0 0)
                                        (3 2 1 0 0 2 1 0 0 0)
                                        (4 3 2 3 0 4 0 0 0 0)) '3 '9) #t)

                     
|#


#|
(getDiagonal '((1 0 0 0 0)
               (2 1 0 0 0)
               (3 2 1 0 0)
               (4 3 2 3 0)
               (5 4 3 2 1)
               (6 5 4 3 2)
               (7 6 5 4 3)
               (8 7 6 5 4)) '7 '4)

(rellenarMatriz (getDiagonalInferior '( (1 0 0 0 0 1) 
                                        (2 1 0 0 0 1)
                                        (3 2 1 0 0 1)
                                        (4 3 2 1 0 1)
                                        (5 4 3 2 1 2)
                                        (6 5 4 3 2 1)) '5 '5))               

(rellenarMatriz (getDiagonalInferior '( (1 0 0 0 0 1 2 3) 
                                        (2 1 0 0 0 1 2 3)
                                        (3 2 1 0 0 1 2 3)
                                        (4 3 2 1 0 1 2 3)
                                        (5 4 3 2 1 2 2 3)
                                        (6 5 4 3 2 1 2 3)) '5 '7))

(rellenarMatriz (getDiagonalInferior '( (1 0 0 0 0)
                                        (2 1 0 0 0)
                                        (3 2 1 0 0)
                                        (4 3 2 3 0)
                                        (5 4 3 2 1)
                                        (6 5 4 3 2)
                                        (7 6 5 4 3)
                                        (8 7 6 5 4)) '7 '4))

|#


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#

