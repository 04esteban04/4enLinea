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
    (checkHorizontal (columnToRow matrix (- (length (car matrix)) 1) 0 '())
                     (- (length (car matrix)) 1))
)

; Función para obtener la matriz transpuesta de la matriz ingresada
; E: matriz, número de filas, contador, lista donde se almacena la nueva matriz 
; S: matriz original transpuesta
(provide columnToRow)
(define (columnToRow matrix size tempColumn newMatrix)
    (cond 
        ((equal? tempColumn size)
            (append newMatrix (list (createList matrix tempColumn '())))
        )
        (else
            (columnToRow matrix size (+ tempColumn 1 ) (append newMatrix (list (createList matrix tempColumn '()))))
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


; 
; E:
; S:
(define (checkDiagonal matrix size)
    (cond ((and (zero? (- (length matrix) 1)) (zero? size))
                matrix)
           (else
                (checkDiagonalAux matrix 
                                    size
                                    (listOfCounters (- (length (car matrix)) 1) '0 '()))
           ) 
    )
)

; 
; E:
; S:
(define (checkDiagonalAux matrix size listaContadores)
    (cond ((zero? size)
                '0)
    
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

; empezando en 0
(define (getDiagonalInferior matrix row column)
    (cond ((and (zero? row) (zero? column))
                matrix)
            ((> row column)
                (getDiagonalInferiorAux matrix
                                        (car matrix) 
                                        row
                                        '0
                                        (listOfCounters (+ row 1) '0 '())
                                        '()
                                        '()))
            ((< row column)
                (getDiagonalInferiorAux matrix
                                        (car matrix) 
                                        column
                                        '0
                                        (listOfCounters (+ column 1) '0 '())
                                        '()
                                        '()))
           (else
                (getDiagonalInferiorAux matrix
                                        (car matrix) 
                                        row
                                        '0
                                        (listOfCounters (+ row 1) '0 '())
                                        '()
                                        '())
            ) 
    )
)

(define (getDiagonalInferiorAux matrix fila size cont listaContadores listaTemp listaDiagonal)
    (cond  ; Caso en donde se encuentre en la última fila de una matriz con más filas que columnas
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
                                        (cdr listaContadores)
                                        '()
                                        listaDiagonal))            
            ; Caso en donde se agregan los elementos de cada diagonal a una lista temporal                                    
            ((> cont 0)
                (append listaDiagonal
                        (list (getListaTemp fila 
                                            cont 
                                            listaContadores 
                                            listaTemp)) 
                        (getDiagonalInferiorAux (cdr matrix) 
                                                (cadr matrix) 
                                                (- size 1) 
                                                (cadr listaContadores) 
                                                (cdr listaContadores)
                                                '()
                                                listaDiagonal))
            )                                             
    )
)

(define (getListaTemp fila cont listaContadores listaTemp)
    (cond ((not(= cont 0))
            (append listaTemp 
                    (list (car fila))
                    (getListaTemp (cdr fila)  
                                    (- cont 1) 
                                    listaContadores
                                    listaTemp ))
            )
           ((= cont 0)
                listaTemp)
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

(define (rellenarMatriz matrix)
    (cond ((null? matrix)
                matrix
            )
           ; Caso en donde se tienen más filas que columnas    
           ((< (length (car matrix)) (length matrix))
                (rellenarMatrizAux matrix 
                                   (createMatrix (length matrix) (length matrix))
                                   '()
                                   '0
                                   (listOfCounters (- (length matrix) 1) '0 '()))
            )
            ; Caso en donde se tiene más columnas que filas
            ((> (length (car matrix)) (length matrix))
                (rellenarMatrizAux matrix 
                                   (createMatrix (length (car matrix)) (length (car matrix)))
                                   '()
                                   '0
                                   (listOfCounters (- (length (car matrix)) 1) '0 '()))
            ) 
            (else
                (rellenarMatrizAux matrix 
                                   (createMatrix (length matrix) (length matrix))
                                   '()
                                   '0
                                   (listOfCounters (- (length matrix) 1) '0 '()))
            )                          
    )
)


(define (rellenarMatrizAux matrixInferior matrixCuadrada matrixNueva cont listaCont)
    (cond ; Caso en donde ya no hay elementos a procesar 
           ((null? matrixInferior)
                matrixNueva
           )
           ; Caso en donde se encuentra la última fila
           ((null? (cdr matrixInferior))
      	                (append matrixNueva
                        (list (pegarListas (invertirLista (car matrixInferior))
                                                          (car matrixCuadrada)
                                                          '()
                                                          (length (car matrixInferior))
                                                          (length (car matrixCuadrada))
                                                          (+ (car listaCont) 1)
                                                          '0)))
           )
           ; Caso en donde hay dos filas con el mismo tamaño
           ((and (= (length (car matrixInferior)) (length (cadr matrixInferior))) (= cont 0))
                (append matrixNueva
                        (list (pegarListas (invertirLista (car matrixInferior))
                                                          (car matrixCuadrada)
                                                          '()
                                                          (length (car matrixInferior))
                                                          (length (car matrixCuadrada))
                                                          '0
                                                          '0))
                        (rellenarMatrizAux (cdr matrixInferior)
                                           (cdr matrixCuadrada)
                                           matrixNueva
                                           '1
                                           (cdr listaCont)))
           ) 
           ; Caso en donde hay dos filas con el mismo tamaño después de encontra la primera
           ((and (= (length (car matrixInferior)) (length (cadr matrixInferior))) (= cont 1))
                (append matrixNueva
                        (list (pegarListas (invertirLista (car matrixInferior))
                                                          (car matrixCuadrada)
                                                          '()
                                                          (length (car matrixInferior))
                                                          (length (car matrixCuadrada))
                                                          (+ (car listaCont) 1)
                                                          '0))
                        (rellenarMatrizAux (cdr matrixInferior)
                                           (cdr matrixCuadrada)
                                           matrixNueva
                                           '1
                                           (cdr listaCont)))
           )  
           (else
                (append matrixNueva
                        (list (pegarListas (invertirLista (car matrixInferior))
                                                          (car matrixCuadrada)
                                                          '()
                                                          (length (car matrixInferior))
                                                          (length (car matrixCuadrada))
                                                          '0
                                                          '0))
                        (rellenarMatrizAux (cdr matrixInferior)
                                           (cdr matrixCuadrada)
                                           matrixNueva
                                           '0
                                           listaCont))
            )          
    )
)


(define (pegarListas lista1 lista2 nuevaLista largoLista1 largoLista2 cont largoTotal)
    (cond   ((and (null? lista1) (not (= largoLista2 largoTotal)))
                (append nuevaLista
                        (list (car lista2))
                        (pegarListas lista1 (cdr lista2) nuevaLista largoLista1 largoLista2 '0 (+ largoTotal 1)))
            )
            ((= largoLista2 largoTotal)
                (append nuevaLista
                        '())
            )
            ; Caso en donde se tienen filas por debajo de la diagonal
            ((= cont 0)
                (append nuevaLista
                        (list (car lista1))
                        (pegarListas (cdr lista1) (cdr lista2) nuevaLista largoLista1 largoLista2 '0 (+ largoTotal 1)))
            )
           ; Caso en donde se tienen filas del mismo tamaño 
           ((= cont 1)
               (append nuevaLista
                        (list (car lista1))
                        (pegarListas (cdr lista1) lista2 nuevaLista largoLista1 largoLista2 '0 (+ largoTotal 1)))
           )
           ; Caso en donde se debe rellenar con ceros           
           ((and (> cont 0) (not(equal? cont 1)))
                (append nuevaLista
                        (list (car lista2))
                        (pegarListas lista1 (cdr lista2) nuevaLista largoLista1 largoLista2 (- cont 1) (+ largoTotal 1)))
            )            
    )
)


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#



(rellenarMatriz (getDiagonalInferior '( (1 0 0 0 0)
                                        (2 1 0 0 0)
                                        (3 2 1 0 0)
                                        (4 3 2 3 0)
                                        (5 4 3 2 1)
                                        (6 5 4 3 2)
                                        (7 6 5 4 3)
                                        (8 7 6 5 4)) '7 '4))




#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#

