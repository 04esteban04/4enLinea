#lang racket

(require "CheckWin-Lose.rkt")
;(require racket/includ(require "CheckWin-Lose.rkt")

;Conjunto de candidatos: posiciones 0, 1 ,2 en una matriz

(define (remplace element newElement lista)
    (remplaceAux element newElement lista '())
)

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

(define (remplaceMatrix element newElement matrix)
    (remplaceMatrixAux element newElement matrix '())
)

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

(define (turnMatrix matrix)
    (turnMatrixAux matrix '())
)

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

;funcion que cuenta las repeticiones de un elemento en una lista
(define (contList element lista)
    (contListAux element lista 0)
)

;funcion auxiliar de contlist que realiza toda la recursion
(define (contListAux element lista res)
    (cond
        ((null? lista) res)

        ((equal? (car lista) element)
            (contListAux element (cdr lista) (+ res 1))
        )
        (else (contListAux element (cdr lista) res))
    )
)

;funcion que determina si existe algun cero posible en una lista para insertar ficha
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

;funcion auxiliar de maxRere que realiza toda la recursion
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

;funcion que retorta el maximo de repeticiones consecutivas en una lista
(define (maxRepe element lista)
    (maxRepeAux element lista 0 0)
)

;funcion auxiliar de findPlace que realiza toda la recursividad
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

;funcion que encuentra la posicion para colocar la ficha en la mejor posicion
(define (findPlace element lista)
    (findPlaceAux element lista lista 0 0)
)

;funcion que determina las posiciones de los campos con 0
(define (availableField lista)
    (availableFieldAux lista '() 0)
)

;funcion auxiliar de availableField
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

;Funcion que obtiene el elemento en la pisicion determinada
(define (getElement pos lista)
    (cond
        ((zero? pos)
            (car lista)
        )
        (else (getElement (- pos 1) (cdr lista)))
    )

)

;Funcion auxiliar de put
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

;Funcion que inserta/remplaza un elemento en una lista
(define (put pos element lista)
    (putAux pos element lista '())
)

;Funcion que determina el mejor espacio para colocar la siguiente ficha en una lista
(define (selectPlace element lista altElement)
    (cond
        ((canAdd? lista)
            (cond
                ;caso cuando inserta fuera de la lista a la izquierda
                ((< (findPlace element lista) 0)
                    (put (car (availableField lista)) (+ element altElement) lista) 
                )
                ;Cuando no hay campo disponible en el mayor numero de repeticiones
                ((not (zero? (getElement (findPlace element lista) lista)))
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

#|
;Funcion que determina la fila para insertar la ficha en la matriz
(define (findRow element matrix)
    (findRowAux element matrix '())
)

(define (findRowAux element matrix newMatrix)
    (cond
        ((null? matrix)
            #f
        )
        ((equal? (selectPlace element (car matrix)) #f)
            (findRowAux element (cdr matrix) (append newMatrix (list (car matrix))))
        )
        (else
            (append newMatrix (list (selectPlace element (car matrix))) (cdr matrix))
        )
    
    )

)
|#

; Función que ejecuta el algoritmo codicioso
; E: matriz
; S: matriz con el elemento insertado en la mejor posición encontrada por el algoritmo
(define (greedyAlgorithm matrix)
    (findBestSolution matrix)
)

; Función que busca un solución temporal al algoritmo
; E: elemento a buscar, matriz
; S: matriz con una solución parcial
(define (findTemporal element matrix)
    (findTemporalAux element matrix 0 '() '())
)

;tempList primer fila de la matriz
; Función auxiliar de findTemporalAux
; E: elemento a buscar, matriz, número de repeticiones, matriz vacía, lista vacía
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

