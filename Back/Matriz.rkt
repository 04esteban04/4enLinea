#lang racket

; Función que crea una matriz del tamaño deseado
; E: dos numeros enteros positivos con la cantidad de filas y columnas deseadas
; S: matriz mxn
(provide createMatrix)
(define (createMatrix row column)
    (cmAux row column 0 '())
    (print row)
    (print column)
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
                   (append matrix (list (createList column '()))))
        )
    )
)

; Función que crea una lista del tamano deseado
; E: numero entero positivo (tamaño de la lista deseada)
; S: lista de largo n
(define (createList size lista)
    (cond
        ((zero? size)
            lista)
        (else
            (createList (- size 1) (append lista (list 0))) ))
)

; Función principal para reemplazar un valor de una matriz
; E: valor a cambiar, fila y columna  como valores enteros. Matriz es una lista que contiene una matriz
; S: matriz
(define (remplaceValue value row column matriz)

    (remplaceValueAux value row column matriz '())
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


;(createMatrix 8 8)
