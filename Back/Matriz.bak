#lang racket

;Crea una matriz cuadrada con el parametro introducido
;e: numero entero
;s: matriz nxn
(provide createMatrix)
(define (createMatrix size column)
    (cond
        ((and (< 8 size) (< 8 column) (> 16 size) (> 16 column)) (list '()))
        (else (cmAux size column 0 '())))
)

;Funcion auxiliar de createMatrix que de forma recursiva crea las columnas con el parametro introducido
;E: numero entero
;S: matriz nxn
(define (cmAux size column cont matrix)
    (cond 
        ((= column cont) matrix)
        (else
            (cmAux size column (+ cont 1) (append matrix (list (createList size '()))) )
        ))
)

;Crea una lista de tamano introducido como parametro
;E: numero entero
;S: lista de largo n
(define (createList size lista)
    (cond
        ((zero? size)
            lista
        )
        (else
        (createList (- size 1) (append lista (list 0))) ))
)

;Funcion principal para reemplazar un valor de una matriz
;E: value, row, column : como valores enteros. Matriz es una lista que contiene una matriz
;S: matriz
(define (remplaceValue value row column matriz)

    (remplaceValueAux value row column matriz '())
)

;Funcion auxiliar que realiza la recursion para reemplazar un valor en una matriz
;E: numeros enteros y una matriz
;S: matriz
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
;E: numeros enteros y una lista
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

(createMatrix 8 16)