#lang racket

; Función que crea una matriz del tamaño deseado
; e: numero entero
; s: matriz mxn
(provide createMatrix)
(define (createMatrix row column)
    (cmAux row column 0 '())
)

;Funcion auxiliar de createMatrix que de forma recursiva crea las columnas con el parametro introducido
;E: numero entero
;S: matriz nxn
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

;Crea una lista de tamano introducido como parametro
;E: numero entero
;S: lista de largo n
(define (createList column lista)
    (cond
        ((zero? column)
            lista)
        (else
            (createList (- column 1) (append lista (list 0))) ))
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


(createMatrix 15 8)
(createMatrix 9 15)
