#lang racket

;(require racket/include)
(require "Matriz.rkt")

;Conjunto de candidatos: posiciones 0, 1 ,2 en una matriz

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
(define (selectPlace element lista)
    (cond
        ((canAdd? lista)
            (cond
                ((< (findPlace element lista) 0)
                    (put (car (availableField lista)) element lista) 
                )
                ((not (zero? (getElement (findPlace element lista) lista)))
                    (put (car (availableField lista)) element lista) 
                )
                (else
                    (put (findPlace element lista) element lista)
                )
            )
        )
        (else
            #f
        )
    )
)

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

(findRow 2 '((2 1 1 1 2 1) 
            (2 2 2 2 1 2) 
            (0 0 1 1 0 0) 
            (2 1 1 1 1 2)))

;------------------------------------------------------
;------------------------------------------------------
;------------------------------------------------------
;---------------------------------------------------------------------------
