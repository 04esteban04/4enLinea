#lang racket

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


(define (4inLine matrix size temp cont)
    (display '" ")
)


;size = numero de filas (comenzando en 0)
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

;size = numero de columnas (comenzando en 0)
(define (checkVertical matrix size)
    (checkHorizontal (columnToRow matrix (- (length (car matrix)) 1) 0 '()) (- (length (car matrix)) 1) )
)

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

(define (getRow matrix pos)
    (cond
        ((zero? pos)
            (car matrix)
        )
        (else
            (getRow (cdr matrix) (- pos 1)))
    )
)

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

(define (checkDiagonal matrix size)
    (cond ((and (zero? (- (length matrix) 1)) (zero? size))
                matrix)
           (else
                (checkDiagonalAux matrix 
                                    size
                                    (listOfCounters (- (length (car matrix)) 1) '0 '())
                                    )
           ) 
    )
)

(define (checkDiagonalAux matrix size listaContadores)
    (cond ((zero? size)
                '0)
    
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

#|
(getDiagonal '((1 0 0 0 0 0 0 0 0 0 0)
               (0 2 0 0 0 0 0 0 0 0 0)
               (0 0 5 0 0 0 0 0 0 0 0)
               (0 0 0 1 0 0 0 0 0 0 0) 
               (0 0 0 0 2 0 0 0 0 0 0) 
               (0 0 0 0 0 3 0 0 0 0 0) 
               (0 0 0 0 0 0 5 0 0 0 0) 
               (0 0 0 0 0 0 0 3 0 0 0)
               (0 0 0 0 0 0 0 0 1 0 0)) '8 '10)

;9x11
(display '"\n")

(getDiagonal '((1 0 0 0 0 0 0 0 0 0)
               (0 2 0 0 0 0 0 0 0 0)
               (0 0 5 0 0 0 0 0 0 0)
               (0 0 0 1 0 0 0 0 0 0) 
               (0 0 0 0 2 0 0 0 0 0) 
               (0 0 0 0 0 3 0 0 0 0) 
               (0 0 0 0 0 0 5 0 0 0) 
               (0 0 0 0 0 0 0 3 0 0)
               (0 0 0 0 0 0 0 0 1 0)
               (0 0 0 0 0 0 0 0 0 2)) '9 '9)

;10x10
|#
(display '"\n")

(getDiagonal '((1 0 0 0 0 0 0 0)
               (0 2 0 0 0 0 0 0)
               (0 0 5 0 0 0 0 0)
               (0 0 0 1 0 0 0 0) 
               (0 0 0 0 2 0 0 0) 
               (0 0 0 0 0 3 0 0) 
               (0 0 0 0 0 0 5 0) 
               (0 0 0 0 0 0 0 3)
               (0 0 0 0 0 0 0 0)) '8 '7)

;9x8
