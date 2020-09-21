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