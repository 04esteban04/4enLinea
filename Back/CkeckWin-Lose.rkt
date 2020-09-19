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

(define (checkHorizontal matrix size)

    (cond 
        ((zero? size)
            (checkList (caar matrix) (car matrix))
        )
        (else
            (cond
                ((zero? (checkList (caar matrix) (car matrix)))
                    (checkHorizontal (cdr matrix) (- size 1))
                )
                (else
                    (checkList (caar matrix) (car matrix))
                )
            )
        )
    )
)

(define (checkList elemento lista)
    (cond
        ((null? lista)
            elemento
        )
        ((zero? (car lista))
            0
        )
        
        ((equal? elemento (car lista))
            (checkList elemento (cdr lista))
        )
        (else
            0
        )
    )
)

(define (checkVertical matrix size)
    (checkHorizontal (columnToRow matrix (- (length matrix) 1) 0 '()) (- (length matrix) 1) )
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