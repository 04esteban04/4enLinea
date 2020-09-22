#lang racket

;(require racket/include)
(require "Matriz.rkt")

;Conjunto de candidatos: posiciones 0, 1 ,2 en una matriz

;Funciones para encontrar siguiente lugar en horizontal

(define (contList element lista)
    (contListAux element lista 0)
)

(define (contListAux element lista res)
    (cond
        ((null? lista) res)

        ((equal? (car lista) element)
            (contListAux element (cdr lista) (+ res 1))
        )
        (else (contListAux element (cdr lista) res))
    )
)

(define (sustitution element newElement lista res)
    (cond
        ((null? lista)
            res
        )
        ((equal? (car lista) element)
            (sustitution element newElement (cdr lista) (append res (list newElement)))
        )
        (else
        (sustitution element newElement (cdr lista) (append res (list (car lista)))))
    
    )

)

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

(define (maxRepe element lista)
    (maxRepeAux element lista 0 0)
)

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

(define (findPlace element lista)
    (findPlaceAux element lista lista 0 0)
)

(define (availableField? lista)
    (cond
        ((null? lista)
            #f
        )
        ((zero? (car lista))
            #t
        )
        (else
        (availableField (cdr lista)))
    
    )

)

(define (availableField lista)
    (availableFieldAux lista '() 0)
)

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

(define (getElement pos lista)
    (cond
        ((zero? pos)
            (car lista)
        )
        (else (getElement (- pos 1) (cdr lista)))
    )

)

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

(define (put pos element lista)
    (putAux pos element lista '())
)

(define (selectPlace element lista)
    (cond
        ((canAdd? lista)
            (cond
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

;---------------------------------------------------------------------------