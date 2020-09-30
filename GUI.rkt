#lang racket
(require racket/gui)
(require "Back/CheckWin-Lose.rkt")
(require embedded-gui)

#|
*********************************************************************************************
************************    VARIABLES REQUERIDAS POR EL JUEGO   *****************************
*********************************************************************************************
|#

; Variable que contiene la matriz del tablero
(define matrix '(()))

; Variable que contiene un boolean indicando si se realizó un cambio en la matriz
(define changeValue #f)

; Variables x,y que contienen la posición "x" y "y" del mouse
(define-values (mouseX mouseY) (values '0 '0))

; Variable que contiene la matriz temporal al realizar un cambio en la matriz original 
(define listaMatrizTemp '())


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#


#|
*********************************************************************************************
***********************    FUNCIONES REQUERIDAS PARA VALIDACIÓN   ***************************
*********************************************************************************************
|#

; Función que devuelve la fila y la columna correspondientes de la matriz seleccionadas por el usuario  
; E: matriz, posición en "x" del mouse, posición en "y" del mouse
; S: lista con la fila y columna de la matriz respectiva
(define (checkPiecePosition matrix posXmouse posYmouse)
  (checkPiecePositionAux matrix posXmouse posYmouse '0 '40 '0 '40 '0 '0 '())
)

; Función auxiliar de checkPiecePosition
; E: matriz, posición en "x" del mouse, posición en "y" del mouse, posición mínima en "x", posición máxima en "x", 
;    posición mínima en "y", posición máxima en "y", número de fila, número de columna, lista vacía    
; S: lista con la fila y columna de la matriz respectiva
(define (checkPiecePositionAux matrix posXmouse posYmouse xMin xMax yMin yMax fila columna lista)
    (cond ; Caso en donde se tiene la fila y la columna
          ((and (>= posXmouse xMin) (<= posXmouse xMax) (>= posYmouse yMin) (<= posYmouse yMax))
              ;(set! matrix (remplaceValue '1 fila columna matrix))
              (append lista (list fila) 
                            (list columna))
          )
          ; Caso en donde se tiene la fila (se busca la columna)
          ((and (>= posYmouse yMin) (<= posYmouse yMax) (not (and (>= posXmouse xMin) (<= posXmouse xMax))) )
              (checkPiecePositionAux matrix 
                                     posXmouse 
                                     posYmouse
                                     (+ xMin 40)
                                     (+ xMax 40)
                                     yMin
                                     yMax
                                     fila
                                     (+ columna 1)
                                     lista)
          )
          ; Caso en donde se tiene la columna (se busca la fila)
          ((and (>= posXmouse xMin) (<= posXmouse xMax) (not (and (>= posYmouse yMin) (<= posYmouse yMax))) )
              (checkPiecePositionAux matrix 
                                     posXmouse 
                                     posYmouse
                                     xMin
                                     xMax
                                     (+ yMin 40)
                                     (+ yMax 40)
                                     (+ fila 1)
                                     columna
                                     lista)
          )
          ; Caso en donde no se tienen ni la fila ni la columna
          (else
              (checkPiecePositionAux matrix 
                                     posXmouse 
                                     posYmouse
                                     (+ xMin 40)
                                     (+ xMax 40)
                                     (+ yMin 40)
                                     (+ yMax 40) 
                                     (+ fila 1)
                                     (+ columna 1)
                                     lista)
          )
    )

)

; Función que revisa si el jugador ganó, perdió o quedo empatado
; E: matriz
; S: número que indica si el jugador ganó (1 = ganó, 2 = perdío, 3 = empate)
(define (checkWinLoseGUI matrix)
  (cond
    ((equal? 1 (checkWinLose matrix))
        (send winnerAlert show #t)
        (exit)
    )
    ((equal? 2 (checkWinLose matrix))
        (send loserAlert show #t)
        (exit)
    )
    ((equal? 3 (checkWinLose matrix))
        (send tieAlert show #t)
        (exit)
    )
  )
)

; Función para verificar si la posición seleccionada es válida
; E: matriz, posición en "x" (fila), posición en "y" (columna)
; S: boolean que indica si la posición seleccionada por el usuario es válida
(define (checkMatriz matrix posX posY)
  (checkMatrizAux matrix (car matrix) (car(checkPiecePosition matrix posX posY)) (cadr(checkPiecePosition matrix posX posY)) '0 '0)
)

; Función auxiliar de checkMatrix
; E: matriz, primera fila de la matriz, número de fila, número de columna, 
;    número de fila temporal, número de columna temporal
; S: boolean que indica si la posición seleccionada por el usuario es válida
(define (checkMatrizAux matrix fila numFila numColumna filaTemp columnaTemp)
  (cond ; Se busca la fila correcta
        ((not (equal? filaTemp numFila))
            (checkMatrizAux (cdr matrix)
                            (cadr matrix)
                            numFila
                            numColumna
                            (+ filaTemp 1)
                            columnaTemp)
        )
        ; Se busca la columna correcta
        ((and (equal? filaTemp numFila) (not (equal? columnaTemp numColumna)))
            (checkMatrizAux matrix
                            (cdr fila)
                            numFila
                            numColumna
                            filaTemp
                            (+ columnaTemp 1))
        )
        ; Se tienen la fila y la columna pero ya hay ficha en esa posición
        ((and (equal? filaTemp numFila) (equal? columnaTemp numColumna) (not (equal? (car fila) '0)))
            #f
        )
        ; Se tienen la fila y la columna y no hay ficha en esa posición
        ((and (equal? filaTemp numFila) (equal? columnaTemp numColumna) (equal? (car fila) '0))
            #t
        )
  )
)

; Función solución que verifica la posición correcta de la ficha ingresada
; E: matriz
; S: matriz con la posición deseada cambiada por un 2
(define (checkCorrectSolution2 matrix)
    (cond ((null? matrix)
                '())
           (else
                (checkCorrectSolutionAux2 matrix matrix (car matrix) (cadr matrix) '0 '0 '0)
           )
    )
)
 
; Función auxiliar de checkCorrectSolution
; E: matriz, matriz, primera fila de matriz, segunda fila de matriz, número de fila, número de columna
; S: matriz con la posición deseada cambiada por un 2
(define (checkCorrectSolutionAux2 matrix matrixOriginal fila1 fila2 numFila numPos cont)
    (cond   ; Caso en donde se tiene solo una fila en la matrixOriginal
            ((null? (cdr matrixOriginal))
                (cond   ((null? fila1)
                            matrix
                        )
                        ; Caso en donde se busca en la última fila
                        ((not(equal? (car fila1) '3))
                            (checkCorrectSolutionAux2 matrix 
                                                      matrixOriginal 
                                                      (cdr fila1) 
                                                      fila2 
                                                      numFila 
                                                      (+ numPos 1)
                                                      cont)
                        )
                        (else   
                            (list (remplaceMatrix '3 '1 matrix)
                                  (list cont))
                        )
                )
            )
            ; Se realiza la búsqueda en la siguiente fila de la matriz
            ((and (null? fila1) (>= (length matrixOriginal) 3))
                (checkCorrectSolutionAux2 matrix 
                                        (cdr matrixOriginal) 
                                        (cadr matrixOriginal) 
                                        (caddr matrixOriginal) 
                                        (+ numFila 1)
                                         '0
                                         cont)
            )
            ; Se realiza la búsqueda en la siguiente fila de la matriz cuando solo se tienen 2 filas
            ((and (null? fila1) (= (length matrixOriginal) 2))
                (checkCorrectSolutionAux2 matrix 
                                        (cdr matrixOriginal) 
                                        (cadr matrixOriginal) 
                                        '() 
                                        (+ numFila 1) 
                                        '0
                                        cont)
            )
            ; Caso en donde se tienen más de 2 filas
            ((and (not(equal? (car fila1) '3)) (>= (length matrixOriginal) 2))
                (checkCorrectSolutionAux2 matrix 
                                         matrixOriginal 
                                         (cdr fila1) 
                                         (cdr fila2) 
                                         numFila 
                                         (+ numPos 1)
                                         cont)
            )
            ; Caso en donde se encuentra la posición deseada
            ((and (equal? (car fila1) '3) (equal? (car fila2) '0))
                (checkCorrectSolutionAux2 (changeTempValue2 matrix numFila numPos '0 '() #f)
                                         (changeTempValue2 matrix numFila numPos '0 '() #f)
                                         (car (changeTempValue2 matrix numFila numPos '0 '() #f))
                                         (cadr (changeTempValue2 matrix numFila numPos '0 '() #f))
                                         '0
                                         '0
                                         (+ cont 40))
            )
            (else
                (list (remplaceMatrix '3 '1 matrix)
                        (list cont))
            )
    
    )
)

; Función que cambia un valor temporal en una fila de la matriz dada
; E: matriz, posición de fila, posición de columna, contador, matriz vacía, boolean que indica si ya se cambió el elemento de la matriz 
; S: matriz con el elemento buscado cambiado de posición
(define (changeTempValue2 matrix rowPos columnPos cont newMatrix bool)
    (cond   ((null? matrix)
                newMatrix
            )
            ; Caso en donde se debe cambiar el 3
            ((equal? cont rowPos)
                (append newMatrix
                        (list (put columnPos '0 (car matrix)))
                        (changeTempValue2 (cdr matrix)
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
                        (changeTempValue2 (cdr matrix)
                                          rowPos
                                          columnPos
                                          (+ cont 1)
                                          newMatrix
                                          #f))
            )
            (else
                (append newMatrix
                        (list (car matrix))
                        (changeTempValue2 (cdr matrix)
                                          rowPos
                                          columnPos
                                          (+ cont 1)
                                          newMatrix
                                          #f))
            )
    )
)

; Función que reemplaza un elemento por otro dado en un lista específica
; E: elemento a reemplazar, elemento deseado, lista
; S: lista con el elemento deseado en la posición del elemento a reemplazar
(define (remplace element newElement lista)
    (remplaceAux element newElement lista '())
)

; Función auxiliar de remplace
; E: elemento a reemplazar, elemento deseado, lista
; S: lista con el elemento deseado en la posición del elemento a reemplazar
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

; Función para reemplazar un elemento en una matriz por el elemento deseado 
; E: elemento a reemplazar, elemento deseado, matriz
; S: matriz con el elemento a cambiar sustituido por el elemento deseado
(define (remplaceMatrix element newElement matrix)
    (remplaceMatrixAux element newElement matrix '())
)

; Función auxiliar de remplaceMatrixAux 
; E: elemento a reemplazar, elemento deseado, matriz, matriz vacía
; S: matriz con el elemento a cambiar sustituido por el elemento deseado
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

; Función que inserta/remplaza un elemento en una lista
; E: posición en la cual se insertará el elemento, elemento, lista
; S: lista con el elemento insertado en la posición indicada
(define (put pos element lista)
    (putAux pos element lista '())
)

; Función auxiliar de put
; E: posición en la cual se insertará el elemento, elemento, lista, lista vacía
; S: lista con el elemento insertado en la posición indicada
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


; Función que realiza el cambio de valor en la matriz y actualiza el valor en la interfaz
; E: evento del mouse
; S: actualiza la matriz en caso de un cambio exitoso y sino muestra una ventana de alerta
(define (position-piece event)
  (define-values (x y) (values (send event get-x) (+ 20 (send event get-y))))
  (cond ;Se verifica si la posición indicada es válida 
        ((equal? (checkMatriz matrix x y) #t)
            (set! matrix (remplaceValue '3 (car (checkPiecePosition matrix x y)) (cadr (checkPiecePosition matrix x y)) matrix))
            ; Se verifica si se debe bajar la posición de la ficha
            (set! listaMatrizTemp (checkCorrectSolution2 matrix))
            
            (set! matrix (car listaMatrizTemp))
            (set! mouseY (cadr listaMatrizTemp))

            (set! changeValue #t)
        ) 
        (else
            (send positionWarning show #t)
        )
  )
)


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#


#|
*********************************************************************************************
***********************    CONFIGURACIONES DE INTERFAZ GRÁFICA   ****************************
*********************************************************************************************
|#

; Frame principal del juego
(define gameWindow (new frame% 
                [label "4 en Linea"]
                [width 640]
                [height 640]
                [style (list 'no-resize-border)])
)

; Frame para ventana de alerta que indica que el jugador ganó
(define winnerAlert (new dialog% [label "Alerta"]
                 [parent gameWindow]
                 [width 300]
                 [height 350])
)

; Frame para ventana de alerta que indica que el jugador perdió
(define loserAlert (new dialog% [label "Alerta"]
                 [parent gameWindow]
                 [width 300]
                 [height 350])
)

; Frame para ventana de alerta que indica que el jugador empató
(define tieAlert (new dialog% [label "Alerta"]
                 [parent gameWindow]
                 [width 300]
                 [height 350])
)

; Frame para ventana de alerta que indica que el jugador puso una ficha en una posición inválida
(define positionWarning (new dialog% [label "Alerta"]
                 [parent gameWindow]
                 [width 900]
                 [height 350])
)

; Canvas del frame winnerAlert
(define winnerCanvas (new canvas% [parent winnerAlert]
                                  [paint-callback 
                                  (lambda (canvas dc)
                                  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                                  (send dc draw-text "GANASTE!" 10 10)
                                  (let ((winnerPicture (make-object bitmap% "3enlinea/4enLinea.png")))
                                  (send dc draw-bitmap winnerPicture 30 50)))])
)

; Canvas del frame loserAlert
(define loserCanvas (new canvas% [parent loserAlert]
                                  [paint-callback 
                                  (lambda (canvas dc)
                                  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                                  (send dc draw-text "PERDISTE!" 10 10)
                                  (let ((winnerPicture (make-object bitmap% "3enlinea/4enLinea.png")))
                                  (send dc draw-bitmap winnerPicture 30 50)))])
)

; Canvas del frame tieAlert
(define tieCanvas (new canvas% [parent tieAlert]
                                  [paint-callback 
                                  (lambda (canvas dc)
                                  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                                  (send dc draw-text "EMPATE!" 10 10)
                                  (let ((winnerPicture (make-object bitmap% "3enlinea/4enLinea.png")))
                                  (send dc draw-bitmap winnerPicture 30 50)))])
)

; Canvas del frame positionWarning
(define positionWarningCanvas (new canvas% [parent positionWarning]
                                  [paint-callback 
                                  (lambda (canvas dc)
                                  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                                  (send dc draw-text "No se puede insertar la ficha en el lugar indicado!" 10 10)
                                  (let ((winnerPicture (make-object bitmap% "3enlinea/4enLinea.png")))
                                  (send dc draw-bitmap winnerPicture 30 50)))])
)

; Frame de configuraciones de la ventana principal
(define topWindow (new frame% 
                [label "4 en Linea"]
                [width 350]
                [height 310]
                [style (list 'no-resize-border)])
)
(send topWindow show #t)

; Frame de configuración para el título
(define(drawTitle canvas dc)
  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
  (send dc draw-text "4 EN LINEA" 100 10)
)

; Configuración del título
(define title(new canvas% [parent topWindow]
                           [paint-callback drawTitle]
                           [style (list 'transparent)])
)

; Panel de configuraciones
(define controlPane (new vertical-pane% [parent topWindow]
                                        [spacing 25]
                                        [alignment '(center top)])
)

; Configuraciones de la matriz
(new message% [parent controlPane]
              [label "Configuraciones: "]
)

; Configuraciones del panel 
(define sizePane (new horizontal-pane% [parent controlPane]
                                       [spacing 5]
                                       [alignment '(center top)])
)

; Configuración de seleción de filas                           
(define selectRow(new choice% [parent sizePane]
                              [label "Filas:  "]
                              [choices '("8" "9" "10" "11" "12" "13" "14" "15" "16")])
)

; Configuración de selección de columnas
(define selectColumn(new choice% [parent sizePane]
                                 [label "Columnas:  "]
                                 [choices '("8" "9" "10" "11" "12" "13" "14" "15" "16")])
)

; Configuración de selección de ficha
(define selectToken(new choice% [parent controlPane]
                                [label "Ficha:  "]
                                [choices '("Rojo" "Azul")])
)

; Botón encargado de guardar las configuraciones y comenzar el juego
(define accept(new button% [parent controlPane]
                           [label "Aceptar"]
                           [callback (lambda (button event)
                           (define matrixRows (string->number (send selectRow get-string-selection)))
                           (define matrixCols (string->number (send selectColumn get-string-selection)))
                           (set! matrix (createMatrix matrixRows matrixCols))
                           (send gameWindow show #t)
                           (send topWindow show #f))])
)

; Variable para almacenar el token escogido por el usuario
(define token (send selectToken get-string-selection))

#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#


#|
*********************************************************************************************
*********************    CONFIGURACIONES DEL TABLERO Y FICHAS    ****************************
*********************************************************************************************
|#


; Override de la clase pasteboard%, de manera que sea posible dibujar el tablero 
; con la funcion draw-4Line-board y pintar las fichas
(define 4Line-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-4Line-board dc)))
    
    ;Button mouseclick izquierdo donde empieza el game
    (define/override (on-event e)
      (when (eq? (send e get-event-type) 'left-down)
        
        (position-piece e)
        
        (for ([id (in-hash-keys token-piece-data)])
          (define piece (make-token-piece id))

        ; Se pinta del color del token del jugador en caso de que haya un cambio válido
        (when (equal? changeValue #t)
          (send board insert piece (- (send e get-x) 15) (+ (- (send e get-y) 15) (- (car mouseY) 10)))
          (checkWinLoseGUI matrix)
          (set! changeValue #f)
        )

        (sleep/yield 1)
        
        ; Se ejecuta el algoritmo codicioso
        (set! matrix (greedyAlgorithm matrix))

        
        ;###########################################################################
        ;############### POSICIONES PARA LA FICHA DE LA MAQUINA ####################

        ;1 segundo por turno
        (sleep/yield 1)
        (define rows (length matrix))
        (define columns (length (list-ref matrix 0)))

        ;###############PRIMERA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 0) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 0))
        
        (when (equal? 2 (list-ref(list-ref matrix 0) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 0))
        
        (when (equal? 2 (list-ref(list-ref matrix 0) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 0))
        
        (when (equal? 2 (list-ref(list-ref matrix 0) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 0))
        
        (when (equal? 2 (list-ref(list-ref matrix 0) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 0))
        
        (when (equal? 2 (list-ref(list-ref matrix 0) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 0))

        (when (equal? 2 (list-ref(list-ref matrix 0) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 0))

        (when (equal? 2 (list-ref(list-ref matrix 0) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 0))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 0) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 0)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 0) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 0)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 0) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 0)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 0) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 0)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 0) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 0)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 0) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 0)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 0) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 0)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 0)))

        ;###############SEGUNDA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 1) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 35))
        
        (when (equal? 2 (list-ref(list-ref matrix 1) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 35))
        
        (when (equal? 2 (list-ref(list-ref matrix 1) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 35))
        
        (when (equal? 2 (list-ref(list-ref matrix 1) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 35))
        
        (when (equal? 2 (list-ref(list-ref matrix 1) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 35))
        
        (when (equal? 2 (list-ref(list-ref matrix 1) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 35))

        (when (equal? 2 (list-ref(list-ref matrix 1) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 35))

        (when (equal? 2 (list-ref(list-ref matrix 1) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 35))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 35)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 35)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 35)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 35)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 35)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 35)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 35)))
          
        (when (= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 1) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 35)))

        
        ;###############TERCERA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 2) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 70))
        
        (when (equal? 2 (list-ref(list-ref matrix 2) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 70))
        
        (when (equal? 2 (list-ref(list-ref matrix 2) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 70))
        
        (when (equal? 2 (list-ref(list-ref matrix 2) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 70))
        
        (when (equal? 2 (list-ref(list-ref matrix 2) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 70))
        
        (when (equal? 2 (list-ref(list-ref matrix 2) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 70))

        (when (equal? 2 (list-ref(list-ref matrix 2) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 70))

        (when (equal? 2 (list-ref(list-ref matrix 2) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 70))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 70)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 70)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 70)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 70)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 70)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 70)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 70)))
          
        (when (= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 2) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 70)))

        ;###############CUARTA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 3) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 110))
        
        (when (equal? 2 (list-ref(list-ref matrix 3) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 110))
        
        (when (equal? 2 (list-ref(list-ref matrix 3) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 110))
        
        (when (equal? 2 (list-ref(list-ref matrix 3) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 110))
        
        (when (equal? 2 (list-ref(list-ref matrix 3) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 110))
        
        (when (equal? 2 (list-ref(list-ref matrix 3) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 110))

        (when (equal? 2 (list-ref(list-ref matrix 3) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 110))

        (when (equal? 2 (list-ref(list-ref matrix 3) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 110))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 110)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 110)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 110)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 110)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 110)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 110)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 110)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 3) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 110)))

        ;###############QUINTA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 4) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 150))
        
        (when (equal? 2 (list-ref(list-ref matrix 4) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 150))
        
        (when (equal? 2 (list-ref(list-ref matrix 4) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 150))
        
        (when (equal? 2 (list-ref(list-ref matrix 4) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 150))
        
        (when (equal? 2 (list-ref(list-ref matrix 4) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 150))
        
        (when (equal? 2 (list-ref(list-ref matrix 4) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 150))

        (when (equal? 2 (list-ref(list-ref matrix 4) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 150))

        (when (equal? 2 (list-ref(list-ref matrix 4) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 150))

        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 4) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 150)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 4) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 150)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 4) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 150)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 4) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 150)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 4) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 150)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 4) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 150)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 4) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 150)))

        ;###############SEXTA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 5) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 190))
        
        (when (equal? 2 (list-ref(list-ref matrix 5) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 190))
        
        (when (equal? 2 (list-ref(list-ref matrix 5) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 190))
        
        (when (equal? 2 (list-ref(list-ref matrix 5) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 190))
        
        (when (equal? 2 (list-ref(list-ref matrix 5) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 190))
        
        (when (equal? 2 (list-ref(list-ref matrix 5) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 190))

        (when (equal? 2 (list-ref(list-ref matrix 5) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 190))

        (when (equal? 2 (list-ref(list-ref matrix 5) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 190))

        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 5) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 190)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 5) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 190)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 5) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 190)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 5) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 190)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 5) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 190)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 5) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 190)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 5) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 190)))


        ;###############SEPTIMA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 6) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 230))
        
        (when (equal? 2 (list-ref(list-ref matrix 6) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 230))
        
        (when (equal? 2 (list-ref(list-ref matrix 6) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 230))
        
        (when (equal? 2 (list-ref(list-ref matrix 6) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 230))
        
        (when (equal? 2 (list-ref(list-ref matrix 6) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 230))
        
        (when (equal? 2 (list-ref(list-ref matrix 6) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 230))

        (when (equal? 2 (list-ref(list-ref matrix 6) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 230))

        (when (equal? 2 (list-ref(list-ref matrix 6) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 230))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 230)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 230)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 230)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 230)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 230)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 230)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 230)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 6) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 230)))

        ;###############OCTAVA FILA###################
        (when (equal? 2 (list-ref(list-ref matrix 7) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 270))
        
        (when (equal? 2 (list-ref(list-ref matrix 7) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 270))
        
        (when (equal? 2 (list-ref(list-ref matrix 7) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 270))
        
        (when (equal? 2 (list-ref(list-ref matrix 7) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 270))
        
        (when (equal? 2 (list-ref(list-ref matrix 7) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 270))
        
        (when (equal? 2 (list-ref(list-ref matrix 7) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 270))

        (when (equal? 2 (list-ref(list-ref matrix 7) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 270))

        (when (equal? 2 (list-ref(list-ref matrix 7) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 270))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 270)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 270)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 270)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 270)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 270)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 270)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 270)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 7) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 270)))

        ;############### NOVENA FILA ###################
        (when (>= rows 9)
          
        (when (equal? 2 (list-ref(list-ref matrix 8) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 305))
        
        (when (equal? 2 (list-ref(list-ref matrix 8) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 305))
        
        (when (equal? 2 (list-ref(list-ref matrix 8) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 305))
        
        (when (equal? 2 (list-ref(list-ref matrix 8) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 305))
        
        (when (equal? 2 (list-ref(list-ref matrix 8) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 305))
        
        (when (equal? 2 (list-ref(list-ref matrix 8) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 305))

        (when (equal? 2 (list-ref(list-ref matrix 8) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 305))

        (when (equal? 2 (list-ref(list-ref matrix 8) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 305))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 305)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 305)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 305)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 305)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 305)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 305)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 305)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 8) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 305)))
        )  
        
        ;############### DECIMA FILA ###################
        (when (>= rows 10)
          
        (when (equal? 2 (list-ref(list-ref matrix 9) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 345))

        (when (equal? 2 (list-ref(list-ref matrix 9) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 345))

        (when (equal? 2 (list-ref(list-ref matrix 9) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 345))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 345)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 345)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 345)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 345)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 345)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 345)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 345)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 345)))
        )

        ;############### DECIMA FILA ###################
        (when (>= rows 10)
          
        (when (equal? 2 (list-ref(list-ref matrix 9) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 345))
        
        (when (equal? 2 (list-ref(list-ref matrix 9) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 345))

        (when (equal? 2 (list-ref(list-ref matrix 9) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 345))

        (when (equal? 2 (list-ref(list-ref matrix 9) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 345))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 345)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 345)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 345)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 345)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 345)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 345)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 345)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 9) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 345)))
        )

        ;############### UNDECIMA FILA ###################
        (when (>= rows 11)
          
        (when (equal? 2 (list-ref(list-ref matrix 10) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 380))
        
        (when (equal? 2 (list-ref(list-ref matrix 10) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 380))
        
        (when (equal? 2 (list-ref(list-ref matrix 10) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 380))
        
        (when (equal? 2 (list-ref(list-ref matrix 10) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 380))
        
        (when (equal? 2 (list-ref(list-ref matrix 10) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 380))
        
        (when (equal? 2 (list-ref(list-ref matrix 10) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 380))

        (when (equal? 2 (list-ref(list-ref matrix 10) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 380))

        (when (equal? 2 (list-ref(list-ref matrix 10) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 380))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 380)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 380)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 380)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 380)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 380)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 380)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 380)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 10) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 380)))
        )    

        ;############### DOCEAVA FILA ###################
        (when (>= rows 12)
          
        (when (equal? 2 (list-ref(list-ref matrix 11) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 420))
        
        (when (equal? 2 (list-ref(list-ref matrix 11) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 420))
        
        (when (equal? 2 (list-ref(list-ref matrix 11) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 420))
        
        (when (equal? 2 (list-ref(list-ref matrix 11) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 420))
        
        (when (equal? 2 (list-ref(list-ref matrix 11) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 420))
        
        (when (equal? 2 (list-ref(list-ref matrix 11) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 420))

        (when (equal? 2 (list-ref(list-ref matrix 11) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 420))

        (when (equal? 2 (list-ref(list-ref matrix 11) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 420))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 420)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 420)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 420)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 420)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 420)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 420)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 420)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 11) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 420)))
        )      
        
        ;############### TRECEAVA FILA ###################
        (when (>= rows 13)
          
        (when (equal? 2 (list-ref(list-ref matrix 12) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 455))
        
        (when (equal? 2 (list-ref(list-ref matrix 12) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 455))
        
        (when (equal? 2 (list-ref(list-ref matrix 12) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 455))
        
        (when (equal? 2 (list-ref(list-ref matrix 12) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 455))
        
        (when (equal? 2 (list-ref(list-ref matrix 12) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 455))
        
        (when (equal? 2 (list-ref(list-ref matrix 12) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 455))

        (when (equal? 2 (list-ref(list-ref matrix 12) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 455))

        (when (equal? 2 (list-ref(list-ref matrix 12) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 455))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 455)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 455)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 455)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 455)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 455)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 455)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 455)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 12) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 455)))
        )

        ;############### CATORCEAVA FILA ###################
        (when (>= rows 14)
          
        (when (equal? 2 (list-ref(list-ref matrix 13) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 495))
        
        (when (equal? 2 (list-ref(list-ref matrix 13) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 495))
        
        (when (equal? 2 (list-ref(list-ref matrix 13) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 495))
        
        (when (equal? 2 (list-ref(list-ref matrix 13) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 495))
        
        (when (equal? 2 (list-ref(list-ref matrix 13) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 495))
        
        (when (equal? 2 (list-ref(list-ref matrix 13) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 495))

        (when (equal? 2 (list-ref(list-ref matrix 13) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 495))

        (when (equal? 2 (list-ref(list-ref matrix 13) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 495))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 495)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 495)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 495)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 495)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 495)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 495)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 495)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 13) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 495)))
        )

        ;############### QUINCEAVA FILA ###################
        (when (>= rows 15)
          
        (when (equal? 2 (list-ref(list-ref matrix 14) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 535))
        
        (when (equal? 2 (list-ref(list-ref matrix 14) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 535))
        
        (when (equal? 2 (list-ref(list-ref matrix 14) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 535))
        
        (when (equal? 2 (list-ref(list-ref matrix 14) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 535))
        
        (when (equal? 2 (list-ref(list-ref matrix 14) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 535))
        
        (when (equal? 2 (list-ref(list-ref matrix 14) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 535))

        (when (equal? 2 (list-ref(list-ref matrix 14) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 535))

        (when (equal? 2 (list-ref(list-ref matrix 14) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 535))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 535)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 535)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 535)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 535)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 535)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 535)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 535)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 14) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 535)))
        )

        ;############### DECIMOSEXTA FILA ###################
        (when (>= rows 16)
          
        (when (equal? 2 (list-ref(list-ref matrix 15) 0))
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 0 570))
        
        (when (equal? 2 (list-ref(list-ref matrix 15) 1)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 40 570))
        
        (when (equal? 2 (list-ref(list-ref matrix 15) 2)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 80 570))
        
        (when (equal? 2 (list-ref(list-ref matrix 15) 3)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 120 570))
        
        (when (equal? 2 (list-ref(list-ref matrix 15) 4)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 160 570))
        
        (when (equal? 2 (list-ref(list-ref matrix 15) 5)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 200 570))

        (when (equal? 2 (list-ref(list-ref matrix 15) 6)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 240 570))

        (when (equal? 2 (list-ref(list-ref matrix 15) 7)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 280 570))

        (when (>= columns 9) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 8)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 320 570)))
        
        (when (>= columns 10) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 9)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 360 570)))

        (when (>= columns 11) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 10)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 400 570)))     
        
        (when (>= columns 12) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 11)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 440 570)))

        (when (>= columns 13) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 12)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 480 570)))

        (when (>= columns 14) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 13)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 520 570)))
        
        (when (>= columns 15) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 14)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 560 570)))
          
        (when (>= columns 16) 
          (when (equal? 2 (list-ref(list-ref matrix 15) 15)) 
            (define piece (make-tokenBOT-piece id))
            (send board insert piece 600 570)))
        )
        (checkWinLoseGUI matrix)
        ) 
      )
    )
  )
)

; Función encargada de dibujar el tablero
; E: tablero
; S: tablero con las configuraciones dadas por el usuario
(define (draw-4Line-board dc)
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
  (define pen (send the-pen-list find-or-create-pen "white" 1 'solid))
  (define font (send the-font-list find-or-create-font 8 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width 16)) ;Tamano maximo **
  (define cell-height (/ dc-height 16))
  (define margin 3)

  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)

  (for* ([row (in-range (string->number (send selectRow get-string-selection)))] [col (in-range (string->number (send selectColumn get-string-selection)))])
      (define-values [x y] (values (* col cell-width) (* row cell-height)))
      (send dc draw-ellipse x y cell-width cell-height)
  )
)

; Variable para mantener el tablero
(define board (new 4Line-board%))

; Canvas de clase editor%, encargado de contener el pasteboard correspondiente
(define boardContainer (new editor-canvas%
                       [parent gameWindow]
                       [style '(no-hscroll no-vscroll)]
                       [horizontal-inset 0]
                       [vertical-inset 0]
                       [editor board])
)

; Unicode del token de los dos jugadores
(define token-piece-data
  (hash "Token" #\u2b55)
)

; Se define la clase tipo snip del jugador, para poder pegarlo en el pasteboard
(define token-piece-snip-class
  (make-object
   (class snip-class%
      (super-new)
      (send this set-classname "token-snip")
   )
  )
)

; Se añade la clase tipo snip del jugador
(send (get-the-snip-class-list) add token-piece-snip-class)

; Se define la clase tipo snip del bot, para poder pegarlo en el pasteboard
(define tokenBOT-piece-snip-class
  (make-object
   (class snip-class%
      (super-new)
      (send this set-classname "tokenBOT-snip")
   )
  )
)

; Se añade la clase tipo snip del bot
(send (get-the-snip-class-list) add tokenBOT-piece-snip-class)

; Se define al objeto Token del jugador, de tipo class snip
(define token-piece%
  (class snip%
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass token-piece-snip-class)
    
    ; Configura el tamaño del Token
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 1.0))
      (when space (set-box! space 1.0))
      (when lspace (set-box! lspace 1.0))
      (when rspace (set-box! rspace 1.0))
    )

     ; Dibuja el token en el board según el color que eligió el jugador 
    (define/override (draw dc x y . other)
      (cond 
        ((equal? "Rojo" (send selectToken get-string-selection))
          (send dc set-font font)
          (send dc set-text-foreground "red")
          (define-values (glyph-width glyph-height baseline extra-space)
          (send dc get-text-extent glyph font #t))
          (let ((ox (/ (- size glyph-width) 2))
                (oy (/ (- size glyph-height 2))))
          (send dc draw-text glyph (+ x ox) (+ y oy)))
        )
        ((equal? "Azul" (send selectToken get-string-selection))
          (send dc set-font font)
          (send dc set-text-foreground "blue")
          (define-values (glyph-width glyph-height baseline extra-space)
          (send dc get-text-extent glyph font #t))
          (let ((ox (/ (- size glyph-width) 2))
                (oy (/ (- size glyph-height 2))))
          (send dc draw-text glyph (+ x ox) (+ y oy)))
        )
      )
    )
  )
)

; Se define al objeto Token del bot, de tipo class snip
(define tokenBOT-piece%
  (class snip%
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass tokenBOT-piece-snip-class)
    
    ; Configura el tamaño del bot Token
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 1.0))
      (when space (set-box! space 1.0))
      (when lspace (set-box! lspace 1.0))
      (when rspace (set-box! rspace 1.0))
    )

     ; Dibuja el token del bot en el board según el color que eligió el jugador 
    (define/override (draw dc x y . other)
      (cond 
        ((equal? "Rojo" (send selectToken get-string-selection))
          (send dc set-font font)
          (send dc set-text-foreground "blue")
          (define-values (glyph-width glyph-height baseline extra-space)
          (send dc get-text-extent glyph font #t))
          (let ((ox (/ (- size glyph-width) 2))
                (oy (/ (- size glyph-height 2))))
            (send dc draw-text glyph (+ x ox) (+ y oy)))
        )
        ((equal? "Azul" (send selectToken get-string-selection))
          (send dc set-font font)
          (send dc set-text-foreground "red")
          (define-values (glyph-width glyph-height baseline extra-space)
          (send dc get-text-extent glyph font #t))
          (let ((ox (/ (- size glyph-width) 2))
                (oy (/ (- size glyph-height 2))))
          
            (send dc draw-text glyph (+ x ox) (+ y oy)))
        )
      )
    )
  )
)

; Se crea el Token piece del jugador       
(define (make-token-piece id)
  (define glyph (hash-ref token-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new token-piece% [glyph (string glyph)] [font font] [size 35])
)

; Se crea el Token piece del bot  
(define (make-tokenBOT-piece id)
  (define glyph (hash-ref token-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new tokenBOT-piece% [glyph (string glyph)] [font font] [size 35])
)


#|
*********************************************************************************************
*********************************************************************************************
*********************************************************************************************
|#




