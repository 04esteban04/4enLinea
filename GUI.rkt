#lang racket
(require racket/gui)
;(require racket/include)
(require "Back/Matriz.rkt")


;    /################################################\
;   ||                PRIMERA VENTANA                 ||
;    \################################################/

;Frame principal
(define topWindow (new frame% 
                [label "4 en Linea"]
                [width 350]
                [height 350]
                [style (list 'no-resize-border)]))

(define(drawTitle canvas dc)
  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
  (send dc draw-text "4 EN LINEA" 100 10))

;Titulo
(define title(new canvas% [parent topWindow]
                           [paint-callback
                           (lambda (canvas dc)
                           (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                           (send dc set-text-foreground "orange")
                           (send dc draw-text "4 EN LINEA!" 100 10))]
                           [style (list 'transparent)]))

;Panel para configuraciones
(define controlPane (new vertical-pane% [parent topWindow]
                                           [spacing 25]))
                           
  

;CONFIGURACIONES PARA LA MATRIZ

;Crea un hash para identificar el tamaño que sera introducido a la funcion
;createMatrix 
(define matrixSize (hash "8x8" 8
                    "9x9" 9
                    "10x10" 10
                    "11x11" 11
                    "12x12" 12
                    "13x13" 13
                    "14x14" 14
                    "15x15" 15 
                    "16x16" 16))

;Envia el tamaño a la funcion createMatrix
(define (set-size choice event)
  (createMatrix (hash-ref matrixSize (send choice get-string-selection))))

(define selectSize(new choice% [parent controlPane]
                               [label "Tamaño del tablero:  "]
                               [choices '("8x8" "9x9" "10x10" "11x11" "12x12" "13x13" "14x14" "15x15" "16x16")]
                               [callback set-size]))


(define accept(new button% [parent controlPane]
                           [label "Aceptar"]))







  (send topWindow show #t)