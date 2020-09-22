#lang racket
(require racket/gui)
;(require racket/include)
(require "Back/Matriz.rkt")


;    /################################################\
;   ||                PRIMERA VENTANA                 ||
;    \################################################/

;FRAME PRINCIPAL
(define topWindow (new frame% 
                [label "4 en Linea"]
                [width 350]
                [height 350]
                [style (list 'no-resize-border)]))

(define(drawTitle canvas dc)
  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
  (send dc draw-text "4 EN LINEA" 100 10))

(define title(new canvas% [parent topWindow]
                           [paint-callback
                           (lambda (canvas dc)
                           (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                           (send dc set-text-foreground "orange")
                           (send dc draw-text "4 EN LINEA!" 100 10))]
                           [style (list 'transparent)]))

;PANEL PARA CONFIGURACIONES
(define controlPane (new vertical-pane% [parent topWindow]
                                           [spacing 25]))
                           
  

;CONFIGURACIONES PARA LA MATRIZ
(define msg (new message% [parent controlPane]
                          [label "Tamaño del tablero: "]))

(define selectRow(new choice% [parent controlPane]
                               [label "Filas:  "]
                               [choices '("8" "9" "10" "11" "12" "13" "14" "15" "16")]
                               ))
(define selectColumn(new choice% [parent controlPane]
                               [label "Columnas:  "]
                               [choices '("8" "9" "10" "11" "12" "13" "14" "15" "16")]
                               ))

;Boton encargado de enviar el tamaño de la matriz a la funcion respectiva
(define accept(new button% [parent controlPane]
                           [label "Aceptar"]
                           [callback (lambda (button event)
                           (createMatrix (string->number (send selectRow get-string-selection)) (string->number (send selectColumn get-string-selection))))]))

(send topWindow show #t)