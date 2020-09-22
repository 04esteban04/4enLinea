#lang racket
(require racket/gui)
;(require racket/include)
(require "Back/Matriz.rkt")


;    /################################################\
;   ||                PRIMERA VENTANA                 ||
;    \################################################/

;FRAME DEL JUEGO
(define gameWindow (new frame% 
                [label "4 en Linea"]
                [width 800]
                [height 600]
                [style (list 'no-resize-border)]))

;FRAME PRINCIPAL
(define topWindow (new frame% 
                [label "4 en Linea"]
                [width 350]
                [height 310]
                [style (list 'no-resize-border)]))
                (send topWindow show #t)


(define(drawTitle canvas dc)
  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
  (send dc draw-text "4 EN LINEA" 100 10))

(define title(new canvas% [parent topWindow]
                           [paint-callback drawTitle]
                           [style (list 'transparent)]))

;PANEL PARA CONFIGURACIONES
(define controlPane (new vertical-pane% [parent topWindow]
                                        [spacing 25]
                                        [alignment '(center top)]))
;CONFIGURACIONES PARA LA MATRIZ
(new message% [parent controlPane]
              [label "Configuraciones: "])

(define sizePane (new horizontal-pane% [parent controlPane]
                                       [spacing 5]
                                       [alignment '(center top)]))
                           

(define selectRow(new choice% [parent sizePane]
                              [label "Filas:  "]
                              [choices '("8" "9" "10" "11" "12" "13" "14" "15" "16")]
                              ))

(define selectColumn(new choice% [parent sizePane]
                                 [label "Columnas:  "]
                                 [choices '("8" "9" "10" "11" "12" "13" "14" "15" "16")]
                                 ))

(define selectToken(new choice% [parent controlPane]
                                [label "Ficha:  "]
                                [choices '("Rojo" "Azul")]
                                ))

;Boton encargado de guardar las configuraciones y comenzar el juego
(define accept(new button% [parent controlPane]
                           [label "Aceptar"]
                           [callback (lambda (button event)
                           (createMatrix 
                           (string->number (send selectRow get-string-selection)) 
                           (string->number (send selectColumn get-string-selection)))
                           (send gameWindow show #t)
                           (send topWindow show #f))]))


;    /################################################\
;   ||                 SEGUNDA VENTANA                ||
;    \################################################/

;En esta parte el juego empieza segun las configuraciones con una nueva ventana

;DISEÑO
(new canvas% [parent gameWindow]
             [paint-callback
             (lambda (canvas dc)
             (send dc draw-bitmap(make-object bitmap% "3enlinea/4enLinea.png") 10 1))]
)

(define matrixPane(new pane% [parent gameWindow]
                             [border 10]
                             [alignment '(center center)]))