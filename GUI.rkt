#lang racket
(require racket/gui)
(require "Back/CheckWin-Lose.rkt")


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

(define matrixPanel (new panel% [parent gameWindow]
                                [style (list 'border)]
                                [alignment '(right center)]
                                [vert-margin 50]
                                [horiz-margin 50]))

(define 4Line-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-4Line-board dc)))))

(define (draw-4Line-board dc)
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'solid))
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
    (send dc draw-rectangle x y cell-width cell-height)))


(define board (new 4Line-board%))

(define canvas (new editor-canvas%
                    [parent matrixPanel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))
