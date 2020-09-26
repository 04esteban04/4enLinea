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

(define token (send selectToken get-string-selection))

;Panel encargado de mostrar el tablero
(define matrixPanel (new panel% [parent gameWindow]
                                [style (list 'border)]
                                [alignment '(center center)]
                                [vert-margin 50]
                                [horiz-margin 50]))

;Override de la clase pasteboard%, de manera que sea posible dibujar el tablero con la funcion draw-4Line-board
(define 4Line-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-4Line-board dc)))
    (define/override (on-event e)
      (define window-x (send e get-x))
      (define window-y (send e get-y))
      (when (eq? (send e get-event-type) 'left-down)
          (for ([id (in-hash-keys token-piece-data)])
          (define piece (make-token-piece id))
          (send board insert piece window-x window-y))))))

;Funcion encargada de dibujar el tablero
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
    (send dc draw-rectangle x y cell-width cell-height)))

;Tablero
(define board (new 4Line-board%))

;Canvas de clase editor%, encargado de contener el pasteboard correspondiente
(define boardContainer (new editor-canvas%
                       [parent matrixPanel]
                       [style '(no-hscroll no-vscroll)]
                       [horizontal-inset 0]
                       [vertical-inset 0]
                       [editor board]))

;###########################################

(define token-piece-data
  (hash
   "Token" #\u2b55))

(define token-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "token-snip"))))

(send (get-the-snip-class-list) add token-piece-snip-class)

(define token-piece%
  (class snip%
    (init-field glyph font size)
    (super-new)
    (send this set-snipclass token-piece-snip-class)

    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
    (cond 
      ((equal? "Rojo" (send selectToken get-string-selection))

      (send dc set-font font)
      (send dc set-text-foreground "red")
      (define-values (glyph-width glyph-height baseline extra-space)
      (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
      
      ((equal? "Azul" (send selectToken get-string-selection))
      
      (send dc set-font font)
      (send dc set-text-foreground "blue")
      (define-values (glyph-width glyph-height baseline extra-space)
      (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))))
        
(define (make-token-piece id)
  (define glyph (hash-ref token-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new token-piece% [glyph (string glyph)] [font font] [size 35]))



