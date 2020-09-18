#lang racket
(require racket/gui)

; Make a frame by instantiating the frame% class
  (define Main (new frame% 
                [label "4 en Linea"]
                [width 600]
                [height 400]
                [style '(fullscreen-button)]
                [alignment '(left top)]))
  
  ; Make a static text message in the frame
  
(define Title (new message% [parent Main]
                          [label "4 EN LINEA"]
                          [vert-margin 10]
                          [horiz-margin 300]))

(define Tabletx (new message% [parent Main]
                            [label "Tamaño del Tablero:"]
                            [vert-margin 20]
                            [horiz-margin 200]))

(define Colortx (new message% [parent Main]
                            [label "Eliga su ficha:"]
                            [vert-margin 30]
                            [horiz-margin 200]))





  
  ; Show the frame by calling its show method
  (send Main show #t)
