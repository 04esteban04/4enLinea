#lang racket
(require racket/gui)
;############# PRIMERA VENTANA ##################

;Frame principal
(define Ventana1 (new frame% 
                [label "4 en Linea"]
                [width 500]
                [height 400]
                [style (list 'no-resize-border)]))

(new canvas% [parent Ventana1]
             [style (list 'transparent)]
             [min-width 10]
             [min-height 10]
             [paint-callback
             (lambda (canvas dc)
             (send dc set-scale 2 2)
             (send dc set-text-foreground "red")
             (send dc draw-text "4 en Linea!" 0 0))])


;Configuraciones
(define SelectToken(new choice% [parent Ventana1]
                                [label "Seleccione su ficha:  "]
                                [choices (list "Rojo Carmesi" "Verde Lima Amareto")]
                                [vert-margin 10]
                                ))



 

; Show the frame by calling its show method
  (send Ventana1 show #t)
