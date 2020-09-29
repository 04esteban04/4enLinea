#lang racket
(require racket/gui)
(require "Back/CheckWin-Lose.rkt")
(require "Back/GreedyAlgorithms.rkt")
(require embedded-gui)

;################################################
;########## IMPLEMENTACION CON LA LOGICA ########
;################################################

;LA MATRIZ PRINCIPAL
(define matrix '(()))

(define (position-piece event)
  (define-values (x y) (values (send event get-x) (+ 20 (send event get-y))))
  
  (cond
    ;######################################
    ;########### PRIMERA FILA #############

    ((and (>= x 0) (<= x 40) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 0) (<= y 40)
      (set! matrix (remplaceValue 1 0 15 matrix))
    ))

    ;######################################
    ;########### SEGUNDA FILA #############

    ((and (>= x 0) (<= x 40) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 40) (<= y 80)
      (set! matrix (remplaceValue 1 1 15 matrix))
    ))
    
    ;######################################
    ;########### TERCERA FILA #############

     ((and (>= x 0) (<= x 40) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 2 matrix))
      
    ))

    ((and (>= x 120) (<= x 160) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 80) (<= y 120)
      (set! matrix (remplaceValue 1 2 15 matrix))
    ))
    ;######################################
    ;########### CUARTA FILA ##############

    ((and (>= x 0) (<= x 40) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 120) (<= y 160)
      (set! matrix (remplaceValue 1 3 15 matrix))
    ))

    ;######################################
    ;########### QUINTA FILA ##############

    ((and (>= x 0) (<= x 40) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 160) (<= y 200)
      (set! matrix (remplaceValue 1 4 15 matrix))
    ))

    
    ;###########################################
    ;########### SEXTA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 200) (<= y 240)
      (set! matrix (remplaceValue 1 5 15 matrix))
    ))

    ;###########################################
    ;########### SEPTIMA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 240) (<= y 280)
      (set! matrix (remplaceValue 1 6 15 matrix))
    ))

  
    ;###########################################
    ;########### OCTAVA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 280) (<= y 320)
      (set! matrix (remplaceValue 1 7 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 270) (<= y 320)
      (set! matrix (remplaceValue 1 7 15 matrix))
    ))

    ;###########################################
    ;########### NOVENA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 320) (<= y 360)
      (set! matrix (remplaceValue 1 8 15 matrix))
    ))

    ;###########################################
    ;########### DECIMA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 360) (<= y 400)
      (set! matrix (remplaceValue 1 9 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 350) (<= y 400)
      (set! matrix (remplaceValue 1 9 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 350) (<= y 400)
      (set! matrix (remplaceValue 1 9 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 350) (<= y 400)
      (set! matrix (remplaceValue 1 9 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 350) (<= y 400)
      (set! matrix (remplaceValue 1 9 14 matrix))
      
    ))

    ((and (>= x 600) (<= x 640) (>= y 350) (<= y 400)
      (set! matrix (remplaceValue 1 9 15 matrix))
    ))

    ;###########################################
    ;########### UNDECIMA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 14 matrix))
    ))
    
    ((and (>= x 600) (<= x 640) (>= y 400) (<= y 440)
      (set! matrix (remplaceValue 1 10 15 matrix))
    ))

    ;###########################################
    ;########### DOCEAVA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 440) (<= y 480)
      (set! matrix (remplaceValue 1 11 15 matrix))
    ))

    ;###########################################
    ;########### TRECEAVA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 480) (<= y 520)
      (set! matrix (remplaceValue 1 12 15 matrix))
    ))

  
    ;###########################################
    ;########### CATORCEAVA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 520) (<= y 560)
      (set! matrix (remplaceValue 1 13 15 matrix))

    ))

    ;###########################################
    ;########### QUINCEAVA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 14 matrix))
    ))

    ((and (>= x 600) (<= x 640) (>= y 560) (<= y 600)
      (set! matrix (remplaceValue 1 14 15 matrix))
    ))

    ;###########################################
    ;########### DECIMOSEXTA FILA ###################

    ((and (>= x 0) (<= x 40) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 0 matrix))
    ))
      
    ((and (>= x 40) (<= x 80) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 1 matrix))
    ))

    ((and (>= x 80) (<= x 120) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 2 matrix))
    ))

    ((and (>= x 120) (<= x 160) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 3 matrix))
    ))

    ((and (>= x 160) (<= x 200) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 4 matrix))
    ))

    ((and (>= x 200) (<= x 240) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 5 matrix))
    ))
    
    ((and (>= x 240) (<= x 280) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 6 matrix))
    ))
    
    ((and (>= x 280) (<= x 320) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 7 matrix))
    ))
    
    ((and (>= x 320) (<= x 360) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 8 matrix))
    ))

    ((and (>= x 360) (<= x 400) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 9 matrix))
    ))

    ((and (>= x 400) (<= x 440) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 10 matrix))
    ))

    ((and (>= x 440) (<= x 480) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 11 matrix))
    ))

    ((and (>= x 480) (<= x 520) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 12 matrix))
    ))

    ((and (>= x 520) (<= x 560) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 13 matrix))
    ))

    ((and (>= x 560) (<= x 600) (>= y 600) (<= y 640)
      (set! matrix (remplaceValue 1 15 14 matrix))
          
    ))

    ((and (>= x 600) (<= x 640) (> y 580) (<= y 640)
      (set! matrix (remplaceValue 1 15 15 matrix))
      (print matrix)
    ))

    (else
      (print "Error")
  
  )
  
)

;############################################################
;#############  CHECK WIN-LOSE  PLAYER TURN #################
    
    (cond
    ((equal? 1 (checkWinLose matrix))
      (send winnerAlert show #t)
      (exit))

    ((equal? 2 (checkWinLose matrix))
      (send loserAlert show #t))
  
    ((equal? 3 (checkWinLose matrix))
      (send tieAlert show #t))
    )
)


;#####################################################
;############### INTERFAZ GRAFICA ####################
;#####################################################

;FRAME DEL JUEGO
(define gameWindow (new frame% 
                [label "4 en Linea"]
                [width 640]
                [height 640]
                [style (list 'no-resize-border)]))

(define winnerAlert (new dialog% [label "Alerta"]
                 [parent gameWindow]
                 [width 300]
                 [height 300]))

(define loserAlert (new dialog% [label "Alerta"]
                 [parent gameWindow]
                 [width 300]
                 [height 300]))

(define tieAlert (new dialog% [label "Alerta"]
                 [parent gameWindow]
                 [width 300]
                 [height 300]))

(define winnerCanvas (new canvas% [parent winnerAlert]
                                  [paint-callback 
                                  (lambda (canvas dc)
                                  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                                  (send dc draw-text "GANASTE!" 10 10)
                                  (let ((winnerPicture (make-object bitmap% "3enlinea/4enLinea.png")))
                                  (send dc draw-bitmap winnerPicture 30 50)))]))


(define loserCanvas (new canvas% [parent loserAlert]
                                  [paint-callback 
                                  (lambda (canvas dc)
                                  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                                  (send dc draw-text "PERDISTE!" 10 10)
                                  (let ((winnerPicture (make-object bitmap% "3enlinea/4enLinea.png")))
                                  (send dc draw-bitmap winnerPicture 30 50)))]))


(define tieCanvas (new canvas% [parent tieAlert]
                                  [paint-callback 
                                  (lambda (canvas dc)
                                  (send dc set-font (make-object font% 20 "Century Gothic" 'decorative 'normal 'bold))
                                  (send dc draw-text "EMPATE!" 10 10)
                                  (let ((winnerPicture (make-object bitmap% "3enlinea/4enLinea.png")))
                                  (send dc draw-bitmap winnerPicture 30 50)))]))


;FRAME DE CONFIGURACIONES
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
                           (define matrixRows (string->number (send selectRow get-string-selection)))
                           (define matrixCols (string->number (send selectColumn get-string-selection)))
                           (set! matrix (createMatrix matrixRows matrixCols))
                           (send gameWindow show #t)
                           (send topWindow show #f))]))



(define token (send selectToken get-string-selection))

;#############################################################
;############### TABLERO CON LOGICA Y GUI ####################
;#############################################################



;Override de la clase pasteboard%, de manera que sea posible dibujar el tablero con la funcion draw-4Line-board
(define 4Line-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-4Line-board dc)))
    
    (define/override (on-event e)
      (when (eq? (send e get-event-type) 'left-down)

        (position-piece e)

        (sleep/yield 1)
        (set! matrix (greedyAlgorithm matrix))
        (print matrix)
       
        (define window-x (send e get-x))
        (define window-y (send e get-y))

        (for ([id (in-hash-keys token-piece-data)])
        (define piece (make-token-piece id))
        (define piece1 (make-tokenBOT-piece id))
        (send board insert piece (- window-x 15) (- window-y 15))

;###########################################################################
;############### POSICIONES PARA LA FICHA DE LA MAQUINA ####################

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
    (send board insert piece 0 30))
    
    (when (equal? 2 (list-ref(list-ref matrix 1) 1)) 
    (define piece (make-tokenBOT-piece id))
    (send board insert piece 40 30))
    
    (when (equal? 2 (list-ref(list-ref matrix 1) 2)) 
    (define piece (make-tokenBOT-piece id))
    (send board insert piece 80 30))
    
    (when (equal? 2 (list-ref(list-ref matrix 1) 3)) 
    (define piece (make-tokenBOT-piece id))
    (send board insert piece 120 30))
    
    (when (equal? 2 (list-ref(list-ref matrix 1) 4)) 
    (define piece (make-tokenBOT-piece id))
    (send board insert piece 160 30))
    
    (when (equal? 2 (list-ref(list-ref matrix 1) 5)) 
    (define piece (make-tokenBOT-piece id))
    (send board insert piece 200 30))

    (when (equal? 2 (list-ref(list-ref matrix 1) 6)) 
    (define piece (make-tokenBOT-piece id))
    (send board insert piece 240 30))

    (when (equal? 2 (list-ref(list-ref matrix 1) 7)) 
    (define piece (make-tokenBOT-piece id))
    (send board insert piece 280 30))

    (when (>= columns 9) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 8)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 320 30)))
    
    (when (>= columns 10) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 9)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 360 30)))

    (when (>= columns 11) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 10)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 400 30)))     
    
    (when (>= columns 12) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 11)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 440 30)))

    (when (>= columns 13) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 12)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 480 30)))

    (when (>= columns 14) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 13)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 520 30)))
    
    (when (>= columns 15) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 14)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 560 30)))
      
    (when (= columns 16) 
      (when (equal? 2 (list-ref(list-ref matrix 1) 15)) 
        (define piece (make-tokenBOT-piece id))
        (send board insert piece 600 30)))

    
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

;#############################################################
;#############  CHECK WIN-LOSE  MACHINE TURN #################
    (cond
    ((equal? 1 (checkWinLose matrix))
      (send winnerAlert show #t)
      (exit))

    ((equal? 2 (checkWinLose matrix))
      (send loserAlert show #t))
  
    ((equal? 3 (checkWinLose matrix))
      (send tieAlert show #t)))
   ) 
  )
 )
)
)


;###############################################################
;######################## TABLERO ##############################


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
    (send dc draw-ellipse x y cell-width cell-height)))

;Tablero
(define board (new 4Line-board%))

;Canvas de clase editor%, encargado de contener el pasteboard correspondiente
(define boardContainer (new editor-canvas%
                       [parent gameWindow]
                       [style '(no-hscroll no-vscroll)]
                       [horizontal-inset 0]
                       [vertical-inset 0]
                       [editor board]))

;Unicode del Token de los dos jugadores
(define token-piece-data
  (hash
   "Token" #\u2b55))

;Define a la clase tipo snip, para poder pegarlo en el pasteboard
(define token-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "token-snip"))))

(send (get-the-snip-class-list) add token-piece-snip-class)

;
(define tokenBOT-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "tokenBOT-snip"))))

(send (get-the-snip-class-list) add tokenBOT-piece-snip-class)

;Define al objeto Token, de tipo snip class
(define token-piece%
  (class snip%
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass token-piece-snip-class)
    
    ;Configura el tamano del Token
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 1.0))
      (when space (set-box! space 1.0))
      (when lspace (set-box! lspace 1.0))
      (when rspace (set-box! rspace 1.0)))

     ;Dibuja el token en el board segun el color que eligio el jugador 
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
    ))
  )
)

(define tokenBOT-piece%
  (class snip%
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass tokenBOT-piece-snip-class)
    
    ;Configura el tamano del bot Token
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 1.0))
      (when space (set-box! space 1.0))
      (when lspace (set-box! lspace 1.0))
      (when rspace (set-box! rspace 1.0)))

     ;Dibuja el token del bot en el board segun el color que eligio el jugador 
    (define/override (draw dc x y . other)
    (cond 
      ((equal? "Rojo" (send selectToken get-string-selection))

      (send dc set-font font)
      (send dc set-text-foreground "blue")
      (define-values (glyph-width glyph-height baseline extra-space)
      (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
      
      ((equal? "Azul" (send selectToken get-string-selection))
      
      (send dc set-font font)
      (send dc set-text-foreground "red")
      (define-values (glyph-width glyph-height baseline extra-space)
      (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
       
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))
  )
)

 ;Crea el Token piece       
(define (make-token-piece id)
  (define glyph (hash-ref token-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new token-piece% [glyph (string glyph)] [font font] [size 35])
)

(define (make-tokenBOT-piece id)
  (define glyph (hash-ref token-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new tokenBOT-piece% [glyph (string glyph)] [font font] [size 35])
)





