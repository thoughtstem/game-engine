#lang racket

(require "../main.rkt")

(require 2htdp/image)   
        
(define green-star
  (register-sprite (star 40 'solid 'green)))
      
(define-component counter number?)                                         
    
(define e
  (entity
    (sprite green-star)
      
    (counter 0 
             (^ add1))

    (rotation 0 
              (remainder (get-counter) 360))
        
    (size 1 
	  (sin
	    (/ (get-counter) 100)))

    (position (posn 0 200)
              (struct-copy posn (get-position)
                           [x (get-counter)]))))                           
                                                                           
(play! (game e))   
