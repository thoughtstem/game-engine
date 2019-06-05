#lang racket

(require game-engine
         2htdp/image)

;TODO:  Make it not leave trash files while we're at it...

(no-hotswap me
  (game 
    (entity
      (position (posn 200 200))
      (sprite (register-sprite 
                (circle 40 'solid 'green))))))



