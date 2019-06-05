#lang racket

(require game-engine
         2htdp/image)

;Just open this in an editor, then run it from the commandline.
;  Edit the code and get live changes.

(hotswap me
  (game 
    (entity
      (position (posn 200 200))
      (sprite (register-sprite 
                (circle 40 'solid 'red))))))



