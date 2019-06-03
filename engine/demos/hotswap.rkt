#lang racket

(require "../extensions/main.rkt"
         "../extensions/meta-games/hotswap.rkt"
         2htdp/image)

;TODO: Hotswap is broken due to the refactoring.  Fix it. 
;  Make it not leave trash files while we're at it...

(hotswap me
  (game 
    (entity
      (position (posn 200 200))
      (sprite (register-sprite (circle 40 'solid 'red))))))



