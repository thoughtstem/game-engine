#lang racket

(require "../extensions/main.rkt"
         "../extensions/meta-games/hotswap.rkt"
         2htdp/image)

;TODO: Hotswap is broken due to the refactoring.  Fix it. 
;  Make it not leave trash files while we're at it...

(hotswap me
  (game 
    (entity
      (counter 0 (add1 (get-counter)))
      (size 0 (/ (get-counter) 1000))
      (position (posn 200 200)
                (posn (get-counter) 200))
      (rotation 0  
                (sin (/ (get-counter) 75)))
      (sprite (register-sprite 
                (above
                  (square 20 'solid 'orange)      
                  (star 40 'solid 'green)))))
    (entity
      (position (posn 100 200))
      (counter 0 (add1 (get-counter)))
      (rotation 0  (sin (/ (get-counter) 50)))
      (sprite (register-sprite 
                (above
                  (circle 20 'solid 'orange)      
                  (star 40 'solid 'green)))))
    (entity
      (position (posn 300 200))
      (counter 0 (add1 (get-counter)))
      (rotation 0  (sin (/ (get-counter) 100)))
      (sprite (register-sprite 
                (above
                  (circle 20 'solid 'red)      
                  (star 40 'solid 'blue)))))))



