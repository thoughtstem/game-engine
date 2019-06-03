#lang racket

(require "../extensions/main.rkt")

(require threading
         (prefix-in h: 2htdp/image))

(require "../core/test/conway-impl.rkt")

;Over a 1000 entities rendering a simple conway game.  Runs at 20+ FPS on my low-end CB.

(define donut
  '((* * *)
    (* _ *)
    (* * *)))

(define square-3 (square 3))

(define padded-donut
  (overlay  
    (square 11)
    donut))

(define quilt
  (beside
    (above donut square-3)
    (above square-3 donut)))

(define padded-donut3
  (beside (beside padded-donut padded-donut) padded-donut))

(define quilted-donut
  (above quilt padded-donut3))

(define quilted-donut2
  (above quilted-donut quilted-donut))

(define-component conway-manager any/c)

(define live-sprite (register-sprite (h:circle 5 'solid 'green)))
(define dead-sprite (register-sprite (h:circle 5 'solid 'red)))

(define (pick-sprite r c)
  (if (alive? quilted-donut2 c r) 
    live-sprite
    dead-sprite))

(play! 
  (game
    (entity 
      (counter 0 (^ add1))
      (conway-manager (void)
                      (when (= 0 (remainder (get-counter) 10))
                        (set! quilted-donut2
                          (conway-tick quilted-donut2))))

      (also-render
        (game
          (entity-grid 400 400 12
                       (lambda (r c)
                         (list 
                           (sprite (thunk* (pick-sprite r c)))
                           )))))
      )))  



