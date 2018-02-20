#lang racket

(provide (struct-out entity)
         (struct-out game)
         image->bb
         touching?
         button-states
         button-states?
         button-states-left
         button-states-right
         button-states-up
         button-states-down
         start-game
         set-game-state)

(require posn)
(require 2htdp/image)
(require 2htdp/universe)

(struct bb [w h])
(struct entity [posn bb])

(define (image->bb i)
  (bb
   (image-width i)
   (image-height i)))

(define (touching? e1 e2)
  (match-define (entity (posn e1-x e1-y)
                        (bb   e1-w e1-h))
                e1)

  (match-define (entity (posn e2-x e2-y)
                        (bb   e2-w e2-h))
                e2)
  
  (if (and (>= (- e1-x e2-x) (+ (- e1-w) 10))
           (<= (- e1-x e2-x) (- e2-w     10))
           (>= (- e1-y e2-y) (+ (- e1-h) 10))
           (<= (- e1-y e2-y) (- e2-h     10)))
      #t
      #f))


;Input

(struct button-states [left right up down])

(define (button-states-set-left btn-states left)
  (struct-copy button-states btn-states [left left]))

(define (button-states-set-right btn-states right)
  (struct-copy button-states btn-states [right right]))

(define (button-states-set-up btn-states up)
  (struct-copy button-states btn-states [up up]))

(define (button-states-set-down btn-states down)
  (struct-copy button-states btn-states [down down]))



(struct game (state input))

(define (set-game-state g s)
  (game s
        (game-input g)))

;Consumes a world, handles a single key PRESS by setting button state to true and returning the world
(define (handle-key-down larger-state a-key)
  (match-define (game s btn-states) larger-state)
  (game s
   (cond
    [(key=? a-key "left")  (button-states-set-left btn-states #t)]
    [(key=? a-key "right") (button-states-set-right btn-states #t)]
    [(key=? a-key "up")    (button-states-set-up btn-states #t)]
    [(key=? a-key "down")  (button-states-set-down btn-states #t)]
    [else btn-states])))

;Consumes a world, handles a single key RELEASE by setting button state to false and returning the world
(define (handle-key-up larger-state a-key)
  (match-define (game s btn-states) larger-state)
  (game s
   (cond
    [(key=? a-key "left")  (button-states-set-left btn-states #f)]
    [(key=? a-key "right") (button-states-set-right btn-states #f)]
    [(key=? a-key "up")    (button-states-set-up btn-states #f)]
    [(key=? a-key "down")  (button-states-set-down btn-states #f)]
    [else btn-states])))


(define (start-game initial-world tick draw)
  (define larger-state (game initial-world
                             (button-states #f #f #f #f)))
  (big-bang larger-state                         ; <-- initial state
            (on-tick    tick)                    ; <-- motion and collision detection happens here
            (to-draw    draw)                    ; <-- redraws the world
            (on-key     handle-key-down)         ; <-- process the event of key release
            (on-release handle-key-up)))         ; <-- process the event of key release

