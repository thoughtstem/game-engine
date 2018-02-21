#lang racket

(require racket/match)
(require 2htdp/image)
(require 2htdp/universe)
(require "animated-sprites.rkt")
(require "sprite-machine.rkt")

(define (pad i)
  (overlay i
           (square 60 "solid" "transparent")))

(define (c1 c) (pad (circle 18 "solid" c)))
(define (c2 c) (pad (circle 20 "solid" c)))
(define (c3 c) (pad (circle 22 "solid" c)))

(define (my-sprite c r )
  (new-sprite
    (list (c1 c)
          (c2 c)
          (c3 c)
          (c2 c)
          (c1 c))
    r))

(define s1 (my-sprite "red"   1))
(define s2 (my-sprite "green" 1))
(define bg (square 200 "solid" "white"))

(define sm2
  (sprite-machine
        'red
        (hash 'red s1
              'green s2)
        (hash 'red   (list
                      (transition (lambda (x) (current-animation-finished? x)) 'green))
              'green (list
                      (transition (lambda (x) (current-animation-finished? x)) 'red)))))

(define monster
  (bitmap/url "https://cdn.tutsplus.com/mobile/uploads/legacy/Corona-SDK_Side-Scroller/3/monsterSpriteSheet.png"))

(define monster-sprite-right
  (new-sprite
     (sheet->costume-list monster 7 1 6)
     1))

(define monster-sprite-left
  (struct-copy animated-sprite monster-sprite-right
               [frames (map flip-horizontal (animated-sprite-frames monster-sprite-right))]))

(define sm
  (sprite-machine
        'left
        (hash 'left monster-sprite-left
              'right monster-sprite-right)
        (hash 'left (list
                      #;(transition (lambda (x) (current-animation-finished? x)) 'right))
              'right (list
                      #;(transition (lambda (x) (current-animation-finished? x)) 'left))
        )))

(struct game (sm input))

(define (tick state)
  (match-define (game sm input) state)
  (game
         (update-machine sm)
         input))

(define (key state a-key)
  (match-define (game sm input) state)
  (define new-input
    (cond
      [(key=? a-key "left")  'left]
      [(key=? a-key "right") 'right]
      [else 'left]))
  (define new-sm (transition-to sm new-input))  ;Hmmm. we should be transitioning in tick...  Everything will hapen in tick..
  (game new-sm new-input)                       ;Otherwise, why are we even storing input in the state when it's in the machine already?
  )

(big-bang (game sm 'left)
          (on-tick tick)
          (on-key  key)
          (to-draw (lambda (x) (render (current-sprite (game-sm x))))))


