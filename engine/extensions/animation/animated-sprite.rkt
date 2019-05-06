#lang racket

(provide 
  Sprite
  Sprite?
  sprite-id
  get-Sprite
  register-sprite
  
  #;
  (rename-out [make-position position])
  x
  y
  #;
  update:position/x^
  #;
  update:position/y^
  #;
  position?

  get-queued-sprites
  flush-queued-sprites!
  
  move-to

  #;
  spawn-here

  #;
  move-to-parent

  name
  named?
  get-by-name
  set-insertion-queue!
  set-seen-sprite-ids! 
  )

(require "../../core/main.rkt"
         2htdp/image)

(define-component name string?)

(define (named? e s)
  (define n
    (get-component e name?))     
  (and n
       (string? s)
       (string=? s
                 (get-name n))))

(define (get-by-name g s)
  (findf (curryr named? s) (game-entities g)))

(require posn)
(define-component Position posn?)

(define (x e)
  (define p
    ;TODO
    ;Slow...
    #;
    (get-component e Position?)


    ;Maybe faster?
    (get-component e 'Position)

    ;Fast...
    #;
    (list-ref (entity-components e) 1))
  (posn-x (get-Position p)))

(define (y e)
  (define p
    #;
    (get-component e Position?)

    ;Maybe faster?
    (get-component e 'Position)

    #;
    (list-ref (entity-components e) 1))

  (posn-y (get-Position p)))

(define (move-to p e)

  (define current-p 
    (get-component e Position?))

  (define new-p
    (set-Position current-p p))

  (update-component e 
                    current-p
                    new-p))

;A component that wraps a single sprite id
;  there's no animation at this component's level.
;  It can be used to create animation systems with more complex
;  components.
(define-component Sprite symbol?)

(define (sprite-id s)
  (get-Sprite s))

;Whenever you construct a new sprite, it ends up in the
; insertion queue, along with its id.  This is the last step
; before we throw the image away (to the gcard) and refer to it only by id.
(define insertion-queue '())  ;Holds images, Gets emptied
(define seen-sprite-ids '())  ;Doesn't hold images, Doesn't get emptied

(define (seen? i)
  (member i seen-sprite-ids))


(define #;/contract 
  (image->id i)

  #;
  (-> image? symbol?)
  ;I'm sure this is super slow.
  ;We should figure out how to warn the user if the are unknowingly calling this at runtime (e.g. by constructing new sprites at runtime).
  ;But as long as you declare all your sprite components before the game starts, there will be no overhead.
  ;NOTE: This overhead happens ANY time you do (sprite ...) -- regardless of whether you pass in an image that has already been compiled.  Just the act of checking WHETHER that image has been compiled previously triggers this function.

  (string->symbol
    (~a "sprite-"
        (equal-hash-code (~a (image->color-list i))))))

(define (register-sprite i)
  (let ([id (image->id i)])
    (when (not (seen? id))
      (set-insertion-queue! (cons (list id i) insertion-queue))
      (set-seen-sprite-ids! (cons id seen-sprite-ids)))

    id))

(define (set-insertion-queue! l)
  (set! insertion-queue l))

(define (set-seen-sprite-ids! l)
  (set! seen-sprite-ids l))

(define (get-queued-sprites) insertion-queue)
(define (flush-queued-sprites!) 
  (set! insertion-queue '()))




;Is this where we tie in the mode-lambda stuff??
;When do things get added to the sprite database?
;What queries will need to be made to it?

(define sprite-db (list))

;Can happen at runtime...
(define (insert-sprite s)
  42 ;Return id??
  )

(define (get-id s)
  
  42)





;Under this rendering semantics:
;  Every entity that you want rendered should have
;   1) one or more animated-sprite components, each of which will respond to a (render ...) call with a sprite id.  
;   2) a position component 
;   2) a size component
;   3) [later?] a scale component
;   4) [later?] a rotation component
;   5) [later?] a layer component

;On every tick, the rendering system will look at the collection of entities with animated sprites.  Let's call that es:

; entities will be rendered as if sorted by layer, and then by order of animated sprites on the entity.  their position will be translated to screen coordinates according to some parameterizable scalar factor. 
; the width and height of the draw area will be determined by the size of the game's bounding box on the first tick.  It should not change after that. 

;As for sprites, I'd like to try to minimize memory usage.  One option is to make the user register all the sprite up front, then reference them by ids from then on.  We can throw them away after they go to the graphics card.

;Buuut.  That feels onerous compared to letting the user lexically see what sprite will be attached to what entity..






