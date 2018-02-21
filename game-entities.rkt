#lang racket

(provide (struct-out entity)
         (struct-out entity-name)
         entity-animation
         sprite->entity
         sprite->bb
         update-entity
         get-component
         add-component
         add-components
         get-name
         basic-entity

         colliding-with

         (struct-out game)
         image->bb
         touching?
         on-collide
         button-states
         button-states?
         button-states-left
         button-states-right
         button-states-up
         button-states-down
         key-movement
         
         start-game
         set-game-state

         draw)

(require posn)
(require 2htdp/image)
(require 2htdp/universe)
(require "animated-sprites.rkt")

(require threading)

(struct bb [w h])
(struct entity [components] #:transparent)

(struct entity-name (string))

(define (get-name e)
  (define n (get-component e entity-name?))
  (if n
      (entity-name-string n)
      #f))

(define (entity-animation e)
  (findf animated-sprite? (entity-components e)))

(define (entity-bb e)
  (findf bb? (entity-components e)))


(define (update-component components component-pred f)
  (define ci (index-where components component-pred))
  (define old-component (list-ref components ci))
  (if (procedure? f)
      (list-set components ci (f old-component))
      (list-set components ci f)))

(define (update-entity e component-pred f)
  (match-define (entity components) e)
  (entity (update-component components component-pred f)))

(define (get-component e component-pred)
  (findf component-pred (entity-components e)))

(define (add-component e c)
  (match-define (entity components) e)
  (entity (cons c components)))

(define (add-components e . cs)
  (if (empty? cs)
      e
      (apply (curry add-components (add-component e (first cs)))
             (rest cs))))

(define (basic-entity p s)
  (entity (list p
                (image->bb (render s))
                s)))

(define (sprite->entity sprite #:position p
                               #:name     n
                               #:components (c #f)
                               . cs)
  (define all-cs (filter identity (cons
                                   (entity-name n)
                                   (cons c cs))))
  (apply (curry add-components (basic-entity p sprite) )
         all-cs))

(define (sprite->bb s)
  (image->bb (render s)))

(define (image->bb i)
  (bb
   (image-width i)
   (image-height i)))

(define (touching? e1 e2)
  (match-define (entity components1)
                e1)

  (match-define (entity components2)
                e2)

  (match-define (bb e1-w e1-h) (get-component e1 bb? ))
  (match-define (bb e2-w e2-h) (get-component e2 bb? ))

  (match-define (posn e1-x e1-y) (get-component e1 posn? ))
  (match-define (posn e2-x e2-y) (get-component e2 posn? ))
  
  (if (and (>= (- e1-x e2-x) (+ (- e1-w) 10))
           (<= (- e1-x e2-x) (- e2-w     10))
           (>= (- e1-y e2-y) (+ (- e1-h) 10))
           (<= (- e1-y e2-y) (- e2-h     10)))
      #t
      #f))

(struct on-collide (name func))


;Input

(struct key-movement (speed))

(struct button-states [left right up down])

(define (button-states-set-left btn-states left)
  (struct-copy button-states btn-states [left left]))

(define (button-states-set-right btn-states right)
  (struct-copy button-states btn-states [right right]))

(define (button-states-set-up btn-states up)
  (struct-copy button-states btn-states [up up]))

(define (button-states-set-down btn-states down)
  (struct-copy button-states btn-states [down down]))


(struct game (entities input collisions) #:transparent)

(define (set-game-state g s)
  (game s
        (game-input g)))

;Consumes a world, handles a single key PRESS by setting button state to true and returning the world
(define (handle-key-down larger-state a-key)
  (define btn-states (game-input larger-state))
  (struct-copy game larger-state
               [input
                (cond
                  [(key=? a-key "left")  (button-states-set-left btn-states #t)]
                  [(key=? a-key "right") (button-states-set-right btn-states #t)]
                  [(key=? a-key "up")    (button-states-set-up btn-states #t)]
                  [(key=? a-key "down")  (button-states-set-down btn-states #t)]
                  [else btn-states])]))

;Consumes a world, handles a single key RELEASE by setting button state to false and returning the world
(define (handle-key-up larger-state a-key)
  (define btn-states (game-input larger-state))
  (struct-copy game larger-state
               [input
                (cond
                  [(key=? a-key "left")  (button-states-set-left btn-states #f)]
                  [(key=? a-key "right") (button-states-set-right btn-states #f)]
                  [(key=? a-key "up")    (button-states-set-up btn-states #f)]
                  [(key=? a-key "down")  (button-states-set-down btn-states #f)]
                  [else btn-states])]))


(define/contract (velocity-from-buttons btn-states speed)
  (-> button-states? number? posn?)
  (define leftVel  (if (button-states-left btn-states) (- speed) 0))
  (define rightVel (if (button-states-right btn-states)   speed  0))
  (define upVel    (if (button-states-up btn-states) (- speed) 0))
  (define downVel  (if (button-states-down btn-states)   speed  0))
  (posn (+ leftVel rightVel)
        (+ upVel downVel)))



(define (draw-entity e)
  (render (get-component e animated-sprite?)))

(define (draw-entities es)
  (if (= 1 (length es))
      (draw-entity (first es))
      (let* ([p (get-component (first es) posn?)]
             [x (posn-x p)]
             [y (posn-y p)])
        ;(displayln (first es))
        (place-image (draw-entity (first es))
                     x y
                     (draw-entities (rest es))))))


(define/contract (draw g)
  (-> game? image?)
  (define entities (game-entities g))
  (draw-entities entities))

(define (update-key-movement g e c)
  (update-entity e posn?
                 (curry posn-add
                        (velocity-from-buttons (game-input g)
                                               (key-movement-speed c)))))

(define (update-animated-sprite g e c)
  (update-entity e animated-sprite? next-frame))

(define (is-colliding? e g)
  (findf (curry member e) (game-collisions g)))

(define (is-colliding-with? name g me)
  (define names (map get-name (colliding-with me g)))
  (member name names))


(define (extract-out e l)
  (define (not-eq? e o)
    (not (eq? e o)))
  (filter (curry not-eq? e) l))

(define (colliding-with e g)
  (flatten
   (map (curry extract-out e)
        (filter (curry member e) (game-collisions g)))))

(define (tick-component g e c)
  (cond
    [(key-movement? c)              (update-key-movement g e c)]
    [(animated-sprite? c)           (update-animated-sprite g e c)]
    [(and (on-collide? c)
          (is-colliding-with? (on-collide-name c) g e))      ((on-collide-func c) g e)]
    [else e]))

(define (tick-entity g e)
  (foldl
   (lambda (c e2)
     (tick-component g e2 c))
   e
   (entity-components e)))

(define (tick-entities g)
  (define es (game-entities g))
  (struct-copy game g
               [entities (map (curry tick-entity g) es)]))

(define tick
  (lambda (g)
    (~> g
        update-collisions
        tick-entities)))

(define (current-collisions g)
  (filter (curry apply touching?)
          (combinations (game-entities g) 2)))

(define (update-collisions g)
  (struct-copy game g
               [collisions (current-collisions g)]))

(define (start-game . initial-world)
  (define larger-state (game initial-world
                             (button-states #f #f #f #f)
                             '()))
  
  (big-bang larger-state                         ; <-- initial state
            (on-tick    tick)                    ; <-- motion and collision detection happens here
            (to-draw    draw)                    ; <-- redraws the world
            (on-key     handle-key-down)         ; <-- process the event of key release
            (on-release handle-key-up)))         ; <-- process the event of key release

