#lang racket

(provide new-component
         new-game-function
         (struct-out entity)
         (struct-out entity-name)
         
         (struct-out hidden)
         
         (struct-out game)
         
         entity-animation
         sprite->entity
         sprite->bb
         update-entity
         get-component
         add-component
         remove-component
         add-components
         get-name
         basic-entity
         dead
         die
         get-entity

         colliding-with
         is-colliding-with?

         
         image->bb
         touching?
         button-states
         button-states?
         button-states-left
         button-states-right
         button-states-up
         button-states-down
         
         
         physical-collider
         
         start-game

         set-game-state

         draw
         game-width
         game-height)

(require posn)
(require 2htdp/image)
(require 2htdp/universe)
(require "./components/animated-sprite.rkt")

(require threading)

(struct bb [w h])
(struct entity [components] #:transparent)

(struct entity-name (string))

(define component-handlers (hash))

(define (new-component struct? update)
  (set! component-handlers
        (hash-set component-handlers struct? update)))

(define (get-handler-for-component c)
  (define types    (hash-keys component-handlers))
  (define has-type (lambda (c t) (t c)))
  (define type     (findf (curry has-type c) types))
  (hash-ref component-handlers type #f))


(define game-functions (list))

(define (new-game-function update)
  (set! game-functions
        (append game-functions (list update))))


;Animated sprites are components, but we'll handle them specially
; at least until we can untangle them bettter...
(define (update-animated-sprite g e c)
  (update-entity e animated-sprite? next-frame))

(new-component animated-sprite?
               update-animated-sprite)




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
  (if ci
      (let [(old-component (list-ref components ci))]
        (if old-component
            (if (procedure? f)
                (list-set components ci (f old-component))
                (list-set components ci f))
            components))
      components))

(define (update-entity e component-pred f)
  (match-define (entity components) e)
  (entity (update-component components component-pred f)))

(define/contract (get-component e component-pred)
  (-> entity? any/c any/c)
  (findf component-pred (entity-components e)))

(define (add-component e c)
  (match-define (entity components) e)
  (entity (cons c components)))

(define (remove-component e c?)
  (match-define (entity components) e)
  (entity (filter (lambda (c) (not (c? c))) components)))

(define (add-components e . cs)
  (if (empty? cs)
      e
      (apply (curry add-components (add-component e (first cs)))
             (rest cs))))

(define (basic-entity p s)
  (entity (list p
                (image->bb (render s))
                s)))

(define (get-entity name g)
  (define (has-name n e)
    (string=? n (get-name e)))
  (findf (curry has-name name) (game-entities g)))

(define (sprite->entity sprite-or-image #:position p
                               #:name     n
                               #:components (c #f)
                               . cs)
  (define all-cs (filter identity (cons
                                   (entity-name n)
                                   (cons c cs))))
  (define sprite (if (animated-sprite? sprite-or-image)
                     sprite-or-image
                     (new-sprite sprite-or-image)))
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

  (if (and (>= (- e1-x e2-x) (- (+ (+ (/ e1-w 2) (/ e2-w 2)) 10)))
           (<= (- e1-x e2-x)    (- (+ (/ e1-w 2) (/ e2-w 2)) 10))
           (>= (- e1-y e2-y) (- (+ (+ (/ e1-h 2) (/ e2-h 2)) 10)))
           (<= (- e1-y e2-y)    (- (+ (/ e1-h 2) (/ e2-h 2)) 10)))
      #t
      #f))


(struct physical-collider ())

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






(define (draw-entity e)
  (render (get-component e animated-sprite?)))

(define (draw-entities es)
  (if (= 1 (length es))
      (draw-entity (first es))
      (let* ([p (get-component (first es) posn?)]
             [x (posn-x p)]
             [y (posn-y p)])
        (place-image (draw-entity (first es))
                     x y
                     (draw-entities (rest es))))))


(define/contract (draw g)
  (-> game? image?)
  (define (not-hidden e) (not (get-component e hidden?)))
  (define entities (filter not-hidden (game-entities g)))
  (draw-entities entities))



 
(define (is-colliding? e g)
  (findf (curry member e) (game-collisions g)))

(define (is-colliding-with? name g me)
  (define names (map get-name (colliding-with me g)))
  (member name names))

(define (colliding-with-other-physical-colliders? g e)
  (and (get-component e physical-collider?)
       (not
        (empty?
         (filter identity
                 (map (curryr get-component physical-collider?) (colliding-with e g)))))))

(define (extract-out e l)
  (define (not-eq? e o)
    (not (eq? e o)))
  (filter (curry not-eq? e) l))

(define (colliding-with e g)
  (flatten
   (map (curry extract-out e)
        (filter (curry member e) (game-collisions g)))))

(define (main-tick-component g e c)
  (define handler (get-handler-for-component c))
  (if handler
      (handler g e c)
      e))

(define (tick-component g e c)
  (define next-entity-state (main-tick-component g e c))
  next-entity-state)

(define (main-tick-entity g e)
  (foldl
   (lambda (c e2)
     (tick-component g e2 c))
   e
   (entity-components e)))

(define (tick-entity g e)
  ;Naive physics:
  ;If there's a physical collider on e,
  ; And we were previously not colliding,
  ; And we are colliding on the next state,
  ; Then set the entity's position back to the previous position
  (define previous-posn (get-component e posn?))
  
  (define next-entity-state (main-tick-entity g e))

  (define predicted-g (update-collisions (game-replace-entity g next-entity-state)))
  
  (if (colliding-with-other-physical-colliders? predicted-g next-entity-state)
      (update-entity next-entity-state posn? previous-posn)
      next-entity-state))

(define (tick-entities g)
  (define es (game-entities g))
  (struct-copy game g
               [entities (map (curry tick-entity g) es)]))


(define (do-game-functions g)
  (define pipeline (lambda (a-fun a-game)
                     (a-fun a-game)))
  (foldl pipeline g game-functions))

(define tick
  (lambda (g)
    (~> g
        update-collisions
        tick-entities
        handle-dead
        do-game-functions
        )))

(define (handle-dead g)
  (define is-alive? (lambda (e) (not (get-component e dead?))))
  (struct-copy game g
               [entities (filter is-alive? (game-entities g))]))


(define (game-replace-entity g e)
  (define (replace-entity e1)
    (lambda (e2)
      (if (equal? (get-name e1)
                  (get-name e2))
          e1
          e2)))
  (struct-copy game g
               [entities (map (replace-entity e) (game-entities g))]))

(define (current-collisions g)
  (filter (curry apply touching?)
          (combinations (game-entities g) 2)))

(define (update-collisions g)
  (struct-copy game g
               [collisions (current-collisions g)]))


;DEAD ENTITIES

(struct dead ())

(define (die g e)
  (add-component e (dead)))

;END DEAD ENTITIES

;HIDDEN

(struct hidden ())

;END HIDDEN

(define (game-width g)
  (image-width (draw g)))


(define (game-height g)
  (image-height (draw g)))

(define (sample-bg w)
  (define bg-sprite
          (sheet->sprite (square w "solid" "black")
                 #:rows        1
                 #:columns     1
                 #:row-number  1
                 #:speed       1))
  (sprite->entity bg-sprite
                  #:position   (posn 0 0)
                  #:name       "bg"))



(define (start-game . initial-world)
  (define larger-state (game initial-world
                             (button-states #f #f #f #f)
                             '()))
  (big-bang larger-state                        
            (on-tick    tick)                    
            (to-draw    draw)                    
            (on-key     handle-key-down)         
            (on-release handle-key-up)))         
