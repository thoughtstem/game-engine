#lang racket

(provide new-component
         new-game-function
         (struct-out entity)
         (struct-out entity-name)
         
         (struct-out hidden)
         (struct-out disabled)

         (struct-out active-on-bg)
         
         (struct-out game) 
         (struct-out bb)

         entity-eq?
         entity-animation
         sprite->entity
         sprite->bb
         update-entity
         get-component
         add-component
         remove-component
         add-components
         get-name
         get-id
         change-name
         basic-entity
         dead
         die
         get-entity

         colliding-with
         is-colliding-with?
         is-colliding-by-name?

         
         image->bb
         touching?
    
         add-circle-collider
         (struct-out physical-collider)
         
         start-game

         set-game-state

         draw
         game-width
         game-height)

(require posn)
(require 2htdp/image)
(require 2htdp/universe)
(require "./components/animated-sprite.rkt")
;(require "./components/counter.rkt")
(require "./collision-helper.rkt")

(require threading)

(require (for-syntax racket/syntax))

(struct bb [w h])
(struct id [id])
(struct cc [r]) ;<- Circle Collider
(struct entity [components] #:transparent)

(struct entity-name (string) #:transparent)

(define component-handlers (hash))

(define (new-component struct? update)
  (set! component-handlers
        (hash-set component-handlers struct? update)))

(define (get-handler-for-component c)
  (define types    (hash-keys component-handlers))
  (define has-type (lambda (c t) (t c)))
  (define type     (findf (curry has-type c) types))
  (hash-ref component-handlers type #f))


(define (component? x)
  (get-handler-for-component x))


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

(define (get-id e)
  (id-id (get-component e id?)))

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

;You can pass in a predicate or an actual component
(define/contract (get-component e maybe-pred)
  (-> entity? any/c any/c)
  (define component-pred
    (if (procedure? maybe-pred)
        maybe-pred
        (lambda (c)
          (eq? c maybe-pred))))
  (findf component-pred (entity-components e)))

(define (add-component e c)
  (match-define (entity components) e)
  (entity (cons c components)))

(define (remove-component e c?)
  (match-define (entity components) e)
  (entity (filter (lambda (c) (not (c? c))) components)))

(define (add-components e . cs)
  (define flattened (flatten cs))
  (if (empty? flattened)
      e
      (apply (curry add-components (add-component e (first flattened)))
             (rest flattened))))

(define (basic-entity p s)
  (entity (list (id (random 10000000))
                p
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
  (define all-cs (flatten (filter identity (cons
                                            (entity-name n)
                                            (cons c cs)))))
  (define sprite (if (animated-sprite? sprite-or-image)
                     sprite-or-image
                     (new-sprite sprite-or-image)))
  (apply (curry add-components (basic-entity p sprite) )
         all-cs)
  )

(define (sprite->bb s)
  (image->bb (render s)))

(define (image->bb i)
  (bb
   (image-width i)
   (image-height i)))


(define (add-circle-collider e r)
  (add-component (remove-component e bb?) (cc r)))


(define (bb->points bounding-box p)
  (match-define (bb w h) bounding-box)
  (match-define (posn x y) p)

  (list (posn (- x (/ w 2)) (- y (/ h 2)))
   (posn (+ x (/ w 2)) (- y (/ h 2)))
   (posn (+ x (/ w 2)) (+ y (/ h 2)))
   (posn (- x (/ w 2)) (+ y (/ h 2))))
  )

(define (touching? e1 e2)
  (match-define (entity components1)
                e1)

  (match-define (entity components2)
                e2)

  (define (rect-case)
    (match-define (bb e1-w e1-h) (get-component e1 bb? ))
    (match-define (bb e2-w e2-h) (get-component e2 bb? ))
    (rect-hits-rect? (get-component e1 posn?) e1-w e1-h
                          (get-component e2 posn?) e2-w e2-h))
  

  (define (circle-rect-case-1)
    (define rect-points (bb->points (get-component e2 bb?) (get-component e2 posn?)))
    (apply (curry circle-hits-rect? (get-component e1 posn?) (cc-r (get-component e1 cc?))) rect-points))

  (define (circle-rect-case-2)
    (define rect-points (bb->points (get-component e1 bb?) (get-component e1 posn?)))
    (apply (curry circle-hits-rect? (get-component e2 posn?) (cc-r (get-component e2 cc?))) rect-points))
  
 
 
  (cond [(and (get-component e1 bb?) (get-component e2 bb?))
         (rect-case)]
         [(and (get-component e1 cc?) (get-component e2 bb?))
          (circle-rect-case-1)]
         [(and (get-component e2 cc?) (get-component e1 bb?))
          (circle-rect-case-2)]
         [(and (get-component e2 cc?) (get-component e1 cc?))
          (circle-hits-circle? (get-component e2 posn?) (cc-r (get-component e2 cc?)) (get-component e1 posn?) (cc-r (get-component e1 cc?)))])
)


(struct physical-collider ())

(provide (struct-out on-collide))

(struct on-collide (name func))

(define (update-on-collide g e c)
  (if (is-colliding-with? (on-collide-name c) g e)
      ((on-collide-func c) g e)
      e))

(new-component on-collide?
               update-on-collide)



(provide (struct-out static))

(struct static ())

(define (handler-identity g e c)
  e)

(new-component static?
               handler-identity) 

;Input


(define-syntax (define-all-buttons stx)
  (syntax-case stx ()
    [(_ (button-states up-f down-f button-down? button-up? button-change-down? button-change-up?)
        (keys ...))
     (with-syntax [(test 42)]
       #`(begin
           (provide button-states)
           
           (define button-states
             (make-hash (list
                         (cons (format "~a" 'keys) #f)
                         ...)))
           
           (define (button-state-set key val)
             (hash-set! button-states (format "~a" key) val)
             button-states)

           (provide button-down?)
           (define (button-down? key g)
             (define h (game-input g))
             (hash-ref h (format "~a" key) #f))

           (provide button-up?)
           (define (button-up? key g)
             (not (button-down? key g)))
           
           (provide button-change-down?)
           (define (button-change-down? key g)
             (define prev-h (game-prev-input g))
             (and
              (button-down? key g)
              (not (hash-ref prev-h (format "~a" key) #f))))

           (provide button-change-up?)
           (define (button-change-up? key g)
             (define prev-h (game-prev-input g))
             (and
              (button-up? key g)
              (hash-ref prev-h (format "~a" key) #f)))

           (define (convert-special a-key)
             (cond [(string=? a-key "\b") "backspace"]
                   [(string=? a-key "\n") "enter"]
                   [(string=? a-key "\r") "enter"]
                   [(string=? a-key " ") "space"]
                   [else a-key]))
           
           ;Consumes a world, handles a single key PRESS by setting button state to true and returning the world
           (define (down-f larger-state a-key)

             (define key-name
               (convert-special a-key))

             (define btn-states (game-input larger-state))
             
             (set-game-input! larger-state
                              (cond                       
                                [(string=? key-name (format "~a" 'keys))  (button-state-set 'keys #t)]
                                ...
                                [else btn-states]))
             larger-state)

           ;Consumes a world, handles a single key RELEASE by setting button state to false and returning the world
           (define (up-f larger-state a-key)

             (define key-name
               (convert-special a-key))

             (define btn-states (game-input larger-state))
             
             (set-game-input! larger-state
                              (cond
                                [(string=? key-name (format "~a" 'keys))  (button-state-set 'keys #f)]
                                ...
                                [else btn-states]))
             larger-state)))]))


(define-all-buttons
  (button-states handle-key-up handle-key-down button-down? button-up? button-change-down? button-change-up?)
  (left right up down wheel-up wheel-down
   rshift lshift
   backspace
   enter
   space
   a b c d e f g h i j k l m n o p q r s t u v w x y z
   A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
   0 1 2 3 4 5 6 7 8 9
   * - / + = < >
   |#| : |,| |.| | |
   |"| |'|
   |]| |[|
))









(struct game (entities
             [input #:mutable]
             [prev-input #:mutable]
             [collisions #:mutable]) #:transparent)

(define (set-game-state g s)
  (game s
        (game-input g)))





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
  (define (not-hidden e) (and (not (get-component e hidden?))
                              (not (get-component e disabled?))))
  (define entities (filter not-hidden (game-entities g)))
  (draw-entities entities))



 
(define (is-colliding? e g)
  (findf (curry member e) (game-collisions g)))

(define (is-colliding-with? name g me)
  (define names (map get-name (colliding-with me g)))
  (member name names))

(define (is-colliding-by-name? name1 name2 g)
  (define names (map (curry map get-name) (game-collisions g)))
  (or (member (list name1 name2) names)
      (member (list name2 name1) names)))

(define (non-disabled-physical-entity? e)
  ((and/c (curryr get-component physical-collider?)
         (not/c (curryr get-component disabled?)))
   e))

(define (colliding-with-other-physical-colliders? g e)
  (and (get-component e physical-collider?)
       (not (get-component e disabled?))
       (findf non-disabled-physical-entity?
              (colliding-with e g))))

(define (entity-eq? e1 e2)
  (= (get-id e1) 
     (get-id e2)))

(define (extract-out e l)
  (define (not-eq? e o)
    (not (entity-eq? e o)))
  (filter (curry not-eq? e) l))

(define (colliding-with e g)
  (flatten
   (map (curry extract-out e)
        (filter (curry member e) (game-collisions g)))))

(define/contract (main-tick-component g e c)
  (-> any/c any/c any/c entity?)
  (define handler (get-handler-for-component c))
  (if (and handler
           (or (not (get-component e disabled?))
               (active-on-bg? c)))
      (handler g e c)
      e))

(define/contract (tick-component g e c)
  (-> any/c any/c any/c entity?)
  (define next-entity-state (main-tick-component g e c))
  next-entity-state)

(define/contract (main-tick-entity g e)
  (-> any/c any/c entity?)
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

(define last-physics-snapshot #f)
(define last-game-snapshot    #f)

(define (copy-collisions-from from to)
  (if (not from)
      to
      (struct-copy game to
                   [collisions (game-collisions from)])))


(define (store-prev-input g)
  (set-game-prev-input! g (hash-copy (game-input g)))
  g)

(define (physics-tick)
  (and last-game-snapshot
       (set! last-physics-snapshot last-game-snapshot))
  (update-collisions last-physics-snapshot)
  (sleep 0.1)
  (physics-tick))

(define tick
  (lambda (g)
    (define new-game (~> g
                         ;(copy-collisions-from last-physics-snapshot _)
                         tick-entities
                         handle-dead
                         do-game-functions
                         store-prev-input))
    ;(set-game-input! g '())
   
    (set! last-game-snapshot new-game)
    new-game))

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


(define (is-static? e)
  (get-component e static?))

(define (is-disabled? e)
  (get-component e disabled?))

(define (is-static-and-not-disabled? e)
  (and (is-static? e)
       (not (is-disabled? e))))

(define (non-static? e)
  (not (is-static? e)))

(define (not-static-and-not-disabled? e)
  (and (not (is-static? e))
       (not (is-disabled? e))))

(define (not-both-static e-pair)
  (not (and (is-static? (first e-pair))
            (is-static? (second e-pair)))))

(define (collidable-pairs g)
  (define normal (filter not-static-and-not-disabled? (game-entities g)))
  (define static (filter is-static-and-not-disabled? (game-entities g)))
  (define normal-pairs (combinations normal 2))

  (define off-pairs (cartesian-product normal static))

  (append off-pairs normal-pairs))

(define (current-collisions g)
  (filter (curry apply touching?)
          (collidable-pairs g)))

(define (update-collisions g)
  (set-game-collisions! g (current-collisions g))
  g)


;DEAD ENTITIES

(struct dead ())

(define (die g e)
  (add-component e (dead)))

;END DEAD ENTITIES

;HIDDEN

(struct hidden ())

;END HIDDEN

;HIDDEN

(struct disabled ())

;END HIDDEN


; ACTIVE ENTITIES

(struct active-on-bg (bg-list))

; END ACTIVE

(define (game-width g)
  (image-width (draw g)))


(define (game-height g)
  (image-height (draw g)))


(define (physics-start larger-state)
  (set! last-physics-snapshot larger-state)
  (thread
    physics-tick))

(define (start-game . initial-world)
  (define larger-state (game (flatten initial-world)
                             button-states
                             button-states
                             '()))
  (displayln "Physics start")
  (physics-start larger-state)
  (displayln "Game start")
  (final-state
   (lux-start larger-state)))


(define (final-state d)
  (demo-state d))




;In parallel, I'm doing the controller stuff with Lux.
;  We might switch to that later (instead of 2htdp/universe)

(require racket/match
         racket/fixnum         
         lux
         lux/chaos/gui
         lux/chaos/gui/val
         (prefix-in lux: lux/chaos/gui/key)
         (prefix-in lux: lux/chaos/gui/mouse))

(struct demo
  (g/v state)
  #:methods gen:word
  [(define (word-fps w)
     60.0)
   
   (define (word-label s ft)
     (lux-standard-label "Values" ft))
   
   (define (word-output w)
     (match-define (demo g/v state) w)
     (g/v (draw state)))
   
   (define (word-event w e)
     (match-define (demo g/v state) w)
     (define closed? #f)
     (cond
      [(eq? e 'close)
       #f]
      [(lux:key-event? e)

       
       (if (not (eq? 'release (send e get-key-code)))
           (demo g/v (handle-key-down state (format "~a" (send e get-key-code))))
           (demo g/v (handle-key-up state (format "~a" (send e get-key-release-code)))))
         
       ]
      [else
       (demo g/v state)]))
   
   (define (word-tick w)
     (match-define (demo g/v state) w)
     (demo g/v (tick state)))])

(define (lux-start larger-state)
  (call-with-chaos
   (make-gui #:start-fullscreen? #f)
   (λ () (fiat-lux (demo (make-gui/val) larger-state)))))

 
(define (change-name name)
  (lambda (g e)
    (update-entity e entity-name? (entity-name name))))
