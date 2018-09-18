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
         
         entity-animation
         sprite->entity
         sprite->bb
         update-entity
         get-component
         get-components
         add-component 
         remove-component
         add-components
         get-name
         change-name
         basic-entity
         dead
         die
         get-entity

         colliding-with
         is-colliding-with?
         is-colliding-by-name?

         
         image->bb
         
         (rename-out [make-physical-collider physical-collider])
         
         start-game

         set-game-state

         draw
         game-width
         game-height
         width
         height
         set-posn
         get-posn
         x y

         component-is?
         set-velocity)

(require posn)
(require 2htdp/image)
(require 2htdp/universe)
(require "./components/animated-sprite.rkt")
(require (prefix-in phys: racket-chipmunk))

(require threading)

(require (for-syntax racket/syntax))

(struct bb [w h])

(define (width e)
  (bb-w (get-component e bb?)))

(define (height e)
  (bb-h (get-component e bb?)))

(define (set-posn e p)
  (~> e
      (update-entity _ posn? p)
      ;We don't need this apparently??
      #;(set-chipmunk-posn! _ p)))

(define (get-posn e)
  (get-component e posn?))

(define (x e)
  (posn-x (get-posn e)))

(define (y e)
  (posn-y (get-posn e)))

(define (w e)
  (bb-w (get-component e bb?)))

(define (h e)
  (bb-h (get-component e bb?)))

(struct id [id])


(struct entity [components] #:transparent)

(struct entity-name (string) #:transparent)


(define (entity-eq? e1 e2)
  (= (get-id e1)
     (get-id e2)))

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

(define (component-is? c)
  (λ(x)
    (eq? x c)))

(define (add-or-update-component e pred? val)
  (add-component (remove-component e pred?)
                 val))

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
(define #;/contract (get-component e maybe-pred)
  #;(-> entity? any/c any/c)
  (define component-pred
    (if (procedure? maybe-pred)
        maybe-pred
        (lambda (c)
          (eq? c maybe-pred))))
  (findf component-pred (entity-components e)))

(define #;/contract (get-components e component-pred)
  #;(-> entity? any/c any/c)

  (filter component-pred (entity-components e)))

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
         all-cs))

(define (sprite->bb s)
  (image->bb (render s)))

(define (image->bb i)
  (bb
   (image-width i)
   (image-height i)))



(struct physical-collider (chipmunk force) #:transparent)

(define (set-chipmunk-posn! e p)
  (and (entity->chipmunk e)
       (phys:cpBodySetPosition (phys:chipmunk-body (entity->chipmunk e))
                               (phys:cpv (posn-x p)
                                         (posn-y p))))
  e)

(define (entity->chipmunk e)
  (define pc (get-component e physical-collider?))
  (if pc
      (physical-collider-chipmunk pc)
      #f))

(define (make-physical-collider)
  (physical-collider #f (posn 0 0)))

(provide (struct-out on-collide))

(struct on-collide (name func))

(define (update-on-collide g e c)
  (if (is-colliding-with? (on-collide-name c) g e)
      ((on-collide-func c) g e)
      e))

(define (set-velocity e p)
  (update-entity e physical-collider?
                 (struct-copy physical-collider (get-component e physical-collider?)
                              [force p])))



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









(struct game ([entities #:mutable]
             [input #:mutable]
             [prev-input #:mutable]
             [collisions #:mutable]) #:transparent)

(define (set-game-state g s)
  (game s
        (game-input g)))





(define (draw-entity e)
  (define s (get-component e animated-sprite?))
  (if s
      (render s)
      empty-image))

(define (draw-entities es)
  (if (= 1 (length es))
      (draw-entity (first es))
      (let* ([p (get-component (first es) posn?)]
             [x (posn-x p)]
             [y (posn-y p)])
        (place-image (draw-entity (first es))
                     x y
                     (draw-entities (rest es))))))


(define #;/contract (draw g)
  #;(-> game? image?)
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

(define (extract-out e l)
  (define (not-eq? e o)
    (not (= (get-id e)
            (get-id o))))
  (filter (curry not-eq? e) l))

(define (colliding-with e g)
  (flatten
   (map (curry extract-out e)
        (filter (curry member e) (game-collisions g)))))

(define #;/contract (main-tick-component g e c)
  #;(-> any/c any/c any/c entity?)
  (define handler (get-handler-for-component c))
  (if (and handler
           (or (not (get-component e disabled?))
               (active-on-bg? c)))
      (handler g e c)
      e))

(define #;/contract (tick-component g e c)
  #;(-> any/c any/c any/c entity?)
  (define next-entity-state (main-tick-component g e c))
  next-entity-state)

(define #;/contract (main-tick-entity g e)
  #;(-> any/c any/c entity?)
  (foldl
   (lambda (c e2)
     (tick-component g e2 c))
   e
   (entity-components e)))


(define debug-circle
  (sheet->sprite (circle 10 'solid 'red)
                  #:rows        1
                  #:columns     1
                  #:row-number  1
                  #:speed       0))

(define (debug-highlight e)
  (add-component (remove-component e animated-sprite?)
                 debug-circle))


(struct previous (state))

(define (previous-entity e)
  (define p (get-component e previous?))
  (if p
      (previous-state p)
      #f))



(define (tick-entity g e)
  (main-tick-entity g e))

(define (tick-entities g)
  (define es (game-entities g))
  (struct-copy game g
               [entities (map (curry tick-entity g) es)]))


(define (do-game-functions g)
  (define pipeline (lambda (a-fun a-game)
                     (a-fun a-game)))
  (foldl pipeline g game-functions))




(define (store-prev-input g)
  (set-game-prev-input! g (hash-copy (game-input g))) 
  g)


(require (prefix-in h:   lang/posn))
(require (prefix-in ffi: ffi/unsafe))

(define *tick-rate* (/ 1 120.0))


(define (physics-tick g)  
  (phys:step-chipmunk *tick-rate*)

  (define (physics-tick-entity e)
    (define pc (get-component e physical-collider?))
    
    (if (not pc)
        e
        (let ([f (physical-collider-force pc)]
              [c (physical-collider-chipmunk pc)])

          (define new-pos (posn (phys:x c)
                                (phys:y c)))

          (update-entity e posn?
                         new-pos))))

  (define new-es (map physics-tick-entity (game-entities g)))
  
  (struct-copy game g
               [entities new-es]))



(define (chipmunkify e)
  (define pc (get-component e physical-collider?))
  (if (not pc)
      e
      (let ([box ((if (get-component e static?) phys:box-kinematic phys:box)
                  (w e)
                  (h e)
                  (phys:cpv (x e)
                            (y e))
                  #:group (if (get-component e static?) 2 1))])


        (update-entity e physical-collider?
                       (physical-collider box (posn 0 0))))))



;The stupid macro....
(require (for-syntax racket))
(define-syntax (dumb-duplicate stx)
  (syntax-case stx ()
    ((_ id body n)
     #`(define id
         (list
          #,@(map (λ(x) #'body)
                  (range 0 (syntax->datum #'n))))))))

(define (find-entity-by-chipmunk-body b g)
  (define (has-body? b e-b)
    (ffi:ptr-equal? b (second e-b)))

  (define e-b
    (findf (curry has-body? b)
            entities-and-bodies))
  
  (define original-e
    (if e-b (first e-b) #f))

  (findf (λ(current-e)
           (entity-eq? current-e original-e)) (game-entities g)))


(define entities-and-bodies '())

(define last-game-snapshot #f)

(define (physics-start g)
  (displayln "Physics start")
  

  (define new-es (map chipmunkify (game-entities g)))

  (define (vel-func body gravity damping dt)
    (ffi:cpointer-push-tag! body 'cpBody)

    (define e (find-entity-by-chipmunk-body body last-game-snapshot))

    (and e
         (get-component e physical-collider?)
         (phys:cpBodySetVelocity body
                                 (phys:cpv (posn-x (physical-collider-force (get-component e physical-collider?)))
                                           (posn-y (physical-collider-force (get-component e physical-collider?))))))
    ffi:_void)

  (dumb-duplicate fs
                  (λ(a b c d)
                    (vel-func a b c d))
                  10)

  (define (set-vel-func-for-sure e)
    
    (define body (phys:chipmunk-body
                                     (physical-collider-chipmunk
                                      (get-component e physical-collider?))))

    (set! entities-and-bodies (cons (list e body)
                                    entities-and-bodies))
    
    (phys:set-cpBody-velocity_func! body
                                    (first fs))

    (set! fs (rest fs)))


  (define (set-vel-func e)
    (if (is-static? e)
        e
        (set-vel-func-for-sure e)))

  

  (define collidables (filter (λ(e) (get-component e physical-collider?)) new-es))



  (map set-vel-func collidables)

  (struct-copy game g

               ;Filtering here is weird.  It assumes that physical, static, disabled components
               ;  should not be added to the game.  Optimizes away things like hidden walls.
               ;  But it's a potential bug if you actually want that thing to become enabled at some point...
               ;Maybe safe though.  Why not just use hidden?  Can initially disabled components actually be woken up
               ;  anyway?
               [entities (filter (or/c
                                  (not/c is-static?)
                                  (not/c is-physical?)
                                  (not/c is-disabled?)) new-es)]))






(define tick
  (lambda (g)
    (set! last-game-snapshot g)
    
    (define new-game (~> g
                         physics-tick ;Just a note for the future.  This is not slow.  Do not move to a different thread in a misguided effort to optimize...
                         tick-entities
                         handle-dead
                         do-game-functions
                         store-prev-input))

    new-game))

(define (handle-dead g)
  (define is-alive? (lambda (e) (not (get-component e dead?))))
  (set-game-entities! g (filter is-alive? (game-entities g)))
  g)


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

(define (is-physical? e)
  (get-component e physical-collider?))

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



(define (start-game . initial-world)
  (define larger-state (game initial-world
                             button-states
                             button-states
                             '()))

  (define g (physics-start larger-state))

  
  (final-state
   (lux-start g)))




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
