#lang racket 
(provide 
  get-physics-colliding?
  get-physics-colliding-with

  get-physics-position
  get-physics-rotation
  get-velocity
  physics-manager
  (rename-out [make-physics-system physics-system]))

(require "../../core/main.rkt" 
         "../common-components/main.rkt"
         posn threading)

(require (prefix-in chip: racket-chipmunk/lang/chipmunk-ffi))

(require ffi/unsafe)

(define (number->pointer n) 
  (define ptr 
    (malloc 'raw 2)) 
  (ptr-set! ptr _uint n) 

  ptr) 

(define-component physics-system entity?)
(define-component chipmunk any/c)

(define-component desired-force posn?)
(define-component desired-velocity posn?)
(define-component force posn?)

(define-component velocity posn?)

(define-component colliding-with list?)
(define-component colliding? boolean?)

(define (make-physics-system #:forces (forces (const #f))
                             #:velocities (velocities (const #f))
                             #:mass (mass 1)
                             #:static (static #f)
                             #:sensor (sensor #f)
                             #:type (type #f)
                             w h)
  (define the-chipmunk-hook
    (chipmunk
      #f
      (init-or-update-chipmunk w h mass static sensor type)))

  (define shadow-entity
    (entity 
        (name #f)
        (physics-world #f) 

        (desired-force #f 
                       (forces))

        (desired-velocity #f 
                          (velocities))

        ;What is slowing things down in here?

        the-chipmunk-hook	

        (colliding-with '() 

                        (map second
                             (filter 
                               (lambda (c)
                                 (eq? (first c)
                                      (entity-id (CURRENT-ENTITY)))) 
                               (get 'physics-manager 'collisions))))


	(force #f
               (chipmunk-force)) 

	(velocity #f 
                  (chipmunk-velocity)) 

	(position #f 
                  (chipmunk-posn)) 

	(rotation #f 
                  (chipmunk-rotation))))

  (list
    (physics-system 
      shadow-entity 
      ;Ultimately: this is slow.  WHat part?
      (update-physics-system))))



(define (update-physics-system)
  (define ps (get-physics-system))

  ;We want to tick the physics system,
  ;but first we make sure certain things are properly
  ;initialized from the parent -- space, position, rotation, etc.

  (define with-space 
    (init-child-from-parent 
      ps 
      get-physics-world 
      set-physics-world
      (get 'physics-manager 'physics-world)))

  (define with-position
    (init-child-from-parent 
      with-space
      get-position
      set-position))

  (define with-rotation
    (init-child-from-parent 
      with-position
      (lambda (e) 
        (get-rotation e 0))
      set-rotation))
  
  (define with-name
    (init-child-from-parent 
      with-rotation
      (lambda (e) 
        (get-name e (name #f)))
      set-name))

  (tick-entity with-name))

;TODO: Move this out if it turns out to be a common pattern
(define (init-child-from-parent child 
                                getter 
                                setter 
                                (value getter))

  (if (not (getter child))
    (setter child (if (procedure? value)
                    ;Usually just runs the getter with no args -- applies to the parent, which is the (CURRENT-ENTITY) 
                    (value (CURRENT-ENTITY)) 
                    value))
    child))


(define (init-or-update-chipmunk w h m static? sensor? type)
  (define current (get-chipmunk)) 


  (if (not current)
    (init-chipmunk w h m static? sensor? type) 

    (~> current
        copy-in-desired-force
        copy-in-desired-velocity)))

(define (copy-in-desired-force c)
  (if (get-desired-force) 
      (begin
        (chip:cpBodySetForce c
                             (chip:cpv (posn-x (get-desired-force))
                                       (posn-y (get-desired-force))))
        c)
      c))

(define (copy-in-desired-velocity c)
  (if (get-desired-velocity) 
    (begin
      (chip:cpBodySetVelocity c
                              (chip:cpv (posn-x (get-desired-velocity))
                                        (posn-y (get-desired-velocity))))
      c)
    c))

  
(define (init-chipmunk w h m static? sensor? type)

  (define p (get-position))
  (match-define (posn x y) p)

  (define space
    (get-physics-world))


  (define mass (real->double-flonum m))
  (define moment (chip:cpMomentForBox mass 
                                      (real->double-flonum w) 
                                      (real->double-flonum h)))

  (define body 
    (chip:cpSpaceAddBody space 
                         (if static?
                           (chip:cpBodyNewStatic)
                           (chip:cpBodyNew mass moment))))

  (chip:cpBodySetPosition body 
			  (chip:cpv x y))


  (define r (get-rotation))
  
  (when r
    (chip:cpBodySetAngle body (real->double-flonum r)))


  (define shape (chip:cpSpaceAddShape space 
				      (chip:cpBoxShapeNew body
							  (real->double-flonum w)
							  (real->double-flonum h)
							  (chip:cpv 0.0 0.0))))

  (when sensor?
    (chip:cpShapeSetSensor shape 1))

  (when type
    (chip:cpShapeSetCollisionType shape type))

  (define e-id (entity-id (CURRENT-ENTITY)))

  (define e-pointer (number->pointer e-id))

  (chip:cpShapeSetUserData shape e-pointer)

  body)


                                                     

(define (chipmunk-x c)
  (define p (chip:cpBodyGetPosition c))
  (chip:cpVect-x p))

(define (chipmunk-y c)
  (define p (chip:cpBodyGetPosition c))
  (chip:cpVect-y p))

(define (chipmunk-vx c)
  (define p (chip:cpBodyGetVelocity c))
  (chip:cpVect-x p))

(define (chipmunk-vy c)
  (define p (chip:cpBodyGetVelocity c))
  (chip:cpVect-y p))

(define (chipmunk-fx c)
  (define p (chip:cpBodyGetForce c))
  (chip:cpVect-x p))

(define (chipmunk-fy c)
  (define p (chip:cpBodyGetForce c))
  (chip:cpVect-y p))

(define (chipmunk-force (c (get-chipmunk)))
  (posn
    (chipmunk-fx c)
    (chipmunk-fy c)))

(define (chipmunk-velocity (c (get-chipmunk)))
  (posn
    (chipmunk-vx c)
    (chipmunk-vy c)))

(define (chipmunk-posn (c (get-chipmunk)))
  (posn
    (chipmunk-x c)
    (chipmunk-y c)))

(define (chipmunk-rotation (c (get-chipmunk)))
  (chip:cpBodyGetAngle c))

(define (get-physics-position)
  (define p 
     (get-physics-system))

  (define c
    (get-chipmunk p))

  (if c 
      (chipmunk-posn c)
      (get-position)))

(define (get-physics-rotation)
  (define p 
     (get-physics-system))

  (define c
    (get-chipmunk p))

  (if c 
      (chipmunk-rotation c)
      (get-rotation)))

(define (get-physics-colliding? . qs)

  (define collision-ids (or (get-physics-colliding-with)
                            '()))

  (define (run-query q id)
    (define e (get-entity (CURRENT-GAME)
                          (curryr entity-id=? id)))
    (q e))

  (define (run-queries id)
    (lambda (id)
      (ormap
        (curryr run-query id) 
        qs)))


  (not (empty?
         (filter
           run-queries
           collision-ids))))

(define (get-physics-colliding-with)
  (define p (get-physics-system))
  (define c (get-colliding-with p))

  c)


(define-component physics-world any/c)
(define-component collisions  (listof number?))
(define-component separations (listof number?))

;Note, this seems like it leaks memory whenever a game links into a new game with a physics-manager.  Its our job to protect against that stuff, so we need to provide safer abstractions.

(define (physics-manager )

  (define space (new-physics-space))

  (entity
    (name 'physics-manager)
    (collisions  '() 
                 my-collisions)

    (separations '() 
                 (let ([ret my-separations])
                       (set! my-separations '()) 
                       ret))
    (physics-world space
                   (begin
                     (chip:cpSpaceStep (get-physics-world) 
                                       (/ 1 120.0))
                     space)) 
    ))


(define my-collisions  '())
(define my-separations '())

(define (new-physics-space)
  (define space (chip:cpSpaceNew))

  (define (begin-callback arbiter space data)

    (let-values ([(s1 s2)  (chip:cpArbiterGetShapes arbiter)])

      (define s1-id 
        (ptr-ref
          (chip:cpShapeGetUserData s1)
          _uint))
      (define s2-id 
        (ptr-ref
          (chip:cpShapeGetUserData s2)
          _uint)) 

      (set! my-collisions 
        (remove-duplicates
          (cons (list s1-id s2-id) my-collisions))))

    1)

  (define (separate-callback arbiter space data)

    (let-values ([(s1 s2)  (chip:cpArbiterGetShapes arbiter)])

      (define s1-id 
        (ptr-ref
          (chip:cpShapeGetUserData s1)
          _uint))

      (define s2-id 
        (ptr-ref
          (chip:cpShapeGetUserData s2)
          _uint))

      (set! my-collisions
        (filter-not
          (curry equal? (list s1-id s2-id))    
          my-collisions))
      
      (set! my-separations 
        (cons (list s1-id s2-id) my-separations)))

    1)

  (define (presolve-callback arbiter space data)
    1)

  (define (postsolve-callback arbiter space data)
    
    1)

  (define handler (chip:cpSpaceAddDefaultCollisionHandler space))

  (chip:set-cpCollisionHandler-cpCollisionBeginFunc! 
    handler 
    begin-callback)

  (chip:set-cpCollisionHandler-cpCollisionPreSolveFunc! 
    handler 
    presolve-callback)

  (chip:set-cpCollisionHandler-cpCollisionPostSolveFunc! 
    handler 
    postsolve-callback)

  (chip:set-cpCollisionHandler-cpCollisionSeparateFunc! 
    handler 
    separate-callback) 

  space)



