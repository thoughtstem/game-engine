#lang racket

(provide 
  get-physics-position
  get-physics-rotation
  physics-manager
  (rename-out [make-physics-system physics-system]))

;TODO: Useful hook for "triggers":  A boolean value if this shape is a sensor or not. Sensors only call collision callbacks, and never generate real collisions.
;
;

;Do we need collision callbacks, or should each shape/body entity just call into chipmunk to see what it's colliding with on the current frame.
;  Probably does help, though.
;  There are some cool queries, but we don't want to be doing them every tick.

(require "../../core/main.rkt" 
         "./common-components.rkt"
         posn)

(require (prefix-in chip: racket-chipmunk))

(define-component physics-system entity?)
(define-component chipmunk any/c)
(define-component velocity posn?)

;Groups and category masks can make things faster by filtering out collisions before they occur.
;  Are these on bodies or shapes, btw?

;For shapes: cpShapeSetUserData, A user definable data pointer. If you set this to point at the game object the shapes is for, then you can access your game object from Chipmunk callbacks.

;There are two ways to set up a dynamic body. The easiest option is to create a body with a mass and moment of 0, and set the mass or density of each collision shape added to the body. Chipmunk will automatically calculate the mass, moment of inertia, and center of gravity for you. This is probably preferred in most cases.

;Maybe should take a list of "shapes"
;  Returns something equivalent to a body
;  Collision handlers are on shapes, so this would involve checking collisions across all child shapes and storing that information somewhere in the returned entity.
;  To start with, can pretend there is only one child shape.  Get it working, then expand to multiple...
(define (make-physics-system #:update (update (const #f)) 
                             #:mass (mass 1)
                             w h)

  (list
    (physics-system 
      (entity 
        (physics-world #f) 

        (velocity #f (update))

	(chipmunk
	  #f
	  (init-or-update-chipmunk w h mass))

	(position #f (chipmunk-posn)) 
	(rotation #f (chipmunk-rotation))) 

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
      get-rotation
      set-rotation))

  (tick-entity with-rotation))

;TODO: Move this out if it turns out to be a common pattern
(define (init-child-from-parent child 
                                getter 
                                setter 
                                (value getter))

  (if (not (getter child))
    (setter child (if (procedure? value)
                    ;Usually just runs the getter with no args -- applies to the parent, which is the (CURRENT-ENTITY) 
                    (value) 
                    value))
    child))


(define (init-or-update-chipmunk w h m)
  (define current (get-chipmunk)) 

  (if (not current)
    (init-chipmunk w h m) 

    (if (and (get-velocity)

             ;For testing whether collisions are broken or just mushy
             #; 
             (not (origin? (get-velocity)))) 
      (begin
        (chip:cpBodySetForce current
                             (chip:cpv (posn-x (get-velocity))
                                       (posn-y (get-velocity))))
        #;
        (chip:cpBodySetVelocity current
                                (chip:cpv (posn-x (get-velocity))
                                          (posn-y (get-velocity))))
        current)
      current) ))
  
(define (init-chipmunk w h m)
  (displayln "Making a chipmunk, which isn't doing much atm")

  (define p (get-position))
  (match-define (posn x y) p)

  (define space
    (get 'physics-manager 'physics-world))

  (define mass (real->double-flonum m))
  (define moment (chip:cpMomentForBox mass 
                                      (real->double-flonum w) 
                                      (real->double-flonum h)))

  (define body 
    (chip:cpSpaceAddBody space 
			 (chip:cpBodyNew mass moment)))

  (define shape (chip:cpSpaceAddShape space 
				      (chip:cpBoxShapeNew body
							  (real->double-flonum w)
							  (real->double-flonum h)
							  (chip:cpv 0.0 0.0))))

  (chip:cpBodySetPosition body 
			  (chip:cpv x y))

  (define r (get-rotation))
  
  (when r
    (chip:cpBodySetAngle body (real->double-flonum r)))

  body)

                                                     

(define (chipmunk-x c)
  (define p (chip:cpBodyGetPosition c))
  (chip:cpVect-x p))

(define (chipmunk-y c)
  (define p (chip:cpBodyGetPosition c))
  (chip:cpVect-y p))

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


(define-component physics-world any/c)

(define (physics-manager)
  (define space (chip:cpSpaceNew))

  (entity
    (name 'physics-manager)
    (physics-world space
                   (begin
                     (chip:cpSpaceStep (get-physics-world) 
                                       (/ 1 120.0))
                     space))))


