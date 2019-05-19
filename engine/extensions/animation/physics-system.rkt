#lang racket

(provide 
  get-physics-position
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

;For shapes: cpShapeSetUserData, A user definable data pointer. If you set this to point at the game object the shapes is for, then you can access your game object from Chipmunk callbacks.



;There are two ways to set up a dynamic body. The easiest option is to create a body with a mass and moment of 0, and set the mass or density of each collision shape added to the body. Chipmunk will automatically calculate the mass, moment of inertia, and center of gravity for you. This is probably preferred in most cases.

;Maybe should take a list of "shapes"
;  Returns something equivalent to a body
;  Collision handlers are on shapes, so this would involve checking collisions across all child shapes and storing that information somewhere in the returned entity.
;  To start with, can pretend there is only one child shape.  Get it working, then expand to multiple...
(define (make-physics-system #:update (update (const #f)) 
                             #:mass (mass 1)
                             x y w h)

  ;TODO: How do we pass the space into the chipmunk for init?

  (list
    (physics-system 
      (entity 
        (physics-world #f) ;Shadow pattern

        (velocity #f (update))

	(chipmunk
	  #f
	  (init-or-update-chipmunk x y w h mass))


        ;Rotation?
	(position (posn x y)
		  (chipmunk-posn))) 

      (update-entity-with-space)
      
      #;
      (^ tick-entity))))


(define (update-entity-with-space)
  (define ps (get-physics-system))

  (define main-w 
    (physics-world (get 'physics-manager 'physics-world)))
  ;Do we really need to shove in a whole new physics world on every tick?  We just want a pointer to the space.  And we only need to shove that in once.
  (define with-space (set-physics-world ps main-w))

  (define ret
    ;TODO: tick-entity breaking because handlers are false.  Also it wasn't doing the debugger correctly -- sad.  We should refactor runtime so they share the same path.
    (tick-entity
      ;Pipe in the physics-world (thus scoping the physics per game?), and tick the entity
      with-space))

   ret)


(define (init-or-update-chipmunk x y w h m)
  (define current (get-chipmunk)) 

  (if (not current)
    (init-chipmunk x y w h m) 

    (if (and (get-velocity)

             ;Just for testing...
             (not (and (zero? (posn-x (get-velocity)))
                       (zero? (posn-y (get-velocity))))))
      (begin
        (chip:cpBodySetVelocity current
                                (chip:cpv (posn-x (get-velocity))
                                          (posn-y (get-velocity))))
        current)
      current) ))
  
(define (init-chipmunk x y w h m)
  (displayln "Making a chipmunk, which isn't doing much atm")

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

(define (get-physics-position)
  (define p 
     (get-physics-system))

  (define c
    (get-chipmunk p))

  (if c 
      (chipmunk-posn c)
      (get-position)))


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


