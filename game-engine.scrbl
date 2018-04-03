#lang scribble/manual
 
@title{Game Engine}
 
This is a game engine built for rapid prototyping and education.

In this engine, a game is a collection of entities. This means that ultimatley, you're
going to create a game by running a command like this:

@racketblock[(start-game entity1
                         entity2
                         entity3)]

Naturally, you'll need to build (or import) those entities.  This package provides
a DSL for specifying the components that define those entities.

Here's a full example:

@racketblock[
(require game-engine
         game-engine-demos-common) 

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (rectangle WIDTH HEIGHT "solid" "black")
                  #:name     "bg"
                  #:position (posn 0 0)))

(define spaceship-entity
  (sprite->entity (circle 20 "solid" "red")
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)))


(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            spaceship-entity
            bg-entity)
] 

The @racket[sprite->entity] function is the powertool for converting from an image to an entity,
allowing you to specify components in the process.

@defproc[(sprite->entity [imgs        (or/c image? (listof image?))]
                         [#:name       name       string?]
                         [#:position   position   posn?]
                         [#:components components component?] ...
)
         entity?]{
 
  Converts @racket[imgs] (which may be a single image or a list of images) into an entity.
  You may also specify a name, position, and any number of components.
}


Conceptually speaking, I like to think of entities as a combination of two things: art and game logic.
The art is specified by the sprite; the game logic is specified by the collection of components.

NOTE: Components can also be added, removed, and updated during runtime.  

The next two sections look at 1) the various components that ship with this library, and 2) the 
functions that allow "slicing" sprite sheets into lists of images (for animation).

@section{Components}

@defproc[(component? [x any/c])
         boolean?]{
 
  There is no data structure called "component".  A component is anything that has been previously registered
  with the engine by calling @racket[new-component].

  Conceptually speaking, a component is 1) some struct, and 2) an update function.

  If you're just now embarking on your journey to learn about game design, I would recommend sticking to the
  components provided by others.  This engine is structured so that you don't need to worry too much about
  how the components work or how to make new components.  
}

That said, here's another function for creating new components (which you can also safely ignore if you're
just beginning).

@defproc[(new-component [struct? (-> any/c boolean?)]
                        [update  (-> game? entity? component? entity?)])
         void?]{
 
  To register a new component, you must use this function.  You must provide two
  functions.  1) a way of recognizing your component, 2) a handler function that
  the engine will call on every tick, for every entity that has this component.

  This handler function returns an entity, which will replace the entity that has
  the component.  The handler function also receives the game as an input.  In effect,
  this means your handler function has read-only access to the entire game state.
  And it has "write" access to the entity possessing the component.  

  I would recommend looking at some of the existing components (i.e. @racket[key-movement])
  if you're going to make new components of your own.
}

@subsection{Pre-built Components}

These components are design to allow you to add behaviour to entities with as little effort as possible.
This engine is designed for rapid prototyping -- which means that you should be able to experiment with
a lot of different game design ideas very quickly.  You do that by using pre-built components as the
fundamental building blocks for your game.


@defproc[(key-movement [speed integer?])
         component?]{
 
  An entity with this component will move when you press the arrow keys.  The speed parameter
  changes how quickly the entity moves.

  Technically speaking, this component updates the entity's @racket[posn] component whenever
  the arrow keys are pressed.

  Example usage:

   @racketblock[
    (define spaceship-entity
      (sprite->entity (circle 20 "solid" "red")
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (key-movement 5)))
    ]

}

@defproc[(posn [x integer?]
               [y integer?])
         component?]{
 
  Determines where the entity will render.  If you're using @racket[sprite->entity], then
  you should specify this with the @racket[#:name] keyword, not the @racket[#:components]
  keyword.

  Example:

   @racketblock[
    (define spaceship-entity
      (sprite->entity (circle 20 "solid" "red")
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (key-movement 5)))
    ]
}

@defproc[(after-time [ticks integer?]
                     [fun (-> game? entity? component? entity?)])
         component?]{
 
  Runs a one-time handler function after the specified number of ticks.

  Example:

  @racketblock[
    (define bullet
      (sprite->entity bullet-sprite 
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-left #:min   0
                                                          #:speed 5))
                                   (after-time 50     die)))
  ] 
}

@defproc[(on-collide [name string?]
                     [fun (-> game? entity? component? entity?)])
         component?]{
 
  Runs a handler function when this entity touches the named entity

  Example:

  @racketblock[
    (define spaceship-entity
      (sprite->entity spaceship-sprite
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (key-movement 5)
                                   (on-collide "ore"    (change-speed-by 1))
                                   (on-collide "enemy"  die)
                                   (on-collide "bullet" die)))
  ] 
}

@defproc[(circle-collider [radius number?])
         component?]{
 
  Changes out the default rect-collider for a circle collider with the specified radius

  Example (make an entity have circular collision detection with a radius of 2):

  @racketblock[
    (define spaceship-entity
      (sprite->entity spaceship-sprite
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (key-movement 5)
                                   (on-collide "ore"    (change-speed-by 1))
                                   (circle-collider 2)))
  ] 
}

@defproc[(health     [amount integer?])
         component?]{
 
  Kills this entity when (if) the health reaches 0.

  Example:

  @racketblock[
    (define spaceship-entity
      (sprite->entity spaceship-sprite
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (health 5)
                                   (on-collide "bullet" (change-health -1))))
  ] 
}

@defproc[(physical-collider)
         component?]{
 
  Two entities with physical colliders will not pass through each other.
  This overrides things like key-handler.
}



@section{Sprites}

With sprites, we assume that you have a sprite "sheet" -- which is a single image that
displays multiple frames of a sprite's animation.

We give a function called @racket[sheet->sprite] to ease the conversion of such a sheet
into a sprite (which can then be turned into an entity).

Example:

@racketblock[
  (define spaceship-sheet (bitmap/url "http://i.imgur.com/8zY5sBR.png"))
  (define spaceship-sprite
    (sheet->sprite spaceship-sheet
                   #:rows        4
                   #:columns     3
                   #:row-number  3
                   #:speed       1))
]

The basic workflow is: make/find a sheet, turn the sheet into a sprite, turn the sprite into
an entity, add components to the entity.  And repeat.

The game ends up being a collection of entities that interact with each other and the player
according to their components.


@section{Examples}

These are taken from the package @racket[spaceship-game-demo].  You can find more there.


@subsection{Spaceship Demo 1}

@racketblock[

(require game-engine
         game-engine-demos-common) 

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define (spaceship-entity)
  (sprite->entity spaceship-sprite
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)
                               (on-collide "ore"    (change-speed-by 1))
                               (on-collide "enemy"  die)
                               (on-collide "bullet" die)))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" (randomly-relocate-me 0 WIDTH 0 HEIGHT))))

(define (enemy-entity p)
  (sprite->entity (spaceship-animator 'left)
                  #:position    p
                  #:name        "enemy"
                  #:components  (every-tick (move-up-and-down #:min   0  
                                                              #:max   HEIGHT
                                                              #:speed 10))
                                (spawner bullet 20)))

(define bullet
  (sprite->entity (new-sprite (list (circle 5 "solid" "red")
                                    (circle 5 "solid" "orange")
                                    (circle 5 "solid" "yellow")
                                    (circle 5 "solid" "orange")) 1)
                  #:position   (posn 100 100)
                  #:name       "bullet"
                  #:components (every-tick (move-left #:min   0
                                                      #:speed 5))
                               (after-time 50     die)  
                               (on-collide "ship" die)))

(define (lost? g e)
  (not (get-entity "ship" g)))

(define (won? g e) 
  (define speed (get-speed (get-entity "ship" g)))
  (>= speed 10))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (game-over-screen won? lost?)
            (spaceship-entity)
            (ore-entity (posn 400 400))
            (enemy-entity (posn 300 300))
            (enemy-entity (posn 400 200))
            bg-entity)
]

@subsection{Spaceship Demo 2}

@racketblock[

(require game-engine
         game-engine-demos-common) 
  
(define WIDTH  800)
(define HEIGHT 800)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define (spaceship-entity)
  (sprite->entity spaceship-sprite
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)
                               (on-collide "ore"    (change-speed-by 1))
                               (on-collide "enemy"  die)
                               (on-collide "bullet" die)))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" (randomly-relocate-me 0 WIDTH 0 HEIGHT))))

(define (enemy-entity p)
  (sprite->entity (spaceship-animator 'left)
                  #:position    p
                  #:name        "enemy"
                  #:components  (every-tick (move-up-and-down #:min   0  
                                                              #:max   HEIGHT
                                                              #:speed 10))
                                (spawner bullet 20)))

(define bullet
  (sprite->entity (new-sprite (list (circle 5 "solid" "red")
                                    (circle 5 "solid" "orange")
                                    (circle 5 "solid" "yellow")
                                    (circle 5 "solid" "orange")) 1)
                  #:position   (posn 100 100)
                  #:name       "bullet"
                  #:components (every-tick (move-left #:min   0
                                                      #:speed 5))
                               (after-time 50     die)  
                               (on-collide "ship" die)))

(define (lost? g e)
  (not (get-entity "ship" g)))

(define (won? g e) 
  (define speed (get-speed (get-entity "ship" g)))
  (>= speed 10))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (game-over-screen won? lost?)
            (spaceship-entity)
            (ore-entity (posn 400 400))
            (enemy-entity (posn 300 300))
            (enemy-entity (posn 400 200))
            (enemy-entity (posn 500 100))
            bg-entity)
]


@subsection{Spaceship Demo 3}

@racketblock[

(require game-engine
         game-engine-demos-common) 

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:name     "bg"
                  #:position (posn 0 0)))

(define (spaceship-entity)
  (sprite->entity spaceship-sprite
                  #:name       "ship"
                  #:position   (posn 100 100)
                  #:components (key-movement 5)
                               (on-collide "ore"    (change-speed-by 1))
                               (on-collide "enemy"  die)
                               (on-collide "bullet" die)))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components (on-collide "ship" (randomly-relocate-me 0 WIDTH 0 HEIGHT))))

(define (enemy-entity p)
  (sprite->entity (spaceship-animator 'left)
                  #:position    p
                  #:name        "enemy"
                  #:components  (every-tick (move-up-and-down #:min   0  
                                                              #:max   HEIGHT
                                                              #:speed 10))
                                (spawner bullet 20)))

(define bullet2
  (sprite->entity (new-sprite (list (circle 2 "solid" "red")
                                    (circle 2 "solid" "orange")
                                    (circle 2 "solid" "yellow")
                                    (circle 2 "solid" "orange")) 1)
                  #:position   (posn 100 100)
                  #:name       "bullet"
                  #:components (every-tick (move-left #:min   0
                                                      #:speed 10))
                               (after-time 20     die)  
                               (on-collide "ship" die)))

(define bullet
  (sprite->entity (sprite-map (lambda (i)
                                (scale 0.35 i)) (spaceship-animator 'left))
                  #:position   (posn 100 100)
                  #:name       "bullet"
                  #:components (every-tick (move-left #:min   0
                                                      #:speed 5))
                               (after-time 75     die)  
                               (on-collide "ship" die)
                               (spawner bullet2 10)))

(define (lost? g e)
  (not (get-entity "ship" g)))

(define (won? g e) 
  (define speed (get-speed (get-entity "ship" g)))
  (>= speed 10))

(start-game (instructions WIDTH HEIGHT "Use arrow keys to move")
            (game-over-screen won? lost?)
            (spaceship-entity)
            (ore-entity (posn 400 400))
            (enemy-entity (posn 500 300))
            bg-entity)

]

