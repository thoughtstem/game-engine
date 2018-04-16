#lang scribble/manual
 
@title{Game Engine}

This is a game engine built for rapid prototyping and education.

In this engine, a game is a collection of entities. This means that ultimatley,
you're going to create a game by running a command like this:

@racketblock[(start-game entity1
                         entity2
                         entity3)]

Naturally, you'll need to build (or import) those entities.  This package
provides a DSL for specifying the components that define those entities.

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

The @racket[sprite->entity] function is the powertool for converting from an
image to an entity, allowing you to specify components in the process.

@defproc[(sprite->entity [imgs        (or/c image? (listof image?))]
                         [#:name       name       string?]
                         [#:position   position   posn?]
                         [#:components components component?] ...
)
         entity?]{
 
  Converts @racket[imgs] (which may be a single image or a list of images) into
  an entity. You may also specify a name, position, and any number of components
}


Conceptually speaking, I like to think of entities as a combination of two
things: art and game logic. The art is specified by the sprite; the game logic
is specified by the collection of components.

NOTE: Components can also be added, removed, and updated during runtime.  

The next two sections look at 1) the various components that ship with this
library, and 2) the functions that allow "slicing" sprite sheets into lists of
images (for animation).

@section{Components}

@defproc[(component? [x any/c])
         boolean?]{
 
  There is no data structure called "component".  A component is anything that
  has been previously registered with the engine by calling @racket[new-component].

  Conceptually speaking, a component is 1) some struct, and 2) an update function.

  If you're just now embarking on your journey to learn about game design, I
  would recommend sticking to the components provided by others. This engine is
  structured so that you don't need to worry too much about how the components
  work or how to make new components.  
}

That said, here's another function for creating new components (which you can
also safely ignore if you're just beginning).

@defproc[(new-component [struct? (-> any/c boolean?)]
                        [update  (-> game? entity? component? entity?)])
         void?]{
 
  To register a new component, you must use this function. You must provide two
  functions. 1) a way of recognizing your component, 2) a handler function that
  the engine will call on every tick, for every entity that has this component.

  This handler function returns an entity, which will replace the entity that
  has the component. The handler function also receives the game as an input. In
  effect, this means your handler function has read-only access to the entire
  game state. And it has "write" access to the entity possessing the component.  

  I would recommend looking at some of the existing components
  (i.e. @racket[key-movement]) if you're going to make new components of your own.
}

@subsection{Pre-built Components}

These components are design to allow you to add behaviour to entities with as
little effort as possible. This engine is designed for rapid prototyping --
which means that you should be able to experiment with a lot of different game
design ideas very quickly.  You do that by using pre-built components as the
fundamental building blocks for your game.


@defproc[(key-movement [speed integer?])
         component?]{
 
  An entity with this component will move when you press the arrow keys.  The
  speed parameter changes how quickly the entity moves.

  Technically speaking, this component updates the entity's @racket[posn]
  component whenever the arrow keys are pressed.

  Example:

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
 
  Determines where the entity will render.  If you're using
  @racket[sprite->entity], then you should specify this with the @racket[#:name]
  keyword, not the @racket[#:components] keyword

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
 
  Runs a one-time handler function after the specified number of ticks

  Example:

  @racketblock[
    (define bullet
      (sprite->entity bullet-sprite 
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-left #:speed 5))
                                   (after-time 50     die)))
  ] 
}

@defproc[(on-collide [name string?]
                     [fun (-> game? entity? component?)])
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

@defproc[(every-tick [func (-> game? entity? component?)])
         component?]{
                     
  Runs a handler function every tick in game

  Example:
   @racketblock[
    (define bullet
      (sprite->entity bullet-sprite 
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-left #:speed 5))
                                   (after-time 50     die)))
  ] 
}

@defproc[(circle-collider [radius number?])
         component?]{
 
  Changes out the default rect-collider for a circle collider with the specified
  radius

  Example:

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
 
  Kills this entity when (if) the health reaches 0

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


@section{Functions}

In addition to components, we also include various pre-built functions to allow
for quick and easy prototyping.

@defproc[(move)
         func?]{
  Should be used with components @racket[(speed)] and @racket[(direction)]. The
  direction component determines where the entity moves and the speed component
  determines how quickly the entity moves.

  Example:
   @racketblock[
    (define bullet
      (sprite->entity bullet-sprite 
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (speed 5)
                                   (direction 180)
                                   (every-tick (move))
                                   (after-time 50     die)))
  ] 
}


@defproc[(move-dir-spd [#:dir dir integer?]
                       [#:speed speed integer?])
         func?]{
  Moves the entity in a fixed manner of motion. dir determines in which
  direction the entity moves, where 0 is to the right and 180 is to the left.
  spd determines how fast the entity moves. (move-dir-spd) is different from
  (move) because the direction and speed parameters will be permanent, so that
  the speed or direction of motion cannot be changed.
}


@defproc*[([(move-right [#:speed spd integer?]) func?]
           [(move-down  [#:speed spd integer?]) func?]
           [(move-left  [#:speed spd integer?]) func?]
           [(move-up    [#:speed spd integer?]) func?])]{
  Same as @racket[(move-dir-spd)] with a direction component of 0, 90, 180, 270
  respectively
}

@defproc[(move-random  [#:speed spd integer?])
          func?]{
  Will randomly call move-left, move-right, move-up, or move-down and pass in
  the speed parameter to the chosen function call
}

@defproc[(move-up-and-down [#:min small integer?]
                           [#:max large integer?]
                           [#:speed spd integer?])
         func?]{
  Moves the entity up and down within a boundary set by small and large. The
  speed parameter determines how fast the entity moves
}

@defproc[(spin [#:speed spd integer?])
        func?]{
  Will rotate the entity, and spd will determine how quickly the entity spins
}

@defproc[(go-to [x integer?]
                [y integer?])
         func?]{
  Will change posn of entity to (posn x y)
}

@defproc[(go-to-random [min-x integer?]
                       [max-x integer?]
                       [min-y integer?]
                       [max-y integer?])
         func?]{
  Will change posn of entity to a random posn, with the bounds for the
  x-coordinate being min-x to max-y, and the bounds for the y-coordinate being
  min-y to max-y
}

@defproc*[([(go-to-pos [pos something?]) func?]
           [(go-to-pos-inside [pos something?]) func?])]{
  Will move the entity to somewhere along the edge of the screen.
  Values for pos: 'left, 'right, 'top, 'bottom, 'top-left, 'top-right,
  'bottom-left, 'bottom-right, 'left-center, 'right-center, 'top-center,
  'bottom-center. go-to-pos will move the entity's center to the edge, while
  go-to-pos-inside will keep the entity completely inside the screen.
}


@defproc[(respawn [edge something?])
         func?]{
  Will move the center of the entity to the specified edge of the screen. The
  entity will be placed somewhere along the edge randomly. Values for edge:
  'left, 'right, 'top, 'bottom
}

@defproc*[([(set-speed [spd integer?]) func?]
           [(set-direction [dir integer?]) func?])]{
  Change the speed or direction component of the entity to spd or dir
}
