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



Conceptually speaking, I like to think of entities as a combination of two
things: art and game logic. The art is specified by the sprite; the game logic
is specified by the collection of components.



@section{Sprites}

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


@defproc[(change-sprite [sprite-or-func (or image? func?)])
         func?]{
  This is a function that allows the sprite of an entity to be changed. If
  sprite-or-func is a image, then the entity's sprite will be changed to the
  image. If sprite-or-func is an function, then the entity's sprite will be
  changed to what is returned by the function.
}
                        
@subsection{Sheets}

A sheet is an image made up of multiple images, where each image within the
sheet can be used for animating the sprite.

@defproc[(sheet->sprite [sheet image?]
                        [#:row        r     integer?]
                        [#:column     c     integer?]
                        [#:row-number rnum  integer?]
                        [#:speed      delay integer?])
         sprite?]{
  Divides the sheet into r rows and c columns. This function will then return
  a sprite that will display each of the rnum row's images with a delay between
  each image of delay ticks.
}

@defproc*[([(sheet->rainbow-hue-sheet [sheet image?]) sheet?]
           [(sheet->rainbow-tint-sheet [sheet image?]) sheet?])]{
  Converts a 1 row animation or single image to 12 rows with shifted hue or tint
}

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

  NOTE: Components can also be added, removed, and updated during runtime.  
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

@defproc[(on-key [key symbol?] [func func?])
         func?]{
  Will call a handler function whenever the specified key is pressed.

  Example:

  @racketblock[
    (define shooter-entity
      (sprite->entity (rectangle 20 40 "solid" "black")
                      #:name        "shooter"
                      #:position    (posn 100 100)
                      #:components  (on-key 'space (spawn bullet-entity))))

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
 
  Runs a handler function after the specified number of ticks and then all
  after-time components are removed from the entity

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

@defproc[(spawner [sprite entity?] [amount integer?])
         component?]{
  Spawns a sprite every amount ticks. @racket[spawner] uses the sprite's
  position component as the relative spawn location to the main entity
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
    (define circle-entity
      (sprite->entity (circle 20 "solid" "red")
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (key-movement 5)
                                   (circle-collider 20)))
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

@defproc[(detect-collide [entity-name1 string?] [entity-name2 string?]
                          [func func?])
         component?]{
  Runs a handler function whenever the two named entities collide

  Example:

  @racketblock[
    (define score-entity
      (sprite->entity score-sprite
                      #:name        "score"
                      #:position    (posn 0 0)
                      #:components  (detect-collide "player" "coin" (add-to-score))))
 ]
}

@defproc[(on-edge [edge symbol?] [#:offset off integer?] [func func?])
         component?]{
  Runs a handler function whenever entity collides with specified edge.
  Possible values for edge are ['left, 'right, 'top, 'bottom].

  Example:
  @racketblock[
    (define player-entity
      (sprite->entity player-sprite
                      #:name       "player"
                      #:position   (posn 100 100)
                      #:components (on-edge 'right (win))))
 ]
}

@defproc[(detect-edge [name string?] [edge symbol?] [func func?])
         component?]{
  Runs a handler function whenvever the named entity collides with specified
  edge. Possible values for edge are ['left, 'right, 'top, 'bottom].

  Example:
  @racketblock[
    (define bg-entity
      (sprite-entity background-sprite
                     #:name        "bg"
                     #:position    (posn 0 0)
                     #:components  (detect-edge "player" 'right (change-background))))
 ]
}

@defproc[(stop-on-edge [edges symbols?])
         component?]{
  Prevents the entity from moving off screen along specified edges. Possible
  values for edge are ['left, 'right, 'top, 'bottom]. There can be any number of
  specified edges. If no parameters are passed in, default will be all edges.

  Example:
  @racketblock[
    (define player-entity
      (sprite-entity player-sprite
                     #:name       "player"
                     #:position   (posn 100 100)
                     #:components (stop-on-edge)))
  ]
}

@defproc[(wrap-around [mode symbol?])
         component?]{
  Moves entity to other edge when it moves off screen along specified modes.
  Possible values for mode are ['left-right, 'top-bottom]. There can be any
  number of modes. If no parameters are passed in, default will be both modes

  Example:
  @racketblock[
    (define player-entity
      (sprite->entity player-sprite
                      #:name       "player"
                      #:position   (posn 100 100)
                      #:components (wrap-around)))
 ]
}

@defproc[(rotation-style [mode symbol?])
         component?]{
  Only usable with entites with @racket[(move)]. Will flip the sprite
  image/animation left-right when the entity has direction between 90-270.
  @racket[(rotation-style)] assumes that the sprite is drawn facing right.
  Possible values for mode are 'left-right and 'face-direction. 'left-right will
  flip the sprite horizontally whenever direction is between 90 and 270.
  'face-direction will rotate the sprite according to the direction.


  Example:
  @racketblock[
    (define snake-entity
      (sprite->entity snake-sprite
                      #:name      "snake"
                      #:position  (posn 100 100)
                      #:component (speed 5)
                                  (direction 180)
                                  (every-tick (move))
                                  (do-every 10 (random-direction 0 360))
                                  (rotation-style 'face-direction)))]
                                             
 ]
}

@defproc[(follow [name string?] [interval number?])
         component?]{
  Will update the direction component of the entity to point towards the named
  entity every interval ticks. If interval is not specified, the default will be
  to set it to 1. @racket[(follow)] does not move the entity. Use
  @racket[(move)] to move the entity 
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
                                   (every-tick (move))))
  ] 
}

@defproc*[([(move-right [#:speed spd integer?]) func?]
           [(move-down  [#:speed spd integer?]) func?]
           [(move-left  [#:speed spd integer?]) func?]
           [(move-up    [#:speed spd integer?]) func?])]{
  Will move the entity with a speed of spd and a direction of 0, 90, 180, 270
  respectively. The speed and direction cannot be changed once set.

  Example:
  @racketblock[
    (define bullet
      (sprite->entity bullet-sprite
                      #:name       "bullet"
                      #:position   (posn 100 100)
                      #:component  (every-tick (move-left #:speed 5))))
  ]
}

@defproc[(move-random  [#:speed spd integer?])
          func?]{
  Will randomly call @racket[(move-left)], @racket[(move-right)],
  @racket[(move-up)], or @racket[(move-down)] and pass in the speed parameter to
  the chosen function call

  Example:
  @racketblock[
    (define random-bullet
      (sprite->entity bullet-sprite
                      #:name      "random"
                      #:position  (posn 100 100)
                      #:component (every-tick (move-random #:speed 5))))
 ]
}

@defproc[(move-up-and-down [#:min small integer?]
                           [#:max large integer?]
                           [#:speed spd integer?])
         func?]{
  Moves the entity up and down within a boundary set by small and large. The
  speed parameter determines how fast the entity moves

  Example:
  @racketblock[
    (define goalie-entity
      (sprite->entity goalie-sprite
                      #:name       "goalie"
                      #:position   (posn 500 300)
                      #:component  (every-tick (move-up-and-down #:min 150 #:max 450 #:speed 5))))
 ]
}

@defproc[(spin [#:speed spd integer?])
        func?]{
  Will rotate the entity, and spd will determine how quickly the entity spins

  Example:
  @racketblock[
    (define spinner-entity
      (sprite->entity spinner-sprite
                      #:name      "spinner"
                      #:position  (posn 300 300)
                      #:component (every-tick (spin #:speed 2))))
 ]
}


@defproc[(go-to [x integer?]
                [y integer?])
         func?]{
  Will change posn of entity to @racket[(posn x y)]

  Example:
  @racketblock[
    (define teleporter-entity
      (sprite->entity teleporter-sprite
                      #:name      "teleporter"
                      #:position  (posn 500 500)
                      #:component (key-movement 5)
                                  (do-every 30 (go-to 500 500))))
 ]
}

@defproc[(go-to-random [min-x integer?]
                       [max-x integer?]
                       [min-y integer?]
                       [max-y integer?])
         func?]{
  Will change posn of entity to a random posn, with the bounds for the
  x-coordinate being min-x to max-y, and the bounds for the y-coordinate being
  min-y to max-y

  Example:
  @racketblock[
    (define coin-entity
      (sprite->entity coin-sprite
                      #:name      "coin"
                      #:position  (posn 300 300)
                      #:component (on-collide "player" (go-to-random 0 600 0 600))))
 ]
}

@defproc*[([(go-to-pos [pos symbol?] [#:offset offset integer?]) func?]
           [(go-to-pos-inside [pos symbol?] [#:offset offset integer?]) func?])]{
  Will move the entity to somewhere along the edge of the screen.
  Possible values for pos: ['left, 'right, 'top, 'bottom, 'top-left, 'top-right,
  'bottom-left, 'bottom-right, 'left-center, 'right-center, 'top-center,
  'bottom-center, 'center]. @racket[(go-to-pos)] will move the entity's center
  to the edge, while @racket[(go-to-pos-inside)] will keep the entity completely
  inside the screen.

  offset will move the "edge" of the screen. A positive offset value will
  always move the edge to the right or down. A negative offset value will do the
  opposite. offset only applies for 'left, 'right, 'top, and 'bottom.

  Example:
  @racketblock[
    (define player-entity
      (sprite->entity player-sprite
                      #:name      "player"
                      #:position  (posn 100 100)
                      #:component (key-movement 5)
                                  (on-collide "enemy" (go-to-pos 'left))))
  ]
}


@defproc[(respawn [edge symbol?] [#:offset offset integer?])
         func?]{
  Will move the center of the entity to the specified edge of the screen. The
  entity will be placed somewhere along the edge randomly. Possible values for
  edge: ['left, 'right, 'top, 'bottom]

  offset will move the "edge" of the screen. A positive offset value will
  always move the edge to the right or down. A negative offset value will do the
  opposite. offset only applies for 'left, 'right, 'top, and 'bottom.

  Example:
  @racketblock[
    (define enemy-entity
      (sprite->entity enemy-sprite
                      #:name      "enemy"
                      #:position  (posn 100 100)
                      #:component (speed 5)
                                  (direction 180)
                                  (every-tick (move))
                                  (on-edge 'left (respawn 'right))))
 ]
}

@defproc*[([(set-speed [amount integer?]) func?]
           [(set-player-speed [amount integer?]) func?]
           [(set-direction [amount integer?]) func?]
           [(set-counter [amount integer?]) func?])]{
  Change the specified component of the entity to amount.
  @racket[(set-speed)] should be used with entities not controlled by the user
  (should not have @racket[key-movement]). @racket[(set-player-speed)] should be
  used with entities controlled by the user (should have @racket[key-movement])


  Example:
  @racketblock[
    (define player-entity
      (sprite->entity player-entity
                      #:name      "player"
                      #:position  (posn 100 100)
                      #:component (key-movement 5)
                                  (on-collide "speed-entity" (set-player-speed 10))))
 ]
}


@defproc*[([(random-direction [min integer?] [max integer?]) func?]
           [(random-speed [min integer?] [max integer?]) func?])]{
 Change the speed or direction component of the entity to a randomly choosen
 value between min and max

 Example:
 @racketblock[
    (define wandering-entity
      (sprite->entity wandering-sprite
                      #:name      "wandering"
                      #:position  (posn 600 300)
                      #:component (speed 3)
                                  (direction 180)
                                  (every-tick (move))
                                  (do-every 15 (random-direction 90 270))))
 ]
}

@defproc*[([(change-ai-speed-by [inc integer?]) func?]
           [(change-speed-by [inc integer?]) func?]
           [(change-direction-by [inc integer?]) func?]
           [(change-counter-by [inc integer?]) func?])]{
  Increase the specified component of the entity by inc. The specified component
  can be lowered by having inc be negative. @racket[(change-ai-speed-by)] should
  be used with entities not controlled by the user (should not have
  @racket[key-movement]). @racket[(change-speed-by)] should be used with 
  entities controlled by the user (should have @racket[key-movement])

  Example:
  @racketblock[
    (define wolf-entity
      (sprite->entity wolf-sprite
                      #:name      "wolf"
                      #:position (posn 0 0)
                      #:component (key-movement 3)
                                  (on-collide "sheep" (change-speed-by 1))))
 ]
}


@defproc[(spawn [sprite entity?])
         func?]{
  Spawns a sprite at current position of the entity. The sprite's posn component
  will be used as the offset from the current position of the entity.
  @racket[(spawn)] will automatically adjust for @racket[rotation-style]

  Example:
  @racketblock[
    (define gun-entity
      (sprite->entity gun-sprite
                      #:name      "gun"
                      #:position  (posn 100 100)
                      #:component (on-key 'space (spawn bullet-entity))))
 ]
}


@defproc[(point-to [name string?])
         func?]{
  Sets the direction component of the entity to point towards the named entity.
  @racket[(point-to)] does not rotate the sprite of the entity

  Example:
  @racketblock[
    (define missile-entity
      (sprite->entity missile-sprite
                      #:name      "missile"
                      #:position  (posn 0 0)
                      #:component (do-every 20 (point-to "target"))))
 ]
}


