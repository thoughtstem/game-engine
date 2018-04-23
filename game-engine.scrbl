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
library, and 2) the functions.

@section{Sheets}

@defproc[(sheet->sprite [img image?]
                        [#:rows r integer?]
                        [#:columns c integer?]
                        [#:row-number rnum integer?]
                        [#:speed delay integer?])
         animated-sprite?]{
 Divides @racket[img] into r rows and c columns, and returns the rnum row of
 images. @racket[delay] determines how long the animation between each picture
 takes

 Example:
 @racketblock[
  (define hero-sprite
    (sheet->sprite (scale 0.5 some-img-sheet)
                   #:rows       4
                   #:columns    3
                   #:row-number 3
                   #:speed      5))


  >(animated-sprite (vector first-pic-in-third-row
                            second-pic-in-third-row
                            third-pic-in-third-row) 3 0 5 0)

 ]
}

@defproc[(sheet->rainbow-hue-sheet [img image?])
         image?]{
  Converts a 1 row animation or single image to 12 rows with shifted hue
}

@defproc[(sheet->rainbow-tint-sheet [img image?])
         image?]{
  Converts a 1 row animation or single image to 12 rows with shifted tint
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

@defproc[(static)
         component?]{
  Entities that both have @racket[(static)] cannot interact with each other
  using @racket[(on-collide)]
}


@defproc[(key-movement [speed integer?])
         component?]{
 
  An entity with this component will move when you press the arrow keys.  The
  speed parameter changes how quickly the entity moves.

  Technically speaking, this component updates the entity's @racket[posn]
  component whenever the arrow keys are pressed.

  Example:
  Creates an entity that will move when arrow keys are pressed 
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
  Creates an entity at coordinates (50,50)
   @racketblock[
    (define sun-entity
      (sprite->entity (circle 20 "solid" "yellow")
                      #:name       "sun"
                      #:position   (posn 50 50)))
    ]
}

@defproc[(after-time [ticks integer?]
                     [func (-> game? entity? component? entity?)])
         component?]{
 
  Runs a one-time handler function after the specified number of ticks.
  This function should only be used with @racket[die], otherwise, use
  @racket[(do-every)]

  Example:
  Creates an entity that will die after 50 ticks 
  @racketblock[
    (define rot-entity
      (sprite->entity (circle 10 "solid" "black")
                      #:position   (posn 100 100)
                      #:name       "rot"
                      #:components (after-time 50     die)))
  ] 
}

@defproc[(do-every [ticks integer?]
                   [func (-> game? eneity? component? entity?)])
         component?]{
  Runs a handler function after every specified number of ticks

  Example:
  Creates a entity that will change direction every 20 ticks
  @racketblock[
    (define random-enemy-entity
      (sprite->entity (circle 20 "solid" "red")
                      #:name       "enemy"
                      #:position   (posn 100 100)
                      #:components (speed 5)
                                   (direction 180)
                                   (every-tick (move))
                                   (do-every 20 random-direction 0 360)))
 ]
}


@defproc[(on-start [func (-> game? entity? component?)])
         component?]{
  Runs a handler funciton when the application first runs

  Example:
  Creates an entity that will be centered on the left edge 
  @racketblock[
    (define left-entity
      (sprite->entity (rectangle 10 100 "solid" "black")
                      #:position   (posn 100 100)
                      #:name       "left"
                      #:components (on-start (go-to 'left-center))))
  ]
}

@defproc[(on-collide [entity-name string?]
                     [func (-> game? entity? component?)])
         component?]{
 
  Runs a handler function when this entity touches the named entity

  Example:
  Creates an entity that will die when it collides with an enemy and will move
  faster when it collides with gas
  @racketblock[
    (define car-entity
      (sprite->entity (rectangle 40 20 "outline" "red"
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (key-movement 5)
                                   (on-collide "gas"   (change-speed-by 1))
                                   (on-collide "enemy"  die))))
  ] 
}

@defproc[(detect-collide [entity-name1 string?]
                         [entity-name2 string?]
                         [func (-> game? entity? component?)])
         component?]{
  Runs a handler function when the two name entities touch each other

  Example:
  Increase counter by 100 whenever "hero" and "chest" collide
  @racketblock[
    (define score-entity
      (sprite->entity (text "Score: 0" 32 "yellow")
                      #:name       "score"
                      #:position   (posn 0 0)
                      #:components (detect-collide “hero” “chest” (change-counter-by 100))))
    ]
}

@defproc[(every-tick [func (-> game? entity? component?)])
         component?]{
                     
  Runs a handler function every tick in game

  Example:
  Creates an entity that moves left every tick
   @racketblock[
    (define bullet
      (sprite->entity bullet-sprite 
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-left #:speed 5))))
  ] 
}

@defproc[(circle-collider [radius number?])
         component?]{
 
  Changes out the default rect-collider for a circle collider with the specified
  radius

  Example:
  Creates an entity with a circle collider with radius 10
  @racketblock[
    (define spaceship-entity
      (sprite->entity (circle 10 "solid" "blue")
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (key-movement 5)
                                   (on-collide "ore"    (change-speed-by 1))
                                   (circle-collider 10)))
  ] 
}

@defproc[(health [amount integer?])
         component?]{
 
  Kills this entity when (if) the health reaches 0

  Example:
  Creates an entity that will lose 1 health every time it collides with a bullet
  @racketblock[
    (define spaceship-entity
      (sprite->entity spaceship-sprite
                      #:name       "ship"
                      #:position   (posn 100 100)
                      #:components (health 5)
                                   (on-collide "bullet" (change-health -1))))
  ] 
}

@defproc[(spawner [sprite entity?] [time integer?])
         component]{
  Will spawn a sprite entity once every time ticks.

  Example:
  Creates an entity that will "shoot" (spawn) a bullet entity every 25 ticks
  @racketblock[
    (define gun-entity
      (sprite->entity (rectangle 20 40 "solid" "white")
                      #:name       "gun"
                      #:position   (posn (100) (100))
                      #:components (spawner bullet 25)))
  ]
}

@defproc[(physical-collider)
         component?]{
 
  Two entities with physical colliders will not pass through each other.
  This overrides things like key-handler.
}

@defproc*[([(speed [value integer?]) component?]
           [(direction [value integer?]) component?])] 
  Determines motion when @racket[(move)] is called

@defproc[(counter [count integer?])
         component?] 
  Generic counter component that can be used for anything like: score, health,
  ammo, high-score. Only one counter component allowed per entity.


@section{Functions}

In addition to components, we also include various pre-built functions to allow
for quick and easy prototyping.

@defproc[(do-many ...)
         func?]{
  Does multiple handler functions at once
}

@defproc[(move)
         func?]{
  Should be used with components @racket[(speed)] and @racket[(direction)]. The
  direction component determines where the entity moves and the speed component
  determines how quickly the entity moves.

  Example:
  
   @racketblock[
    (define bullet
      (sprite->entity (circle 5 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (speed 5)
                                   (direction 180)
                                   (every-tick (move))))
  ] 
}


@defproc[(move-dir-spd [#:dir dir integer?]
                       [#:speed speed integer?])
         func?]{
  Moves the entity in a fixed manner of motion. dir determines in which
  direction the entity moves, where 0 is to the right and 180 is to the left.
  spd determines how fast the entity moves. @racket[(move-dir-spd)] is different
  from @racket[(move)] because the direction and speed parameters will be
  permanent, so that the speed or direction of motion cannot be changed.

  Example:
  Functionality wise, exact same as example above
  @racketblock[
    (define bullet
      (sprite->entity (circle 5 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-dir-spd #:dir 180
                                                             #:speed 5))))
    ]
}


@defproc*[([(move-right [#:speed spd integer?]) func?]
           [(move-down  [#:speed spd integer?]) func?]
           [(move-left  [#:speed spd integer?]) func?]
           [(move-up    [#:speed spd integer?]) func?])]{
  Same as @racket[(move-dir-spd)] with a direction component of 0, 90, 180, 270
  respectively

  Example:
  Same as examples above
  @racketblock[
    (define bullet
      (sprite->entity (circle 5 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-left #:speed 5))))
    ]
}

@defproc[(move-random  [#:speed spd integer?])
          func?]{
  Will randomly call @racket[move-left], @racket[move-right], @racket[move-up],
  or @racket[move-down] and pass in the speed parameter to the chosen function
  call. 

  Example: 
  The entity will continue to move in a randomly chosen direction with speed 5
  @racketblock[
    (define bullet
      (sprite->entity (circle 5 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-random #:speed 5))))
 ]
}

@defproc[(move-up-and-down [#:min small integer?]
                           [#:max large integer?]
                           [#:speed spd integer?])
         func?]{
  Moves the entity up and down within a boundary set by small and large. The
  speed parameter determines how fast the entity moves

  Example:
  Moves the bullet from y-coordinates 0 to 100 with a speed of 5
  @racketblock[
    (define bullet
      (sprite->entity (circle 5 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "bullet"
                      #:components (every-tick (move-up-and-down #:min 0
                                                                 #:max 100
                                                                 #:speed 5))))
 ]
}

@defproc[(spin [#:speed spd integer?])
        func?]{
  Will rotate the entity, and spd will determine how quickly the entity spins

 Example:

  @racketblock[
    (define propeller-entity
      (sprite->entity (rectangle 40 10 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "propeller"
                      #:components (every-tick (spin #:speed 10))))
 ]
}

@defproc[(go-to [x integer?]
                [y integer?])
         func?]{
  Will change posn of entity to @racket[(posn x y)]

  Example:
  The entity will go back to @racket[(posn 100 100)] every 100 ticks
  @racketblock[
    (define rocket-entity
      (sprite->entity (circle 5 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "rocket"
                      #:components (every-tick (move-left #:speed 2))
                                   (do-every 100 (go-to 100 100))))
 ]
}

@defproc[(go-to-random [min-x integer?]
                       [max-x integer?]
                       [min-y integer?]
                       [max-y integer?])
         func?]{
  Will change @racket[posn] of entity to a random @racket[posn], with the bounds
  for the x-coordinate being min-x to max-y, and the bounds for the y-coordinate
  being min-y to max-y

  Example:
  The entity will move to a random location within a 200x200 square every 5
  ticks
  @racketblock[
    (define teleporting-entity
      (sprite->entity (circle 5 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "teleport"
                      #:components (do-every 5 (go-to-random 0 200 0 200))))
 ]
}

@defproc*[([(go-to-pos [pos something?]) func?]
           [(go-to-pos-inside [pos something?]) func?])]{
  Will move the entity to somewhere along the edge of the screen.
  Values for pos: 'left, 'right, 'top, 'bottom, 'top-left, 'top-right,
  'bottom-left, 'bottom-right, 'left-center, 'right-center, 'top-center,
  'bottom-center. go-to-pos will move the entity's center to the edge, while
  go-to-pos-inside will keep the entity completely inside the screen.
  
  Example:
  The spaceship will teleport when it collides with one of two previously
  defined portal entities.
  @racketblock[
    (define spaceship-entity
      (sprite->entity (circle 10 "solid" "yellow")
                      #:position   (posn 100 100)
                      #:name       "ship"
                      #:components (key-movement 5)
                                   (on-collide "portal1" (go-to-pos-inside 'bottom-center))
                                   (on-collide "portal2" (go-to-pos 'bottom-center))))
 ]
}


@defproc[(respawn [loc something?])
         func?]{
  Will move the center of the entity to the specified location on the screen.
  The entity will be placed somewhere along the edge randomly. Values for loc:
  'left, 'right, 'top, 'bottom, 'anywhere

  Example:
  @racketblock[
    (define spaceship-entity
      (sprite->entity (circle 10 "solid" "yellow")
                      #:position   (posn 100 100)
                      #:name       "ship"
                      #:components (key-movement 5)
                                   (on-collide "portal1" (respawn 'left))))
 ]
}

@defproc*[([(set-speed [amount integer?]) func?]
           [(set-direction [amount integer?]) func?]
           [(set-counter [amount integer?]) func?])]{
  Change the corresponding component of the entity to amount

  Example:
  @racketblock[
    (define racecar-entity
      (sprite->entity (rectangle 20 30 "solid" "orange")
                      #:position   (posn 100 100)
                      #:name       "car"
                      #:components (speed 5)
                                   (direction 270)
                                   (counter 0)
                                   (every-tick (move))
                                   (on-collide "boost" (do-many (set-speed 10)
                                                                (set-direction 180)
                                                                (set-counter 1)))))
 ]
}

@defproc*[([(random-direction [min integer?] [max integer?]) func?]
           [(random-speed [min integer?] [max integer?]) func?])]{
  Change the speed or direction component of the entity to some random value
  from min (inclusive) to max (exclusive)

  Example:
  @racketblock[
    (define racecar-entity
      (sprite->entity (rectangle 20 30 "solid" "orange")
                      #:position   (posn 100 100)
                      #:name       "car"
                      #:components (speed 5)
                                   (direction 270)
                                   (every-tick (move))
                                   (on-collide "random-boost" (do-many (random-direction 0 360)
                                                                       (random-speed 1 10)))))
      
 ]
  
}

@defproc*[([(change-ai-speed-by [delta integer?]) func?]
           [(change-speed-by [delta integer?]) func?]
           [(change-direction-by [delta integer?]) func?]
           [(change-counter-by [delta integer?]) func?])]{
  Change the corresponding component by amount delta.

  Example:
  @racketblock[
    (define player-entity
      (sprite->entity (circle 20 "solid" "white")
                      #:position   (posn 100 100)
                      #:name       "player"
                      #:components (key-movement 5)
                                   (on-collide "boost" (change-speed-by 1))
                                   (on-collide "gold" (change-counter-by 1)
                                   (on-collide "enemy" die))))

    (define enemy-entity
      (sprite->entity (circle 20 "solid" "red")
                      #:position    (posn 200 200)
                      #:name        "enemy"
                      #:components  (speed 5)
                                    (direction 180)
                                    (on-collide "boost" (change-ai-speed-by 1))
                                    (on-collide "oil" (change-direction-by 30))))
                                    
                      
 ]

  *@racket[change-speed-by] is only used for entities controlled by players
  using @racket[(key-movement)]. @racket[change-ai-speed-by] is used for
  non-playable entities.
}

@defproc[(change-sprite [sprite-or-func (or/c sprite? func?)])
         func?]
  Changes the sprite of the entity to either a given sprite, or if a function
  was given, the sprite the function generates.


