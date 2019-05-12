#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{core}
@author{thoughtstem}

@defmodule[game-engine/engine/core/main]

Multi-lingual game programming.  Many languages, all entity/component systems beautifully composed

Gives you a truly building-blocks approach to designing animations, simulations, and games.  Bottom up.  Easily create and share your own components, entities, games, partial games, game constructors, procedurally generated games, etc.  They're all just values and easily compose with one another.  

Manipulating games programatically is quite easy.

@codeblock{
  (define-health health number?)

  (define e (entity (health 5
                            (+ (get-Health) 1))
  (define g (game e e e))
  (define g1 (tick g))
  (define g2 (tick g1))

  ;Note there's no renderer at this level.
}

That was relatively simple.  Three entities that start at 5 health and gain health at a rate of 1 per tick.  
It's not even worth rendering -- though perhaps it could be a nice building block for a simulation.

However, when you import other stuff... 

@codeblock{
;TODO: More interesting example.
}

@section{Basic Data Structures}

@defproc[(game [entity entity?] ...)
         game?]{
  Constructor for a @racket[game].  Takes nothing but a list of entities.
}

@defproc[(entity [component component?] ...)
         entity?]{
  Constructor for a @racket[entity].  Takes nothing but a list of components.
}

Other than games and entities, the other basic building block is the component.  But these are things that you define and create yourself.

@defform[(define-component name contract)]{
  Creates a new kind of component named @racket[name], whose values will always match the type described by the contract @racket[contract].  
  Creating a component causes a bunch of useful functions to bedefined for you.

  The most obvious one is the constructor, which we shall show in context below.

@codeblock{
  (define-component health number?)

  (define hero-health (health 100 
                              (- (get-health) 1)))

  ;Now it can be attached to an entity, which can be added to a game, and so on...
  (define hero (entity hero-health))
} 

  A component constructor (like @racket[health]) above takes two arguments.  One is an initial value, the other is a description of how that value changes over time.

  Above, the initial value of health is @racket[100]; the description of change is @racket[(- (get-health) 1)].  Together, these describe a health component whose value decreases with each tick.

  It also introduces one of the other things you get for free with @racket[define-component], which is a getter: @racket[get-health].  This getter, when used without arguments, gets the current health value of the entity to which the component is attached.

  Note that this getter can be used from other components, too.  For example:

@codeblock{
  (define-component health number?)
  (define-component death  any/c)

  (define hero-health (health 100 
                              (- (get-health) 1)))
  (define hero-death (death (void)
                            (when (zero? (get-health))
                              (despawn))))

  (define hero (entity hero-health
                       hero-death))
} 

  Here the @racket[death] component uses @racket[(get-health)] to check when the @racket[health] component has reached 0.  These getters are the primary way of getting data to flow between components.  (What happens if there are two @racket[health] components on the entity?  Don't do that; it's an error.  Components must have unique names.) 

  A quick note, there's one case where you don't have to use a getter, and can take advantage of a shorter syntax.  The @racket[health] component could have been implemented as:

@codeblock{
  (health 100 (lift sub1)) 
} 

Or, using the alias @racket[^], even shorter:

@codeblock{
  (health 100 (^ sub1)) 
} 

These mean, apply the given function to the value stored in the component.  
But be careful, it's not this simple:

@codeblock{
  (health 100 sub1) 
} 

That is syntactically valid, but would mean that on every tick you want to replace the value in the component with the function reference @racket[sub1].  I'm not sure why you'd want to do that, but there are reasons you might want to store functions in components, so that ability is there.  Because that ability is there, you need to lift functions if you want to apply them, rather than store them.

Note: there are two special values that, if stored in a component, have a special meaning in the runtime.

One is @racket[spawn], which is a struct that stores an entity.  On any tick that this value appears in a component, the runtime will spawn that entiti.

The other is @racket[despawn], which is a struct with no fields.  On any tick that this value appears in a component, the runtime will destroy the entiti to which that component is attached.
} 

Now that we've defined games, entities, and components, we can talk about the runtime model.  Note that you have access to all functions that advance the state of games, entities, and components.  


Do a meta example!!!


@defproc[(tick [game game?])
         game?]{
  The returned game is the previous game, advanced one tick.

  It does this by ticking every component on every entity in the order that they are listed.  During the tick, if any @racket[spawn] or @racket[despawn] values are produced by components, the runtime will take the appropriate actions AFTER all of the components have been ticked.

  By default, the game is copied -- not mutated.  To remove this guarantee, run within @racket[(mutable! ...)].
}

@defproc[(tick-entity [entity entity?])]{
  Ticks all of the components on an entity.  If any @racket[spawn] or @racket[despawn] values are produced, they have no effect.
}

@defproc[(tick-component [component component?])]{
  Updates the value stored in the component by replacing it with the value in the component's update expression.  

 If any @racket[spawn] or @racket[despawn] values are produced, they have no effect.
}

@defproc[(ticks [n number?]
                [game game?])
         game?]{
  Calls @racket[tick] @racket[n] times.
}

@defproc[(tick-list [game game?]
                    [n positive?])
         (listof game?)]{
  Returns a list of games beginning with the input game.  The list has length @racket[n].  Each subsequent game in the list is one @racket[tick] away from each other.

  Useful in debugging and analysis of games and simulations.  Also useful for getting all the frames of an animation for subsequent processing.
}

When you are writing your game logic, you will mostly be defining components (or using other people's components).  And you'll be defining functions that specify how components and/or entities change over time.  There's a basic CRUD (create, read, update, destroy) model for both components and entities.

@defproc[(add-component [entity entity?]
                        [component component?])
         entity?]{
  Create.

  Adds the component to the beginning of the entity's list of components. 
  Returns a new entity where this is the case.  Does not change the original. 
}

@defproc[(get-component [entity entity?]
                        [query? (or/c component? (-> component? boolean?))]) 
         (or/c #f component?)]{
  Read.

  Finds and returns the first component on the entity where @racket[pred?] is true.
}

@defproc[(update-component [entity entity?]
                           [old    (or/c component? (-> component? boolean?))]
                           [new    (or/c component? (-> component? component?))])
         entity?]{
  Update.

  Finds an existing component based on @racket[old].  If @racket[old] is a component, then its id will be used to find a matching component on the entity.  If @racket[old] is a predicate, it will find the first component for which that predicate is true.

  Either way, it will be updated according to @racket[new].  If @racket[new] is a component, that component will be swapped in (but @racket[old]'s id will remain).  If it is a function, that function will be called on @racket[old] to get the new component.
}

@defproc[(remove-component [entity entity?]
                           [old    (or/c component? (-> component? boolean?))])
         entity?]{
  Destroy.

  Finds an existing component based on @racket[old].  If @racket[old] is a component, then its id will be used to find a matching component on the entity.  If @racket[old] is a predicate, it will find the first component for which that predicate is true.

  The resulting entity no longer has @racket[old] in its list.
}

With entities, the CRUD model is similar.

@defproc[(add-entity [game game?]
                     [new  entity?] )
         game?]{
  Create.

  Adds the new entity to the beginning of the game's entity list.  Returns a new game where this is the case.  Does not modify the old game.
}

@defproc[(get-entity [game game?]
                     [query    (or/c entity? (-> entity? boolean?))] )
         entity?]{
  Read.

  Finds the entity within the game.  The entity can be described either by a generic function or by an existing entity (in which case the id is used to find the match)
}


@defproc[(update-entity [game game?]
                        [old    (or/c entity? (-> entity? boolean?))]
                        [new    (or/c entity? (-> entity? component?))])
         game?]{
  Update.

  Finds and updates an old entity. See @racket[update-componet]; this function has the same flexibility of types when it comes to @racket[old] and @racket[new].

}


@defproc[(remove-entity [game game?]
                        [old    (or/c entity? (-> entity? boolean?))])
         game?]{
  Destroy.

  Finds and removes an entity described by @racket[old].
}

A useful function for constructing a function of the type @racket[(-> entity? boolean?)] is @racket[has-component].

@defproc[(has-component [query (-> component? boolean?)])
         (-> entity? boolean?)]{
Given the component query, returns an entity query that matches if the entity has a component matching the component query.

        @codeblock{
        (update-entity g 
                     (has-component health?)
                     (update-entity-health-amount add1))
        }
}

@section{Rendering}

The rendering system takes a game and ticks it until the user terminates it. 

@defproc[(play! [game game?)]) game?]{
  Pass in a game to rendering. 

  Any entities that have a @racket[position] component and a @racket[sprite]
  component will be rendered.  See examples below.

  These components are provided by ____.

  TODO: Add layer, rotation, scale, and coloration support...
}

@section{Examples}

Sprites can be constructed with @racket[2htdp/image] objects.

Here's a simple example that uses position, rotation, and scale.
It also demonstrates a common pattern -- to use a counter whose data flows into various other components.

@codeblock{
(require 2htdp/image)

(define green-star                                                       
  (register-sprite (star 40 'solid 'green)))                             

(define-component counter number?)                                       
    
(define e
  (entity
    (sprite green-star)

    (counter 0
             (^ add1))

    (rotation 0
              (remainder (get-counter) 360))

    (size 1
          (sin
            (/ (get-counter) 100)))

    (position (posn 0 200)
              (struct-copy posn (get-position)
                           [x (get-counter)]))))                         
                                                                         
(play! (game e))
}

Here's an example with input.  The input data is not
on the entity whose position is changing; however, it can access it through
@racket[(get-current-input)], which fetches it from the @racket[input-manager],
which you must put into the game if you want to get input data.

@codeblock{
(require 2htdp/image)

;Doc this
(define e
  (entity 
    (position (posn 200 200) 
              (posn-add 
                (get-position)
		(as-posn (get-current-input))))

    (sprite (register-sprite (circle 5 'solid 'green)))))

(define g
  (game input-manager e))

(play! g)  
}

;TODO: Generalize into more specific examples: level swapping when some objective is complete, warping from one game to another via links, one game throws error / other catches it and continues, finally implement hotswap-dev         

;How to implement level swap?  New zones?  "Link" between games?  Warps?
;  Game manager?
;  Back buffer? 


;TODO: Clean up math functions / delta time / etc.



[
TODO: Start an example game
  RPG game started.  Has character and background... AWESOME!

  Keep discipline.
    Rule #0
      Invest in gap reduction.  (TODO: Read Racket evaluation model, racket guide, continuation marks, exceptions, all the basics.)
    Rule #1
      Always: Feature / Ractor / Repeat.
    Rule #2
      Files of screen height only.
    Rule #3
      Something about keeping unit tests and docs in lock step with development
    Rule #4
      Breaks for creative inspiration.  When?  How often?  Enternal question.
      <At minimum before a costly cycle, take some time to decide if the cycle is necessary.  Step away from computer.>

  Turn this into a legit algorithm and move to top-level 

  What's the cleanup goal?
    Have we built enough feature to know something new?
      - If we know something new, document and test it.
    Unit tests for the stuff I'm using in RPG
      - Rendering
      - Input 
      - Helper components
      - Systems: animation / movement 

Meta:

 The goal is to have a robust (documented) way of doing the following:
 * Rendering
 * Input
 * Physics
 * Animation  
 * Other useful components, paradigms, idioms...

 In part, we'll need to build some kind of side project to really be sure... And that will help keep things organized, plus provide a body of example code to help seed the community.   Start making an RPG example -- heading toward the planes or one of the TS-Langs. 
]

[TODO: Continue demonstrating features of input.
  How does as-position work?  How to you bind other keys to things?
  Mouse input?  ...]
[Doing all this would complete the input part of the engine.]

[Also crutial and perhaps worth putting before input.
  Animation...
 Abstract animation into a system...  Document it.
] 

[Physics...
  Fucking prototyped it!
  Now just simplify racket-chipmunk stuff.  That needs some serious polishing.
]

<Then I would start looking for ways to deploy this into dev>

@section{Maintenance Tasks}

;TODO: Immutability is a lie.  See comment in test/define-component.rkt
;  Make a test that explicitly checks this property
;  By the way, is mutability helping us much?  Maybe we should take it out if it's not...

;TODO: Making render tests automated
;(Screenshots??)


@section{Feature Ideas}

[
Moonshot:
  Could we render game previews (various projections of the game?) embedded in a DrRacket snip? 
  That sounds like a game changer... 
]

;Can we use components that store (and tick) other components as a way to implement "systems"?
;Rendering two games at once.  A child game?  
;Components within components.  Entities within components

;Keep having ideas about using rosette or constraint based programming to do
; * Procedural geneartion
; * Generating entire games, sequences
; * Flockin behavior

;Or other meta stuff:
; * A component containing a game and a behaviour that ticks it...
; * Run subgames within a game...
; * Procedurally create games at runtime, run them, do something with the result.
; * "Bake" a game, by running it and observing its values.  Faster now as a sub-game...

;For the paper we write about this engine:
; * Game-oriented programming?
; * A game-based programming "paradigmn"?

@section{Performance}


;ADD a section about performance. Raw notes below...
;TODO: Make conway fast again.  Query optimizations.  Caching.
;  I tried adding a lookup hash to entities, but I'm not seeing any speed improvement on the red/green poopers...
;    Should remove if it's not helping
;TODO: Roll a profiler (or build on top of Racket's now that we've simplified things again).
;  Done.
;  Testing it.  It seemed to blame Position for being slow, so I changed to an implementation of posn that it claims is faster.
;    But... it doesn't feel faster and the FPS looks the same.
;    (Then again, I need to disable the sampling to have a true test...)

;Have it print the stats over time


;One big problem is that I don't know what I should be expecting from this engine.  Is 20 FPS with 250 entities good or bad?
;  What would happen with unity?
;  What would happen with 2htdp/universe?

;Or another angle:
;  How many math operations per second should Racket be able to do?  How much overhead from garbage?  How much from memory allocations?
;  How many is this engine doing?
;  High level profiler might be able to give us that picture, btw.  So we can keep our estimations honest.

