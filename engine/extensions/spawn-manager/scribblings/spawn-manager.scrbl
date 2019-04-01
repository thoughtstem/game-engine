#lang scribble/manual

#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{spawns}
@author{thoughtstem}

@defmodule[game-engine/engine/extensions/spawn-manager]

Adds basic functionality for spawning new entities in game.
To use this system, you must add a spawner entity to your game.

The @racket[spawn-manager] function is provided for this reason.

@codeblock{
  (define s (spawn-manager))
}

A spawner is an entity that has a component (also called spawner) that stores a list of entities that are about to be spawned.  When this component's game-handler runs, those entities are transfered into the game automatically and removed from the spawner's list.

To spawn something, a game-handler must update the spawner entity in your game, placing a new entity into that list.  The convenient function @racket[spawn] is given for this purpose.

@codeblock{
  (define i-get-spawned
    (entity (new-health 5)))
  
  (define i-cause-spawns
     (entity (new-component #:game-handler (spawn i-get-spawned))))

  (game s ;We must add the spawn manager to the game.
        i-cause-spawns)
}



Now, on every tick the the entity @racket[i-cause-spawns] will trigger a spawn of @racket[i-get-spawned].  Note that it will take two ticks before the spawned entity shows up in the game.  It waits in the spawn manager's queue until the spawn manager's handlers get run. 

This gives spawn manager time to do extra work with these entities if necessary.


@defproc[(spawn-manager)
         entity?]{
  Returns a spawn manager that must be placed in your game for the @racket[spawn] function to work.
}


@defproc[(spawn [to-spawn (or/c entity? (-> entity?))])
         game-handler?]{
  Handler that will find and update the spawn manager in the game.   
  It can be placed on any entity that you want to think of as the one "doing the spawning".  A spaceship might, for example, be the spawner for its own bullets. 
}





