#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{core}
@author{thoughtstem}

@defmodule[game-engine/engine/core]

@defstruct[game ([entities (listof entity?)])]{
  A game is simply a list of entities.
}

@defstruct[entity ([id positive?] 
                   [components (listof component?)])]{
  An entity is simply a list of components
}

@defstruct[component ([id positive?] 
                      [update (-> game? entity? component? entity?)])]{
  A component is, at minimum, an id and an update function.
  Often, though, you will use @racket[define-component] to create your own component types, which may have have additional fields.  
}

@defproc[(game [entities (listof entity?)] ...)
         game?]{
  Constructor for a @racket[game].
}

@defproc[(entity [components (listof component?)] ...)
         entity?]{
  Constructor for a @racket[entity].
}

@defform[(define-component name (fields ...))]{
  Creates a new component type named @racket[name].  This is essentially a struct type with @racket[fields], plus an id field, and an update field.  Defining a new component type gives you a constructor function, e.g.:

        @codeblock{
          ;TODO: Fix semantics of #:update, add #:update-entity
          ;      Redoc.  Retest.
          (define-component health (amount))

          (define (decrement-health h) 
            ;TODO: Macroified lenses could clean this up?
            (health (sub1 (health-amount h))))

          (define hero-health 
            (health 100 #:update decrease-over-time))

          (define hero (entity hero-health))
        } 
}


@defproc[(tick [game game?])
         game?]{
  The returned game is the previous game, advanced one tick.
 
  It does this by mapping @racket[tick-entity] across all of the
  game's entities.  
}

@defproc[(tick-entity [game game?] [entity entity?])
         entity?]{
  Returns a ticked entity by looping over each of its component's
  @racket[component-update] functions.  These functions always
  match the signature of @racket[tick-component], which
  means they all return an @racket[entity].

  This @racket[entity] becomes the entity on the next loop's iteration.
  Since it is the @racket[component] that produces entities,
  they can effectively do the following: add and remove components,
  update their own state, update the state of other components
  on the entity.

  They also effectively have read-only access to the rest of the @racket[game] state.  

  The ordering of components is
  important.  An upstream component's result can be overwritten by a downstream component's result.  In other words, the upstream component is handled first, which means later components changes take precidence. 
}

@defproc[(tick-component [game game?] [parent-entity entity?] [parent-component component?])
         entity?]{
  This isn't a real provided function.  This just documents how @racket[component]'s update functions should behave in general.  

  Such a function should be a pure function that returns a new entity based on a game, an entity, and a component.  Generally speaking, this will be attached to a @racket[component] which will, itself, be in the compent list of some @racket[entity].  We'll call those the @racket[parent-entity] and @racket[parent-component] respectively.

  Such a function will be generally be called in the context of a @racket[tick-entity] function, so the entity being returned is the entity that this function "wants" to be the state of the @racket[entity] in the next tick.  

  Whether it will truly be the next state is not controlled by this function.  That is handled by the index of this component within its parent entity's components.

  Note that the @racket[entity-id] field of the returned entity will be ignored in the game.  Component functions are not allowed to change an entity's id.
}




