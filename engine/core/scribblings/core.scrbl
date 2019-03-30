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
                      [handler component-handler?]
                      [entity-handler entity-handler?])]{
  A component is, at minimum, an two update functions (only one of which needs to be populated).  

  Usually you will use @racket[define-component] to create your own component types, which may have have additional fields.  And you will use the constructor generated from @racket[define-component] to construct instances of your defined component.
}


@defthing[game-handler? (-> game? entity? component? game?)]{
  Currently for internal use only.  You could construct a function with this type, but you can't register it with the engine.  Internally, though, the less powerful functions of type @racket[entity-handler?] and @racket[component-handler?] attached to components are lifted to type @racket[game-handler?] before they run.
}

@defthing[entity-handler? (-> entity? component? entity?)]{
  Signature for the kind of function you would attach to your components with @racket[#:entity-handler], and which returns the next desired state for that @racket[entity].
}

@defthing[component-handler? (-> component? component?)]{
  Signature for the kind of function you would attach to your components with @racket[#:entity], and which returns the next desired state for that @racket[component].
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
  Creates a new component type named @racket[name].  This is essentially a struct type with @racket[fields] whose super type is @racket[component].

  It has a special constructor, which we shall show in context below.

  A common usecase is that you'll want to bind a function that takes and returns something of your component type:

        @codeblock{
          (define-component health (amount))

          ;Simple function for updating the state of a health component
          (define (decrement-health h) 
            ;Note that the syntax is exactly as if health were a normal struct.
            ;   However, using define-component will also give an even cleaner syntax shown in a later example
            (struct-copy health h
                         [amount (sub1 (health-amount h))]))

          ;Since the type of our update function is health? -> health?, we can use #:handler
          (define hero-health 
            (health 100 #:handler decrement-health))

          (define hero (entity hero-health))
        } 

  Another common usecase is that you'll want to update the whole entity that the component is attached to.  In that case, you'll use @racket[#:entity-handler]:

 
        @codeblock{
          (define-component health (amount))

          ;Simple function for updating the state of a health component
          (define (decrement-health h) 
            (health (sub1 (health-amount h))))

          (define (entity:decrement-health e h)
            (define new (update-entity e h decrement-health))

            (other-entity-updates new))

          ;Since the type of our update function is entity? health? -> entity?, we can use #:entity-handler
          (define hero-health 
            (health 100 #:entity-handler entity:decrement-health))

          (define hero (entity hero-health))
        } 

  Using @racket[define-component] uses the fields you give it to generate some nice convenience functions for building common handlers.
  For example, here's a cleaner way to build and attach a the same @racket[decrement-health] behaviour as above.

        @codeblock{
          (define-component health (amount))

          (define decrement-health (update-health-amount sub1))

          (define hero (entity (health #:handler decrement-health)))
        } 

  Here's the same idea, but using a generated function for creating an entity-handler:

        @codeblock{
          (define-component health (amount))

          (define decrement-health (update-entity-health-amount sub1)) 

          (define hero (entity (health #:entity-handler decrement-health)))
        } 

  Those last two examples are precisely the same.  You might use the second one, though, if you had more work to do on the entity -- in which case you might compose @racket[decrement-health] with other handlers (see @racket[do-many]) 


  Fuck, what about do-many?  Is that going to be gross now that we've abandoned game-functions as the norm?
     No.  Can just inspect and lift the functions as necessary, I think...
  

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


  In theory, you shouldn't use this method to modify the id of a component.  There's nothing explicitly preventing that, though, at the moment.  (Why would you want to anyway?)   
}




