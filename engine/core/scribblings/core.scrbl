#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{core}
@author{thoughtstem}

@defmodule[game-engine/engine/core/main]

TODO: Fun example: generating trance music...

TODO: Doc all the functions from define-component that we're going with.  Remove the ones we aren't.
TODO: Decide whether we're docing and testing the higher-order handlers at the end of this doc. 
  - Actually, implementing these as components makes more sense, I think.  (times ...) can just store the original child component state and return to it later...

Gives you a truly building-blocks approach to designing animations, simulations, and games.  Bottom up.  Easily create and share your own components, entities, games, partial games, game constructors, procedurally generated games, etc.  They're all just values and easily compose with one another.  

Manipulating games programatically is quite easy.

@codeblock{
  (define-health health (amount))
  (define e (entity (health 5 #:update (update-health-amount^ add1))))
  (define g (game e e e))
  (define g1 (tick g))
  (define g2 (tick g1))

  ;Note there's no renderer at this level.
}

That was relatively simple.  It's not even worth rendering -- though perhaps it could be a nice building block for a simulation.

However, when you import other stuff... 

@codeblock{
;TODO: More interesting example.
}

Roadmap: In any even, the attempt is to make a purely functional core with certain runtime guarantees.  Will that be slow?  Probably.  So we'll try to provide ways of switching off the purity, at the expence of losing the guarantees.  We'll make the compromises as they come up, and try to make them optional.

I think the potential for exploding a whole ecosystem of composable game parts will yeild really cool, innovative games, and bring game programming's difficulty down considerably.  If the games must be modest to make this a reality, so be it. :)

@section{Basic Data Structures}

@defproc[(game [entity entity?] ...)
         game?]{
  Constructor for a @racket[game].  Takes nothing but a list of entities.
}

@defproc[(entity [component component?] ...)
         entity?]{
  Constructor for a @racket[entity].  Takes nothing but a list of components.  Internally, though, entities have an id.  This id remains the same even as an entity is updated at runtime.  
}

@defform[(define-component name (fields ...))]{
  Creates a new component type named @racket[name] and the given @racket[fields].  Creating a component causes a bunch of useful functions to bedefined for you.

  The most obvious one is the constructor, which we shall show in context below.

  A common usecase is that you'll want to bind a function that takes and returns something of your component type:

        @codeblock{
          (define-component health (amount))

          ;We use update-health-amount, which was created for us.
          ;This lets us define decrement-health in point-free form.
          ;The function's type is health? -> health?
          (define decrement-health (update:health/amount^ sub1))

          ;Since the type of our update function is health? -> health?, we can use #:handler in our constructor
          (define hero-health (health 100 #:update decrement-health))

          ;Now it can be attached to an entity, and so on...
          (define hero (entity hero-health))
        } 


  Much like using racket @racket[struct], you get getters and setters (which are immutable by default).  Getters look like @racket[COMPONENT-FIELD].  Setters look like @racket[set-COMPONENT-FIELD].   And you get a predicate @racket[COMPONENT?]. 

  Here we'll use those to define a component and new functions on that component type.
  
  @codeblock{
    (define-component counter (current max))

    ;Takes and r
    (define/contract (inc c)
      (-> counter? counter?)
      (set-counter-current 
        (add1 (counter-current c))))

    (define/contract (safe-inc c)
      (-> counter? counter?)
      (if (< (counter-current c)
             (counter-max c))
          (inc c)
          c))
  }

  In addition to getters and setters, you also get functions that help you deal with your new component in the context of entities it might be attached to.  

} 
 

@defproc[(update:COMPONENT/FIELD [c component?] [f any/c]) component?]{
  If @racket[f] is a function, applys @racket[f] to the @racket[FIELD] field of @racket[c].  Otherwise, sets @racket[FIELD] to that value. 
}

@defproc[(update:COMPONENT/FIELD^ [f any/c]) handler?]{
  Handler version the above.  For use when your component is attached to an entity.  

  @codeblock{
    (define-component health (amount))
    (entity (health 5 #:update (update:health/amount^ sub1)))
  }

  Essentially, it's a delayed version of @racket[update:COMPONENT/FIELD] -- one that will be applied to whatever component's @racket[#:update] it is attached.


  TODO: Currently, these cannot be attached to other components on the same entity...  Should we allow that?  Or should it be a different function.  See test in test/define-component.rkt
}

@defproc[(update:COMPONENT/FIELD^ [f any/c]) handler?]{
  Handler version the above.  For use when your component is attached to an entity.  

  @codeblock{
    (define-component health (amount))
    (entity (health 5 #:update (update:health/amount^ sub1)))
  }

  Essentially, it's a delayed version of @racket[update:COMPONENT/FIELD] -- one that will be applied to whatever component's @racket[#:update] it is attached.
}



  @racket[update-COMPONENT-FIELD] -- e.g. @racket[update-health-amount]. 
  This function is not a handler, but it builds handlers when given a function that takes and returns a values of your field's type.  Since health amount is a number, you can create handlers like @racket[(update-health-amount sub1)], which expresses that the health will decrease whenever the handler runs.

  Moving up the handler heirarchy, you also get a handler that works the same way:

  @racket[update-entity-COMPONENT-FIELD] -- e.g. @racket[update-entity-health-amount]. 

  The main difference is that one handler will return a @racket[operation?] that is a component-changing operation, whereas the other is one that returns an entity-changing operation.   

  If you can do the same thing with an entity handler and a component handler, which should you pick?  The rule of thumb is always: Stay as low in the heirarchy as possible.  If you can use a component handler, do that.  It'll be faster.  If you must make other adjustmenst to the entity, use an entity handler.

  The above two handler builders are for operating on a particular field of a component.  But it'll be quite common that you'll want to operate on an entity that the component is attached to -- i.e. to affect other components on that entity.  (Yes, components can change each other.)

  @codeblock{
    (define-component counter (amount))
    (define-component other-counter (amount))

    (entity 
      (counter       0 #:update (update-counter-amount add1))
      (other-counter 0 #:update (update-entity-counter-amount (curry + 5))))
  }
  
  Both update handlers are changing the same counter -- one with a component operation (which affects the normal counter because it's attached to it).  The other does so with an entity operation, which it must do -- because a component operation would update itself.

  Here's a less abstract example:

  @codeblock{

          (define-component dead ())
                  (define is-zero? (curry = 0))

                  (define e
                   (entity 
                    (health 3 #:update (compose-handlers
                                        (update-health-amount sub1)
                                        (on-rule 
                                         (entity-health-amount? is-zero?) 
                                         (compose-handlers
                                          (add-component (dead))
                                          (remove-self)))))))
  }

  We attach a health component that counts down over time, and will remove itself when it gets to zero -- but not before adding a component named dead (presumably for some other entity or component to do further handling with).





You can control games programmatically with @racket[init] and @racket[tick].  The idea is that you do a single call to @racket[init] followed by as many calls to @racket[tick] as you want.

@defproc[(init [game game?])
         game?]{
  Does any necessary setup on a game.  This includes enforcing properties like uniqueness of ids across entities and components.

  When we have features like @racket[#:on-start], they would execute at this time.
}

@defproc[(tick [game game?])
         game?]{
  The returned game is the previous game, advanced one tick.

  The simple runtime model is that the engine loops over every entity and every component in order, giving each component a chance to run its handlers.  Each component may have a component handler, an entity handler, and/or a game-hander attached.  These handlers run in the aforementioned order -- meaning that the game-handler takes precedience because it runs last.

  TODO: As we inevitably encounter the need to make ticks faster, we'll adjust the above simple model with various optimizations.  If those optimizations could cause any of the game's immutability guarantees to break, they will be optional.  My hope is that developers can begin with the simple immutable model as they are prototyping and unit testing their games.  Then they can increase the speed when necessary (basically by adding more mutability).
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


@section{Handlers}

Working with handlers is most of what you do as you develop in game-engine.  There are various combinators for building more sophisticated handlers from simpler ones.  And also just utilities for working with handlers in the first place.

The nice thing about game-engine is that you can express your logic however you want, in whatever language you want.  As long as you can make a handler out of it, you can run it in game-engine, and compose your functionality with the rest of the handler ecosystem.

@defthing[handler? (-> game? entity? component? operation?)]{
  Semantically, when the engine executes a @racket[handler?], it always does so with three things in its context: the mid-tick state of the component to which the handler is attached, the mid-tick state entity to which that component is attached, and the pre-tick game state of the game.

  The idea behind these input values is that, during a tick: 1) entities don't get to see the changes that other entities are making to themselves or to each other, 2) but they do get to see the changes that they themselves are making to themselves.   It just seems fair, right?  I get full knowlege of what I'm doing to myself.  But no one else gets to know until we all do (at the end of the tick).

  The return type is an @racket[operation?] which expresses a change to @racket[e].  Currently, it can either be a @racket[entity?] or a @racket[component?].  If it is an entity, this will be the new value of @racket[e] as the tick continues on to the next component after @racket[c], or (if @racket[c] was the last component on @racket[e]) it will continue on to the next entity after @racket[e].  Or, if there are no more entities, the tick will be complete and the next tick will begin.
}

@defproc[(compose-handlers [input handler?] ... ) handler?]{
  Equivalent to what would happen if each of @racket[input] were on separate components on the same entity:

  @codeblock{
    (entity (new-component #:update i1)
            (new-component #:update i2)
            (new-component #:update i3)
            (new-component #:update i4)) 
  }

  Is equivalent to:


  @codeblock{
    (entity (new-component #:update (compose-handlers i1 i2 i3 i4)))
  }

  Except that any component-level changes will all be applied to the same component.  Any entity level changes are as before.
}

Note that handlers, because they can return an entire entity, can by definition update other components on the same entity.  Any component, in fact, can update any other component (not that it would be a good idea to make complicated updating semantics for your game logic).  Mainly, it just gives you the freedom to do either: 

@codeblock{
  (define-component health (amount))

  (define (entity (health 100 #:update (update:health/amount^ sub1)))) 
}

Or:

@codeblock{
  (define-component health (amount))

  (define (entity (health 100)
                  (new-component #:update (update:health/amount^ sub1)))) 
}

Handlers are portable at the entity level, provided that you have only one component of type @racket[health?].

Another thing you can do with handlers is pass them to functions that return other handlers.

@codeblock{
  (define-component health (amount))
  (define reduce-health (update-health-amount sub1))

  (define (entity (health 100 
                          #:handler reduce-health ))) 
}

This entity will lose one health per tick (and there's nothing stopping it from going negative -- but that's not the point, right now).   But you can use the @racket[ticks] combinator to change that from the (by default) infinite handler to one that only happens once.

@codeblock{
  (define-component health (amount))
  (define reduce-health (update-health-amount sub1))

  (define (entity (health 100 
                          #:handler (for-ticks 1 reduce-health)))) 
}



;TODO: Should these higher order handler functionalities actually be implemented as components (extensions)?  


@defproc[(for-ticks [n number?] [h handler?]) handler?]{
  The handler (wrapped in the producer) returned by this function will behave like @racket[h] for @racket[n] ticks.  After that, it will behave like a noop. 

  Note that this has the ability to "cut short" a handler that might have much longer term behaviour.   @racket[(ticks 1 h)] can turn an infinite producer into what feels like a one-shot.
}

@defproc[(for-ticks! [n number?] [h handler?]) handler?]{
  The handler (wrapped in the producer) returned by this function will behave like @racket[h] for @racket[n] ticks.  After that, it will remove itself from the component.

  Note that this has the ability to "cut short" a handler that might have much longer term behaviour.   @racket[(ticks 1 h)] can turn an infinite producer into a literal one-shot -- only attached to the component for one tick before marking the component as "done"

  This is mainly useful with the @racket[times] function or the @racket[sequence] function.
}


@defproc[(times [n number?] [h handler?]) handler?]{
  Returns whatever the input handler returns until it tries to register the component as "done" (by setting its update handler to @racket[#f]).  It does this n times.  

  After that, it becomes a noop forever.
}

@defproc[(times! [n number?] [h handler?]) handler?]{
  Returns whatever the input handler returns until it tries to register the component as "done" (by setting its update handler to @racket[#f]).  It does this n times.  

  After that, it tries to mark the component as "done".
}

@defproc[(sequence [input handler?] ...) handler?]{
  Does each input handler until it tries to mark the component as done, after which it proceeds to the next.  After that it behaves like a noop.
}

@defproc[(sequence! [input handler?] ...) handler?]{
  Does each input handler until it tries to mark the component as done, after which it proceeds to the next.  After that it tries to mark the component as done.  
}

@defproc[(after-ticks [n number?] [h handler?]) handler?]{
  For n ticks, behaves like a noop.  Then, it behaves like h. 
}


