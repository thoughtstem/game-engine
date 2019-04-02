#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{core}
@author{thoughtstem}

@defmodule[game-engine/engine/core/main]

TODO: Make the handlers return leaner game operations (CRUD OPs).  '(update (e 1)
                            (remove (c 0)))
   Make the op language and application functions
   Redo our existing crud functions to use ops

   Can we make the ops hidden to the user?  Can they learn an API that seems purely functional (crud functions take and return games, entities, and components).  But then, under the hood, can we not call these functions but rather call our op functions instead?
   (Let's not get sidetracked by that, though.  It's not that bad to expose the ops to the user.)

ACTUALLY, I don't think we need crud ops so much as we need lazy evaluation -- but only on games, entities, and compoents and their crud operations.  Maybe the just need a field for queued up changes.  Then they all happen at some point in the future -- on the tick.



TODO: Drop the various handler keywords and just use @racket[#:handler].

TODO: Doc the rest of the cool handlers.

Gives you a truly building-blocks approach to designing animations, simulations, and games.  Bottom up.  Easily create and share your own components, entities, games, partial games, game constructors, procedurally generated games, etc.  They're all just values and easily compose with one another.  

Manipulating games programatically is quite easy.

@codeblock{
  (define e (entity (new-health 5 #:entity-handler entity:gain-health)))
  (define g (initialize-game (game e e e)))
  (define g2 (tick g))
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
          (define decrement-health (update-health-amount sub1))

          ;Since the type of our update function is health? -> health?, we can use #:handler in our constructor
          (define hero-health (health 100 #:handler decrement-health))

          ;Now it can be attached to an entity, and so on...
          (define hero (entity hero-health))
        } 

  Another common usecase is that you'll want to update the whole entity that the component is attached to.  In that case, you'll use @racket[#:entity-handler]:

 
        @codeblock{
          (define-component health (amount))

          ;We can update the health by updating the entity.
          ;  The type of this function is entity? health? -> entity?
          (define decrement-health (update-entity-health-amount sub1))

          ;Since the type of our update function is entity? health? -> entity?, we can use #:entity-handler
          (define hero-health (health 100 #:entity-handler decrement-health))
        } 

  @racket[define-component] uses the fields you give it to generate some nice convenience functions.

  Much like using racket @racket[struct], you get getters and setters (which are immutable by default).  Getters look like @racket[COMPONENT-FIELD].  Setters look like @racket[set-COMPONENT-FIELD].   And you get a predicate @racket[component?]. 

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

  In addition to getters and setters, you also get, for each field in your component, a builder for a @racket[component-handler?] function that looks like:

  @racket[update-COMPONENT-FIELD] -- e.g. @racket[update-health-amount]. 
  This function is not a component handler, but it builds component handlers when given a function that takes and returns a values of your field's type.  Since health amount is a number, you can create component handlers like @racket[(update-health-amount sub1)], which expresses that the health will decrease whenever the handler runs.

  Moving up the handler heirarchy, you also get an entity handler that works the same way:

  @racket[update-entity-COMPONENT-FIELD] -- e.g. @racket[update-entity-health-amount]. 

  Likewise, you must give this function a function that would adjust one of your fields, and it gives you an entity hander function (suitable for use with @racket[#:entity-handler]).

  If you can do the same thing with an entity handler and a component handler, which should you pick?  The rule of thumb is always: Stay as low in the heirarchy as possible.  If you can use a component handler, do that.  It'll be faster.  If you must make other adjustmenst to the entity, use an entity handler.

  The above two handler builders are for operating on a particular field of a component.  But it'll be quite common that you'll want to operate at the level of an entire component. 

  The example counter component above has some @racket[(-> counter? counter?)] functions already defined.  These could be passed into @racket[#:handler] directly, or they could be lifted to entity handlers with the @racket[update-entity-COMPONENT] function.

  @codeblock{
    (entity (counter #:entity-handler (update-entity-counter safe-inc)))
  }
  
  That's the same as the more efficient:

  @codeblock{
    (entity (counter #:handler safe-inc))
  }

  TODO: Document functions like update-entity-first-component -- e.g. less targeted 
} 


We've been talking about the handler heirarchy.  Here is a more formal listing of the three handler types:

@defthing[game-handler? (-> game? entity? component? game?)]{
  The most powerful and slowest.  This essentially has read access to the entire game state and returns a new game state.  You can attach these through a component's @racket[#:game-handler] keyword.
}

@defthing[entity-handler? (-> entity? component? entity?)]{
  Signature for the kind of function you would attach to your components with @racket[#:entity-handler], and which returns the next desired state for that @racket[entity].
}

@defthing[component-handler? (-> component? component?)]{
  Signature for the kind of function you would attach to your components with @racket[#:entity], and which returns the next desired state for that @racket[component].
}


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

We've looked at the basic handler heirarchy: @racket[component-handler], @racket[entity-handler], and @racket[game-handler].  But really, these are all members of a more general type:


@defthing[handler? (-> game? entity? component? operation?)]{
  Like a @racket[game-handler], but returns a more general type -- basically anything that the other handlers types can return, plus the additional values @racket['noop] and @racket['done].  Or it could be a list of the aforementioned things.

  Semantically, when the engine executes a @racket[handler?], it always does so with three things in its context: a game, an entity, and a component.  If the handler returns one of these, the corresponding input of that type is updated.  If the handler returns @racket['noop], this means no changes occur.  A @racket['done'] is like a noop, but means that the handler will no longer produce game, entity, and component values.  A list of these things means: apply the operations in the given order.  
}

@defthing[operation? (or/c game? entity? component? 'noop 'done (list (or/c game? entity? component? 'noop 'done))) ]{
  Return type for a @racket[handler?].
}

@defproc[(compose-handlers [input handler?] ... ) handler?]{
   A returns a handler that, when called, produeces a list of the return values that the input handlers would have produced.  
}



The fun thing with handlers is using them to change the behaviour of other handlers.  Suppose you have some function you've written or generated.  For example:

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


@defproc[(for-ticks [n number?] [h handler?]) handler?]{
  The handler (wrapped in the producer) returned by this function will behave like @racket[h] for @racket[n] calls.  After that, it will return @racket['done].

  Note that this has the ability to "cut short" a handler that might have much longer term behaviour.   @racket[(ticks 1 h)] can turn an infinite producer into a one-shot.
}

Another useful one is @racket[times], but it only makes sense to use this one with non-infinitely producing handlers.  It will run some handler to completion some number of times.  So obviously an infinite handler never completes, so times will be infinite too. 

But suppose we've been using @racket[for-ticks] to create handlers that do complete:

@codeblock{
  (define-component health (amount))
  (define reduce-health (update-health-amount sub1))
  (define gain-health   (update-health-amount add1))
  (define lose-1-health (for-ticks 1 reduce-health))
  (define gain-1-health (for-ticks 1 gain-health))

  (define (entity (health 100 
                          #:handler 
                          (times 2 gain-1-health)))) 
}

That's kind of cool.  We started with the auto generated handler @racket[update-health-amount] to describe an infinite change over time.  Then we cut it short with @racket[for-ticks], and we elongated it again with @racket[times].  (Of course, we could have just used @racket[(for-ticks 2 gain-health)] to get the same effect.  But this example is just for educational purposes.)







@defproc[(do-many [h (or/c component-handler? entity-handler? game-handler?)] ...)
         game-handler?]{
  Given an arbitrary number of handlers, returns a handler that runs each in sequence (meaning that the later handlers may overwrite changes from previous ones).

  TODO: Currently this function always returns a game handler.  But I think we should make it more polymorphic than that.  That is: @racket[(do-many ch1 ch2 ch3)] would return a @racket[component-handler?] if all the inputs were component handlers.  If one were an @racket[entity-handler?], though, then this would be the return type.  In other words, it would do the minimal amount of function lifting.
}



@defproc[(component-handler->game-handler [component-handler component-handler?])
         game-handler?]{
  Lifts a component handler to a game handler.  
}

@defproc[(entity-handler->game-handler [entity-handler entity-handler?])
         game-handler?]{
  Lifts an entity handler to a game handler.  
}



