#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{core/base}
@author{thoughtstem}

@defmodule[game-engine/engine/core/main]
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

