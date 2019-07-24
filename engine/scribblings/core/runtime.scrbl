#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{runtime}
@author{thoughtstem}

@defproc[(tick [game game?])
         game?]{
  The returned game is the previous game, advanced one tick.

  It does this by ticking every component on every entity in the order that they are listed.  During the tick, if any @racket[spawn] or @racket[despawn] values are produced by components, the runtime will take the appropriate actions AFTER all of the components have been ticked.

  By default, the game is copied -- not mutated.  To remove this guarantee, run within @racket[(mutable! ...)].
}

@defproc[(tick-entity [entity entity?]) entity?]{
  Ticks all of the components on an entity.  If any @racket[spawn] or @racket[despawn] values are produced, they have no effect.
}

@defproc[(tick-component [component component?]) component?]{
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
