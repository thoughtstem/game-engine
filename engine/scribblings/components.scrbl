#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{components}
@author{thoughtstem}

@defmodule[game-engine/extensions/main]

@section{Rendering}

The rendering system takes a game and ticks it until the user terminates it. 

@defproc[(play! [game game?]) game?]{
  Pass in a game to rendering. 

  Any entities that have a @racket[position] component and a @racket[sprite]
  component will be rendered.  See examples below.

  These components are provided by ____.

  TODO: Add layer, rotation, scale, and coloration support...
}

DOC THIS BEAUTIFUL THING:
level swapping when some objective is complete 
  Manager
    -> Level
    -> Level
    -> Level
   
DOC THIS
warping from one game to another via links, 
  Is this different from a sequence (cycle) of levels?
  Yeah
  Manager
    -> World
       -> Next-World

  Manager watches some component on the game,
    When that component produces a game, the manager swaps to that new game.
      (Back buffer, bread crumbs: Could capture the last game and provide a way to pop back to it.)
 
OMG, IF WE CAN GET TO THIS TODAY...
implement hotswap-dev         
  Extension idea: one game throws error / other catches it and continues, 

