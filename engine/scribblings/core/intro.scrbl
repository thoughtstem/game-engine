#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{intro}
@author{thoughtstem}

;game-engine v2 : Get to "release"
;  - Put online with new name, new repo
;  - Lindsey can use it for the planes
;  - Can use it for animations and side-projects                        
;  - Can publish about it           
;  - Can start letting it replace game-engine throughout the ts languages
;  All of these are out of scope until it is released                   
;  Given that, I can probably put a timeline on the project completion. 

;free up evenings for new side project (or double up on writing or work projects) 

Multi-lingual game programming.  Many languages, all entity/component systems beautifully composed

Gives you a truly building-blocks approach to designing animations, simulations, and games.  Bottom up.  Easily create and share your own components, entities, games, partial games, game constructors, procedurally generated games, etc.  They're all just values and easily compose with one another.  

Manipulating games programatically is quite easy.

That was relatively simple.  Three entities that start at 5 health and gain health at a rate of 1 per tick.  
It's not even worth rendering -- though perhaps it could be a nice building block for a simulation.

However, when you import other stuff... 

