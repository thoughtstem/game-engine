#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@(require scribble/lp-include)

@title{game-engine v2}
@author{thoughtstem}

Gotta pick a better name

In part, we'll need to build some kind of side project to really be sure... And that will help keep things organized, plus provide a body of example code to help seed the community.   Start making an RPG example -- heading toward the planes or one of the TS-Langs. 
Let's document that, literate code style!
Rebuid this doc, start this section over.

This doc is (opinionatedly) backwards, starting with a motivating example.  The API docs are at the end.

@lp-include["./rpg-demo.scrbl"]

@include-section["./core/main.scrbl"]
@include-section["./components.scrbl"]
@include-section["./issues.scrbl"]
