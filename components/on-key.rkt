#lang racket

(require "../game-entities.rkt")


(provide (struct-out on-key))

(struct on-key (key f))

(define (update-on-key g e c)
 (if (button-change-down? (on-key-key c) g)
     ((on-key-f c) g e)
     e))

(new-component on-key?
               update-on-key) 