#lang racket

(require "../game-entities.rkt")


(provide (rename-out (make-on-key on-key))
         on-key?)

(struct on-key (key rule? f))

(define (make-on-key key #:rule [rule? (lambda (g e) #t)] f)
  (on-key key rule? f))

(define (update-on-key g e c)
 (if (and (button-change-down? (on-key-key c) g)
          ((on-key-rule? c) g e))
     ((on-key-f c) g e)
     e))

(new-component on-key?
               update-on-key) 