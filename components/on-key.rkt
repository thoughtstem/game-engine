#lang racket

(require "../game-entities.rkt")


(provide (except-out (struct-out on-key) on-key)
         (rename-out (make-on-key on-key)
                     (on-key       struct-on-key)
                     (on-key-rule? struct-on-key-rule)
                     (on-key-f     struct-on-key-f)))

(component on-key (key rule? f))

(define (make-on-key key #:rule [rule? (lambda (g e) #t)] f)
  (new-on-key key rule? f))

(define (update-on-key g e c)
 (if (and (button-change-down? (on-key-key c) g)
          ((on-key-rule? c) g e))
     ((on-key-f c) g e)
     e))

(new-component on-key?
               update-on-key) 