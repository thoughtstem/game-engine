#lang racket

(require "./crud.rkt")

(provide component-handler->game-handler
         entity-handler->game-handler)

(define (component-handler->game-handler ch)
  (lambda (g e c)
    (define new-e (update-component e c ch))
   
    (update-entity g e new-e)))

(define (entity-handler->game-handler eh)
  (lambda (g e c)
    (update-entity g e (curryr eh c))))
