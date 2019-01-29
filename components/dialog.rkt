#lang racket

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require 2htdp/image)

(require posn)

(provide (except-out (struct-out dialog) dialog)
         (rename-out (new-dialog dialog))
         set-dialog-index
         get-dialog-index
         get-dialog-sprites
         )

(component dialog (sprites index))

(define (update-dialog g e c) e)

(new-component dialog?
               update-dialog)


(define (set-dialog-index num)
  (lambda (g e)
    (update-entity e dialog? (struct-copy dialog (get-component e dialog?)
                                          [index num]))))

(define (get-dialog-index e)
  (dialog-index (get-component e dialog?)))

(define (get-dialog-sprites e)
  (dialog-sprites (get-component e dialog?)))

#|(define (current-dialog dialog)
  (define simple-dialog? (animated-sprite? (first (dialog-sprites dialog))))
  (define player-dialog-index (get-counter (get-entity "player" g))) ;(get-dialog-index (get-entity "player" g)))
  (if simple-dialog?
      (list-ref (dialog-sprites dialog) (
|#
