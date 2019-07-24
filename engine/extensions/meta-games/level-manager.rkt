#lang racket

(provide level-manager)

(require "../../core/main.rkt"
         "../rendering/animated-sprite.rkt"
         "../common-components/main.rkt")

(define-component level-stream stream?)
(define-component sub-game-complete? boolean?)
(define-component sub-game-complete-changed? boolean?)

(define (should-advance?)
  (and (get-sub-game-complete?)
       (get-sub-game-complete-changed?)))

(define (level-manager levels-str level-complete?)
  (entity
    (name 'level-manager)

    (counter 0 (^ add1))

    (sub-game-complete? #f
                        (level-complete? (get-sub-game)))

    (sub-game-complete-changed? #f
                                (observe-change 
                                  (level-complete? (get-sub-game))))

    (level-stream levels-str
                  (if (should-advance?)
                    (stream-rest (get-level-stream))
                    (get-level-stream)))

    (sub-game (stream-first levels-str)
              (if (should-advance?)
                (stream-first (get-level-stream))
                (tick (get-sub-game))))

    (also-render (stream-first levels-str) 
                 (get-sub-game))))
