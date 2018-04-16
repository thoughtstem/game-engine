#lang racket

(require "./game-engine.rkt")

(define (test-character c)
  (define s  (c 'none))
  (define ff (render s))
  
  (define e
    (sprite->entity s
                  #:position   (posn (image-width ff)
                                     (image-width ff))
                  #:name       "test"
                  #:components (key-animator 'none c)))
  
  (start-game e (sample-bg (* 2 (image-width ff)))))

(define (test-sprite s)
  (define ff (render s))
  
  (define e
    (sprite->entity s
                  #:position   (posn (image-width ff) (image-width ff))
                  #:name       "test"))
  
  (start-game e (sample-bg (* 2 (image-width ff)))))

(define (sample-bg w)
  (define bg-sprite
          (sheet->sprite (square w "solid" "black")
                 #:rows        1
                 #:columns     1
                 #:row-number  1
                 #:speed       1))
  (sprite->entity bg-sprite
                  #:position   (posn 0 0)
                  #:name       "bg"))
