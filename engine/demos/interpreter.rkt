#lang racket

(require game-engine
         2htdp/image)

;TODO: Make this actually work

;Compile this (Make it a macro???)

(define example `(+ 2 (* 2 5)))

(define-component value any/c)
(define-component arguments game?)

;Into 

(define gamified-example
  (game
    (entity 
      (name '+) 
      (value +)
      (arguments
        (game
          (entity (name '2)
                  (value 2)) 
          (entity
            (name '*)
            (value *)
            (arguments
              (game
                (entity (name '2) (value 2))    
                (entity (name '5) (value 5))))
            (also-render (game) (get-arguments))
            )))
      (also-render (game) (get-arguments)))))

;Then render the above


(define (renderable-gamified-example)
  (game 
    (map e->e 
         (game-entities gamified-example))))

(define (function-entity? e)
  (not (not
         (has-component e 'arguments))))

(define (value-entity? e)
  (not
    (function-entity? e)))


(define (renderable-function-entity e)
  ;TODO: Convert this to a game with sub entities for fg and bg 
  (add-components e
                  (sprite (register-sprite (text (~a (get-name e)) 24 'red)))
                  (position (posn 200 200))))

(define (renderable-value-entity e)
  (add-components e
                  (sprite (register-sprite (text (~a (get-name e)) 24 'green)))
                  (position (posn 200 200))))


(define (e->e e (f g->g))
  (cond 
    [(function-entity? e) (traverse (renderable-function-entity e) 
                                    f)]
    [(value-entity? e)    (renderable-value-entity e)]))

(define (traverse e f)
  ;And add in any position stuff too...
  ;  But abstact that.  Or rather... abstract this tree traversal stuff.
  (set-arguments e 
                 (g->g (get-arguments e))))


(define (g->g g)
  (apply game (map e->e (game-entities g))))


;WHen an traversal algorithm is finished, it can be used over in the game visualizer demo to traverse games to be visualized, creating new visualizing games from them.


(play! (renderable-gamified-example)) 


