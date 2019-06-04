#lang racket

;Compile this

(define example `(+ 2 (* 2 5)))

;Into 

(define gamified-example
  (game
    (entity 
      (name '+) 
      (arguments
        (game
          (entity (name '2)
                  (value 2)) 
          (entity
            (name '*)
            (arguments
              (game
                (entity (name '2) (value 2))    
                (entity (name '5) (value 5))))))))))

;Then render the above

(define renderable-gamified-example
  ((game-traverse e->e
                  g->g)
   gamified-example))

;Thinking of a domain as a game has benefits.  You can turn it into one for free.  You can explore it as such.  That alone is a tool of great cognitive benefit.
(define (e->e e)
  (cond 
    [(function-entity? e) (render-function-entity e)]
    [(value-entity? e) (render-value-entity e)]))


(define (render-function-entity e)
  ;And add in any position stuff too...
  ;  But abstact that.  Or rather... abstract this tree traversal stuff.
  (update-component e 'arguments
                    (lambda (a)
                      (arguments 
                        (g->g (get-arguments a))))))


(define (g->g g)
  (apply game (map e->e (game-entities g))))


;WHen an traversal algorithm is finished, it can be used over in the game visualizer demo to traverse games to be visualized, creating new visualizing games from them.



