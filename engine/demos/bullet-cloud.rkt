#lang racket

(provide g)

(require game-engine
         2htdp/image)

;I get 25 FPS with 1000 bullets on my low-end chromebook.  I think that's fine for now.

(define num-bullets 1000)

(define bullet-data
  (build-vector num-bullets 
                (thunk* (vector 200 200 1))))

(define bullet-sprite (register-sprite (circle 6 'solid 'green)))

(define (bullet n)
  (entity
    (sprite bullet-sprite)
    (size (thunk* (vector-ref (vector-ref bullet-data n)
                              2)))  
    (position (thunk* 
                (posn
                  (vector-ref (vector-ref bullet-data n)
                              0)
                  (vector-ref (vector-ref bullet-data n)
                              1))))))

(define (random-walk-position p)
  (+ p (list-ref '(1 -1)
                 (random 2))))

(define (random-walk-size s)
  (+ s (list-ref '(0.01 -0.01)
                 (random 2))))

(define (vector-update! v i f)
  (vector-set! v i 
               (f (vector-ref v i))))

(define (update-bullet-data)
  (for ([d bullet-data])
    (vector-update! d 
                    0 
                    random-walk-position)
    (vector-update! d 
                    1
                    random-walk-position)
    (vector-update! d 
                    2
                    random-walk-size)))

(define-component bullet-data-update void?)

(define g
 (game
  (entity
   (bullet-data-update (void) (update-bullet-data))) 

  (entity
   (also-render
    (game
     (map bullet (range num-bullets)))))))


(module+ main
 (play! g))






