#lang racket

(provide play)

(require racket/match
         racket/fixnum
         racket/flonum
         lux

         (prefix-in ml: mode-lambda)
         (prefix-in ml: mode-lambda/static)
         (prefix-in gl: mode-lambda/backend/gl)
         (prefix-in ml: mode-lambda/text/runtime)
         posn)

(require "../../core/main.rkt"
         "./animated-sprite.rkt")

(struct demo ;TODO: CHANGE THIS NAME
  ( state render-tick)
  #:methods gen:word
  [(define (word-fps w)
     60.0)  
   (define (word-label s ft)
     (lux-standard-label "Values" ft))
   
   (define (word-output w)
     (match-define (demo state render-tick) w)
     (render-tick state))
   
   (define (word-tick w)
     (match-define (demo state render-tick) w)
     (demo (tick state) render-tick))])

(define (get-gui #:width [w 480] #:height [h 360])
  (define make-gui (dynamic-require 'lux/chaos/gui 'make-gui))
  (make-gui #:start-fullscreen? #f
              #:frame-style (if (eq? (system-type 'os) 'windows)
                                (list 'no-resize-border
                                      'no-caption)
                                (list 'no-resize-border) ;DON'T CHANGE THIS
                                )
              #:mode gl:gui-mode
              #:width w
              #:height h))


(define sd  (ml:make-sprite-db))
(define csd #f)

(define (init-db)
  (define to-compile (get-queued-sprites))

  (for ([i to-compile])
    (add-sprite! sd (first i) (second i)))

  (flush-queued-sprites!)

  (set! csd (ml:compile-sprite-db sd))
  csd)

(define (play g)
  (define W 400)
  (define H 400)

  (define render-tick (get-mode-lambda-render-tick g W H))

  (call-with-chaos
   (get-gui #:width W #:height H)
   (λ () (fiat-lux (demo g render-tick)))))

(define (get-mode-lambda-render-tick g W H)
  (define W/2 (/ W 2))
  (define H/2 (/ H 2))

  ;Initialize the compiled sprite database
  ;Use the entities, plus their sprites, to determine the initial sprite database

  (define csd (init-db))
(define layers (vector
		   (ml:layer (real->double-flonum W/2)
			     (real->double-flonum H/2))))


  ;Set up our open gl render function with the current sprite database
  (define ml:render (gl:stage-draw/dc csd W H 8))


  (define (render-game g)
    (displayln "RENDER")
    ;Find uncompiled entities...
    ;Recompile the database if we added anything:

    ;Create our sprites
    (define sprite-id (ml:sprite-idx csd 'sprite-1))

    (define dynamic-sprites (game->ml-sprite-list g))

    (define static-sprites (list))

    ;Actually render them
    (ml:render layers
	       static-sprites
	       dynamic-sprites))

  render-game)

(define (game->ml-sprite-list g)
  (define es (game-entities g)) 

  ;Assume one sprite for now.  Fix later.
  (define (entity->sprite e)
    (define sid
      (ml:sprite-idx csd 
                     (sprite-id (get-component e sprite?))))

    (ml:sprite #:layer 0
               (real->double-flonum (x e))
               (real->double-flonum (y e))
               sid))
 
  (map entity->sprite es) )

(require 2htdp/image)

;Adds to an uncompiled sprite database...
(define (add-sprite! db id-sym i)
  (ml:add-sprite!/value db id-sym i))

