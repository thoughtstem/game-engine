#lang racket

(provide play play!)

(require racket/match
         racket/fixnum
         racket/flonum
         lux

         (prefix-in ml: mode-lambda)
         (prefix-in ml: mode-lambda/static)
         (prefix-in gl: mode-lambda/backend/gl)
         (prefix-in ml: mode-lambda/text/runtime)
         lux/chaos/gui/key)

(require "../../core/main.rkt"
         "./animated-sprite.rkt")

(struct game+render ;TODO: CHANGE THIS NAME
  ( state render-tick)
  #:methods gen:word
  [(define (word-fps w)
     60.0)  
   (define (word-label s ft)
     (lux-standard-label "Values" ft))
   
   (define (word-output w)
     (match-define (game+render state render-tick) w)
     (render-tick state))

   (define (word-event w e)
     (cond
       [(or (eq? e 'close)
            (and (key-event? e)
                 (eq? (send e get-key-code) 'escape)))
        #f
        ]
       [else w]
       ))
   
   (define (word-tick w)
     (match-define (game+render state render-tick) w)
     (game+render (tick state) render-tick))])

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

(define (play! g)
  (mutable! (play g)))

(define (play g)
  (define W 400)
  (define H 400)

  (define render-tick (get-mode-lambda-render-tick g W H))

  (call-with-chaos
   (get-gui #:width W #:height H)
   (λ () (fiat-lux (game+render g render-tick)))))

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
    ;Find uncompiled entities...
    ;Recompile the database if we added anything:

    ;Create our sprites
    (define sprite-id (ml:sprite-idx csd 'sprite-1))

    (define dynamic-sprites 
      (game->ml-sprite-list g))

    (define static-sprites (list))

    ;Actually render them
    (ml:render layers
	       static-sprites
	       dynamic-sprites))

  render-game)

(define sprite-cache
  (make-hasheq))

(require (prefix-in h: 2htdp/image))
(define dummy (sprite (h:circle 5 'solid 'blue)))

(define (game->ml-sprite-list g)
  (define ret '())
  (define hits 0)

  (for ([e (game-entities g)])
    ;How much slowdown from get-component vs from ml:sprite vs from just looping over everything...?
    ;vs the x and y getters?
    (define cs (entity-components e))
    (define s 
      ;TODO: this is not going to generalize as is....    

      #;
      (if (> (length cs) 2)
        (list-ref cs 2)
        #f)

      (get-component e sprite?) 

      )

    ;When there's a sprite? component
    (when s 
      (define eid (entity-id e))
      (define sid (ml:sprite-idx csd (sprite-id s)))


      (define mls
        #;
        (ml:sprite #:layer 0
                   (real->double-flonum (x e))
                   (real->double-flonum (y e))
                   sid)
        (if (and (hash-has-key? sprite-cache eid)
                 (not (entity-changed? e)))
          (begin
            #;
            (set! hits (add1 hits))

            (hash-ref sprite-cache eid))
          (let ([new-mls (ml:sprite #:layer 0
                                    (real->double-flonum (x e))
                                    (real->double-flonum (y e))
                                    sid)])


            new-mls)))

      (hash-set! sprite-cache eid mls)
      (set! ret (cons mls ret))) 
    )


  ret
  

  )


(require 2htdp/image)

;Adds to an uncompiled sprite database...
(define (add-sprite! db id-sym i)
  (ml:add-sprite!/value db id-sym i))

