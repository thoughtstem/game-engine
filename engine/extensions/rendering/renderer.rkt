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
         "../common-components/main.rkt" 
         "./animated-sprite.rkt")

(provide buttons)

;TODO: This needs to get fleshed out better.  And probably needs to get handled in extensions/input/
;  so that we can better separate rendering from input.
(define buttons
  (hash
    #\a #f
    #\d #f
    #\w #f
    #\s #f))

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

       [(and (key-event? e)
             (eq? 'press 
                  (send e get-key-release-code))
             )

        (begin
          (set! buttons
            (hash-set buttons
                      (send e get-key-code)
                      #t))

          w)]

       [(and (key-event? e)
             (eq? 
               'release
               (send e get-key-code)))

        (begin
          (set! buttons
            (hash-set buttons
                      (send e get-key-release-code)
                      #f))

          w)]

       [(or (eq? e 'close)
            (and (key-event? e)
                 (eq? (send e get-key-code) 'escape)))
        (begin
          (display-performance-stats)
          #f)
        ]
       [else w]
       ))
   
   (define (word-tick w)
     (match-define (game+render state render-tick) w)
     (define new-state (tick state))

     (game+render new-state 
                  render-tick))])

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


(define recompiled #f)
(define csd #f)

(define (init-db)
  (define sd  (ml:make-sprite-db))
  (define to-compile (get-queued-sprites))

  (for ([i to-compile])
    (add-sprite! sd (first i) (second i)))

  #;
  (flush-queued-sprites!)

  (set! recompiled #t)
  (set! csd (ml:compile-sprite-db sd))
  csd)

(define (play! g)
  (mutable! (play g)))

(define (play g)
  (define W 400)
  (define H 400)

  (define render-tick (get-mode-lambda-render-tick W H))

  (call-with-chaos
   (get-gui #:width W #:height H)
   (λ () (fiat-lux 
            (game+render g render-tick))))) 

(define (new-layer w h)
  (ml:layer (real->double-flonum w)
            (real->double-flonum h)))


(define (get-mode-lambda-render-tick W H)
  (define W/2 (/ W 2))
  (define H/2 (/ H 2))

  ;Initialize the compiled sprite database
  ;Use the entities, plus their sprites, to determine the initial sprite database

  (define layers (vector
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)
                   (new-layer W/2 H/2)))

  ;Set up our open gl render function with the current sprite database

  (define ml:render #f)

  (init-db) ;Initializes csd

  (define (render-game g)
    (set! ml:render 
      (if recompiled
        (begin
          (set! recompiled #f)
          (gl:stage-draw/dc csd W H 8))
        ml:render))

    ;Find uncompiled entities...
    ;Recompile the database if we added anything:
    (define dynamic-sprites 
      (game->ml-sprite-list g))

    (define static-sprites (list))

    ;Actually render them
    (ml:render layers
               static-sprites
               dynamic-sprites))

  render-game)

(require (prefix-in h: 2htdp/image))


(define (game->ml-sprite-list g)
  (define ret '())

  (for ([e (game-entities g)])
    (define s (get-component e 'sprite))

    (when s 
      (define eid (entity-id e))

      (when new-sprites-added 
        (displayln "Recompiling sprite db")
        (init-db) ;Try reinitializing the database, pulling from the sprite queue again
        )

      (define sid 
        (ml:sprite-idx csd (call-if-proc (sprite-id s))))


      ;Note.  Where there are many entities (several hundred), all of these get-*s add up.  Consider looking for optimizations.  (However, it is also worth noting that rendering is usually not the first bottleneck.  Only after certain optimizations -- e.g. demos/bullet-cloud.rkt -- does rendering become the bottleneck)
      (define p (call-if-proc (get-position e (posn 0 0))))

      (define mls
        (ml:sprite #:layer (call-if-proc (get-layer e 0))
                   #:m (real->double-flonum (call-if-proc (get-size e 1)))
                   #:theta (real->double-flonum (call-if-proc (get-rotation e 0)))
                   #:a (real->double-flonum (call-if-proc (get-transparency e 1))) 
                   (real->double-flonum (posn-x p))
                   (real->double-flonum (posn-y p))
                   sid))

      (set! ret (cons mls ret))) 
    

    (define a
      (get-component e 'also-render))

    (when a
     (set! ret (append (game->ml-sprite-list (get-value a)) ret))  

      ))


  ret
  

  )

(define (call-if-proc p)
  (if (procedure? p) (p) p))


(require 2htdp/image)

;Adds to an uncompiled sprite database...
(define (add-sprite! db id-sym i)
  (ml:add-sprite!/value db id-sym i))

