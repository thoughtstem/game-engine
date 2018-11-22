#lang racket

(provide lux-start
         final-state)

(require racket/match
         racket/fixnum
         racket/flonum
         lux
         lux/chaos/gui
         lux/chaos/gui/val
         (prefix-in lux: lux/chaos/gui/key)
         (prefix-in lux: lux/chaos/gui/mouse)

         (prefix-in ml: mode-lambda)
         (prefix-in ml: mode-lambda/static)
         (prefix-in gl: mode-lambda/backend/gl))

(require "./core.rkt")
(require "../components/animated-sprite.rkt")

(define (lux-start larger-state)
  (define render-tick (get-mode-lambda-render-tick (game-entities larger-state)))

  
  (call-with-chaos
   (make-gui #:start-fullscreen? #f #:mode gl:gui-mode)
   (λ () (fiat-lux (demo larger-state render-tick)))))




(define (get-mode-lambda-render-tick original-entities)
  ;Assume the last entity is the background entity
  (define bg-entity (last original-entities))

  ;Use the background to setup some helpful constants
  (define W (w bg-entity))
  (define H (h bg-entity))
  (define W/2 (/ W 2))
  (define H/2 (/ W 2))


  ;Use the entities, plus their sprites, to determine the initial sprite database
  (define csd (entities->compiled-sprite-database original-entities))

  ;Define that we'll have one layer of sprites (for now).
  ;  Fix it's position at the width and height of the game (WHY NOT 0,0?)
  (define layers (vector (ml:layer (real->double-flonum W)
                                   (real->double-flonum H))))

  (define compiled original-entities)

  (define ml:render (gl:stage-draw/dc csd W H 8))
  
  (define (ticky-tick)
    
    ;Find uncompiled entities...
    (define uncompiled (if (not last-game-snapshot)
                           '()
                           (filter-not (curryr member compiled entity-eq? #;sprite-eq?)
                                       (game-entities last-game-snapshot))))


    ;Recompile the database if we added anything:
    (and (not (empty? uncompiled))
         (let ([sd2 (ml:make-sprite-db)])
           (for ([sprite-entity (in-list (append compiled uncompiled))])
             (and (get-component sprite-entity animated-sprite?)
                  (add-entity! sd2 sprite-entity)))
           (set! csd (ml:compile-sprite-db sd2))
           ;(ml:save-csd! csd (build-path "/Users/thoughtstem/Desktop/sprite-db") #:debug? #t)
           (set! compiled (append compiled uncompiled))
           (displayln (ml:compiled-sprite-db-spr->idx csd))
           (set! ml:render (gl:stage-draw/dc csd W H 8))))



    (define (sprites-to-render)
      (if (not last-game-snapshot)
          '()
          (flatten
           (filter identity
                   (for/list ([e (in-list (reverse (game-entities last-game-snapshot)))])
                     (define frame-i   (animated-sprite-current-frame (get-component e animated-sprite?)))
                     (define id-sym    (id->symbol #:prefix (~a "sprite" frame-i) (get-id e)))
                     (define sprite-id (ml:sprite-idx csd id-sym))

                    ; (displayln (~a (get-name e) " id: " id-sym " sprite-id: " sprite-id))

                     (if (not sprite-id)
                           #f
                           (ml:sprite #:layer 0
                                      (real->double-flonum (- (x e) W/2)) (real->double-flonum (- (y e) H/2))
                                      sprite-id)))))))

    
    
    (ml:render layers
               (sprites-to-render)
               '()))

  ticky-tick)


(struct demo
  ( state render-tick)
  #:methods gen:word
  [(define (word-fps w)
     60.0)  ;Changed from 60 to 30, which makes it more smooth on the Chromebooks we use in class.
            ;   Not sure why we were seeing such dramatic framerate drops
   
   (define (word-label s ft)
     (lux-standard-label "Values" ft))
   
   (define (word-output w)
     (match-define (demo  state render-tick) w)
     #;(g/v (draw state)) ;Old, slower drawing method.  For reference...
     (render-tick)
     )
   
   (define (word-event w e)
     (match-define (demo  state render-tick) w)
     (define closed? #f)
     (cond
       [(eq? e 'close)  #f]
       [(lux:key-event? e)

       
        (if (not (eq? 'release (send e get-key-code)))
            (demo  (handle-key-down state (format "~a" (send e get-key-code))) render-tick)
            (demo  (handle-key-up state (format "~a" (send e get-key-release-code))) render-tick))
         
        ]
       [else w]))
   
   (define (word-tick w)
     (match-define (demo  state render-tick) w)
     (demo  (tick state) render-tick)
     )])




(define (final-state d)
  (demo-state d))




(define (add-entity! db e)
  (define frames (animated-sprite-frames (get-component e animated-sprite?)))
  (for ([i (in-range (vector-length frames))])
    (define id-sym (id->symbol #:prefix (~a "sprite" i) (get-id e)))
    (ml:add-sprite!/value db
                          id-sym
                          (vector-ref frames i))

    (displayln (~a "Compiling sprite " (get-name e) " id: " id-sym ))))



(define (entities->compiled-sprite-database entities)
  (define sd (ml:make-sprite-db))

  (for ([e (in-list entities)])
    (and (get-component e animated-sprite?)
         (add-entity! sd e)))
  
  (define csd (ml:compile-sprite-db sd))

  (displayln (ml:compiled-sprite-db-spr->idx csd))
  ; (ml:save-csd! csd (build-path "/Users/thoughtstem/Desktop/sprite-db") #:debug? #t)

  csd)



