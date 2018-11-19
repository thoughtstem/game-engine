#lang racket

(provide randomly-relocate-me
         go-to-random
         go-to
         go-to-pos
         go-to-pos-inside
         respawn
         move-with-speed
         move-random-speed
         point-to
         bounce
         change-x-by
         change-y-by
         change-x-by-random
         change-y-by-random
         freeze-entity
         un-freeze-entity
         distance-between
         get-entities-near
         ;near-entity?       ;DO WE NEED THIS?
         ;near-ent?
         near?
         player-is-near?)

(require "../game-entities.rkt"
         "../components/direction.rkt"
         "../components/every-tick.rkt"
         "../components/animated-sprite.rkt"
         "../component-util.rkt"
         "../ai.rkt"
         2htdp/image
         posn)

(define (randomly-relocate-me min-x max-x min-y max-y)
  (lambda (g e)
    (update-entity e posn? (posn (random min-x max-x)
                                 (random min-y max-y)))))

(define (go-to-random min-x max-x min-y max-y)
  (lambda (g e)
    (update-entity e posn? (posn (random min-x (add1 max-x))
                                 (random min-y (add1 max-y))))))

(define (go-to pos-x pos-y)
  (lambda (g e)
    (update-entity e posn? (posn pos-x pos-y))))


(define alignment? (or/c 'left        'right        'top         'bottom
              'top-left    'top-right    'bottom-left 'bottom-right
              'left-center 'right-center 'top-center  'bottom-center
              'center))

(define/contract (go-to-pos pos #:offset [offset 0])
  (->* (alignment?) (#:offset number?) procedure?) 
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define p (get-component e posn?))
    (define pos-x (posn-x p))
    (define pos-y (posn-y p))
    (update-entity e posn?
                   (cond
                     [(eq? pos 'left)         (posn offset           pos-y)]
                     [(eq? pos 'right)        (posn (+ WIDTH offset) pos-y)]
                     [(eq? pos 'top)          (posn pos-x            offset)]
                     [(eq? pos 'bottom)       (posn pos-x            (+ HEIGHT offset))]
                     [(eq? pos 'top-left)     (posn 0                0)]
                     [(eq? pos 'top-right)    (posn WIDTH            0)]
                     [(eq? pos 'bottom-left)  (posn 0                HEIGHT)]
                     [(eq? pos 'bottom-right) (posn WIDTH            HEIGHT)]
                     [(eq? pos 'left-center)  (posn offset                (/ HEIGHT 2))]
                     [(eq? pos 'right-center) (posn (+ WIDTH offset) (/ HEIGHT 2))]
                     [(eq? pos 'top-center)   (posn (/ WIDTH 2)      offset)]
                     [(eq? pos 'bottom-center)(posn (/ WIDTH 2)      (+ HEIGHT offset))]
                     [(eq? pos 'center)       (posn (/ WIDTH 2)      (/ HEIGHT 2))]))))
                         
(define/contract (go-to-pos-inside pos #:offset [offset 0])
  (->* ((and/c alignment? (not/c 'center))) (#:offset number?) procedure?) 
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define p (get-component e posn?))
    ;(match-define (bb e-w e-h) (get-component e bb?))
    ;(define hw (/ e-w 2))  ;(+ (/ e-w 2) 2)) ; Not sure why 2 was added
    ;(define hh (/ e-h 2))  ;(+ (/ e-h 2) 2)) ; Not sure why 2 was added
    (define as (get-component e animated-sprite?))
    (define hw (/ (image-width  (render as)) 2))
    (define hh (/ (image-height (render as)) 2))
    (define pos-x (posn-x p))
    (define pos-y (posn-y p))
    (update-entity e posn?
                   (cond
                     [(eq? pos 'left)         (posn (+ offset hw)           pos-y)]
                     [(eq? pos 'right)        (posn (+ (- WIDTH hw) offset) pos-y)]
                     [(eq? pos 'top)          (posn pos-x                   (+ offset hh))]
                     [(eq? pos 'bottom)       (posn pos-x                   (+ (- HEIGHT hh) offset))]
                     [(eq? pos 'top-left)     (posn hw                      hh)]
                     [(eq? pos 'top-right)    (posn (- WIDTH hw)            hh)]
                     [(eq? pos 'bottom-left)  (posn hw                      (- HEIGHT hh))]
                     [(eq? pos 'bottom-right) (posn (- WIDTH hw)            (- HEIGHT hh))]
                     [(eq? pos 'left-center)  (posn (+ hw offset)           (/ HEIGHT 2))]
                     [(eq? pos 'right-center) (posn (+ (- WIDTH hw) offset) (/ HEIGHT 2))]
                     [(eq? pos 'top-center)   (posn (/ WIDTH 2)             (+ hh offset))]
                     [(eq? pos 'bottom-center)(posn (/ WIDTH 2)             (+ (- HEIGHT hh) offset))]))))    

(define (respawn edge #:offset [offset 0])
  (lambda (g e)
    (define HEIGHT (game-height g))
    (define WIDTH (game-width g))
    ((cond
      [(eq? edge 'left)   (go-to offset (random 0 HEIGHT))]
      [(eq? edge 'right)  (go-to (+ WIDTH offset) (random 0 HEIGHT))]
      [(eq? edge 'top)    (go-to (random 0 WIDTH) offset)]
      [(eq? edge 'bottom)  (go-to (random 0 WIDTH) (+ HEIGHT offset))]
      [(eq? edge 'anywhere) (go-to (random offset (- WIDTH offset)) (random offset (- HEIGHT offset)))])
      g e)))

(define (move-with-speed spd)
  (lambda (g e)
    (define dir (get-direction e))
    (update-entity e every-tick?
                     (every-tick (move-dir-spd #:dir dir #:speed spd)))))

(define (move-random-speed min max)
  (lambda (g e)
    (define dir (get-direction e))
    (update-entity e every-tick?
                     (every-tick (move-dir-spd #:dir dir #:speed (random min (add1 max)))))))


(define (point-to name)
  (lambda (g e)
    (define target? (get-entity name g))
    (define target-x (unless (eq? target? #f) (posn-x (get-component target? posn?))))
    (define target-y (unless (eq? target? #f) (posn-y (get-component target? posn?))))
    (define x (posn-x (get-component e posn?)))
    (define y (posn-y (get-component e posn?)))
    (define new-dir (unless (eq? target? #f)(radians->degrees (atan (- target-y y) (- target-x x)))))
    (if target?
        (update-entity e direction? (direction (modulo (exact-round new-dir) 360)))
        e)))

(define (bounce)
  (lambda (g e)
    (update-entity e direction? (direction (modulo (+ (get-direction e) 180) 360)))))

(define (change-x-by amount)
  (lambda (g e)
    (define p (get-component e posn?))
    (update-entity e posn? (posn (+ (posn-x p) amount) (posn-y p)))))

(define (change-y-by amount)
  (lambda (g e)
    (define p (get-component e posn?))
    (update-entity e posn? (posn (posn-x p) (+ (posn-y p) amount)))))

(define (change-x-by-random min max)
  (lambda (g e)
    (define p (get-component e posn?))
    (update-entity e posn? (posn (+ (posn-x p) (random min (add1 max))) (posn-y p)))))

(define (change-y-by-random min max)
  (lambda (g e)
    (define p (get-component e posn?))
    (update-entity e posn? (posn (posn-x p) (+ (posn-y p) (random min (add1 max)))))))

; Warning: This may not work with other every-tick components
(define (freeze-entity)
  (lambda (g e)
    (define p (get-component e posn?))
    (add-component (remove-component e every-tick?)
                   (every-tick (do-many (go-to (posn-x p) (posn-y p))
                                        #;(set-direction 0))))))

;;Warning: This will also remove any existing every-tick components
(define (un-freeze-entity)
  (lambda (g e)
    (remove-component e every-tick?)))

(define (distance-between pos1 pos2)
  (define p (posn-subtract pos2 pos1))
  (sqrt (+ (expt (posn-x p) 2) (expt (posn-y p) 2))))

; === ARE THESE NEEDED ANYMORE? ====
(define (near-entity? name [range 80])
  (lambda (g e)
    (define pos (get-component e posn?))
    (define target-pos (get-component (get-entity name g) posn?))
    (< (distance-between target-pos pos) range)))

(define (near-ent? target-e [range 80])
  (lambda (g e)
    (define pos (get-component e posn?))
    (define target-pos (get-component target-e posn?))
    (< (distance-between target-pos pos) range)))

; ===================================

(define (close? range source-e target-e)
  (define source-pos (get-component source-e posn?))
  (define target-pos (get-component target-e posn?))
  (<  (distance-between target-pos source-pos) range))

(define (get-entities-near e g [range 80])
  (filter (curry close? range e) (game-entities g)))

(define (near? name [range 80])
  (lambda (g e)
    (define (name-eq? name e)
      (eq? (get-name e) name))
    (define nearby-ents (filter (curry name-eq? name) (get-entities-near e g range)))
    (not (empty? nearby-ents))))

(define (player-is-near? item [range 80])
  (lambda (g e)
    (define player (get-entity "player" g))
    ((near? item range) g player)))