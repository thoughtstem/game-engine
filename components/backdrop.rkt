#lang racket

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require 2htdp/image)

(require posn)

(provide (struct-out backdrop)
         set-current-tile
         get-current-tile
         next-tile
         more-tiles?)

(struct backdrop (tiles columns current-tile))

(define (update-backdrop g e c) e)

;(new-component backdrop?
;               update-backdrop)


; === BACKDROP HELPER FUNCTIONS ===
(define (set-current-tile num)
 (lambda (g e)
   (update-entity e backdrop? (struct-copy backdrop (get-component e backdrop?)
                                           [current-tile num]))))

(define (get-current-tile e)
  (backdrop-current-tile (get-component e backdrop?)))

(define/contract (pick-tile backdrop i)
  (-> backdrop? integer? image?)
  (list-ref (backdrop-tiles backdrop) i))

(define (next-backdrop-index direction total-tiles col current-backdrop-index)
  (define left-edge-list   (range 0 total-tiles col))
  (define right-edge-list  (range (sub1 col) total-tiles col))
  (define top-edge-list    (range 0  col))
  (define bottom-edge-list (range (- total-tiles col) total-tiles))
  (cond [(eq? direction 'left)  (if (member current-backdrop-index left-edge-list)
                                    #f
                                    (sub1 current-backdrop-index))]
        [(eq? direction 'right) (if (member current-backdrop-index right-edge-list)
                                    #f
                                    (add1 current-backdrop-index))]
        [(eq? direction 'top)    (if (member current-backdrop-index top-edge-list)
                                    #f
                                    (- current-backdrop-index col))]
        [(eq? direction 'bottom)    (if (member current-backdrop-index bottom-edge-list)
                                    #f
                                    (+ current-backdrop-index col))]))

(define (next-tile direction)
  (lambda (g e)
    (define backdrop (get-component e backdrop?))
    (define total-tiles (length (backdrop-tiles backdrop)))
    (define col         (backdrop-columns backdrop))
    (define current-bg-index (get-current-tile e))
    (define next-bg-index (next-backdrop-index direction total-tiles col current-bg-index))
    (if next-bg-index
        (update-entity ((set-current-tile next-bg-index) g e) ;(update-entity e counter? (counter next-bg-index))
                       animated-sprite? (new-sprite (pick-tile backdrop next-bg-index)))
        e)))

; Assumes background is named "bg" and has a backdrop component
(define (more-tiles? direction)
  (lambda (g e)
    (define backdrop         (get-component (get-entity "bg" g) backdrop?))
    (define total-tiles      (length (backdrop-tiles backdrop)))
    (define col              (backdrop-columns backdrop))
    (define current-bg-index (backdrop-current-tile backdrop))
    (define next-bg-index    (next-backdrop-index direction total-tiles col current-bg-index))
    (if next-bg-index
        #t
        #f)))