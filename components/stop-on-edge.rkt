#lang racket

(require "../game-entities.rkt")
(require "../entity-helpers/movement-util.rkt")
(require posn)

;(provide (struct-out stop-on-edge))
(provide (except-out (struct-out stop-on-edge) stop-on-edge)
         (rename-out (make-stop-on-edge stop-on-edge)))

(component stop-on-edge (left right top bottom))

(define (make-stop-on-edge  . args)
  ;(define sorted-args (sort (map symbol->string args) string<?))
  ;(map string->symbol sorted-args)

  (define edges (hash 'bottom #f 'top #f 'left #f 'right #f  ))

  (if (empty? args)
      (set! edges (hash 'bottom #t 'top #t 'left #t 'right #t  ))
      (begin (set! edges (if (member 'left args )
                      (hash-set edges 'left #t)
                      (hash-set edges 'left #f)))

      (set! edges (if (member 'right args )
                      (hash-set edges 'right #t)
                      (hash-set edges 'right #f)))

      (set! edges (if (member 'top args )
                      (hash-set edges 'top #t)
                      (hash-set edges 'top #f)))

      (set! edges (if (member 'bottom args )
                      (hash-set edges 'bottom #t)
                      (hash-set edges 'bottom #f)))))

      (new-stop-on-edge (hash-ref edges 'left)
                        (hash-ref edges 'right)
                        (hash-ref edges 'top)
                        (hash-ref edges 'bottom))
  )

(define (update-stop-on-edge g e c)
  (define WIDTH (game-width g))
  (define HEIGHT (game-height g))
  (define p (get-component e posn?))
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (define left?   (stop-on-edge-left c))
  (define right?  (stop-on-edge-right c))
  (define top?    (stop-on-edge-top c))
  (define bottom? (stop-on-edge-bottom c))
  (cond [(and left?  (< pos-x 0))       (update-entity e posn? (posn 0     pos-y))]
        [(and right? (> pos-x WIDTH))   (update-entity e posn? (posn WIDTH pos-y))]
        [(and top? (< pos-y 0))         (update-entity e posn? (posn pos-x 0))]
        [(and bottom? (> pos-y HEIGHT)) (update-entity e posn? (posn pos-x HEIGHT))]
        [else e]))

(new-component stop-on-edge?
               update-stop-on-edge)
