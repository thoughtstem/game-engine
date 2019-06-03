#lang racket

(provide entity-grid)

(require "../../core/main.rkt"
         "../common-components/main.rkt"
         2htdp/image)

(define-component cell posn?)

(define (entity-grid w h cell-size cs)

  (define cols (/ w cell-size))
  (define rows (/ h cell-size))

  (define es
    (for*/list ([r (range rows)]
                [c (range cols)])
      (entity
        (cell (posn r c))
        (position (posn-add  
                    (posn-scale 0.5 
                                (posn cell-size cell-size))
                    (posn (* c cell-size)
                          (* r cell-size))))
        (cs r c) )))

  es)

