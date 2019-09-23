#lang racket

(provide text-sprite)

(require "../game-entities.rkt")
(require "../components/animated-sprite.rkt")

(require 2htdp/image
         (only-in racket/draw make-font))

(define (text-sprite str-or-list
                     #:scale       [scale 1]
                     #:font-size   [f-size 13]
                     #:font-face   [f-face MONOSPACE-FONT-FACE]
                     #:font-family [f-family 'modern]
                     #:font-style  [f-style  'normal]
                     #:font-weight [f-weight 'normal]
                     #:color       [color 'yellow]
                     #:blink-color [b-color 'red]
                     #:mode        [mode 'normal]
                     #:delay       [delay 20])
  (define new-font (make-font #:size   f-size
                              #:face   f-face
                              #:family f-family
                              #:style  f-style
                              #:weight f-weight))
  (register-fonts! new-font)
  (define str-list (if (list? str-or-list)
                       str-or-list
                       (filter identity (list str-or-list
                                              (and (eq? mode 'blink)
                                                   str-or-list))
                       )))
  (define str-list-with-font-fx
    (for/list ([str str-list]
               [i (range (length str-list))])
      (cond
        [(and (eq? mode 'blink)
              (odd? i))         (text-frame str #:font new-font #:color b-color)]
        [else                   (text-frame str #:font new-font #:color color)])))
  (new-sprite str-list-with-font-fx delay))
    