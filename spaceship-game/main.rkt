#lang racket

(require (for-syntax racket/syntax))

(provide play-spaceship
         edit-spaceship)

(define-syntax (play-spaceship stx)
    (syntax-case stx ()
      [(_ x)
       (with-syntax (;[path (format "./v~a/spaceship-game.rkt" (syntax->datum #'x))]
                     [prefix-id (format-id stx "game-engine/spaceship-game/v~a/spaceship-game" (syntax->datum #'x))])
           #`(begin
               ;(provide (rename-out [start prefix-id]))
               (require prefix-id)))
      ]))


(define (edit-spaceship n)
  (define rmp
    (module-path-index-resolve
     
      (module-path-index-join
       (string->symbol (format "game-engine/spaceship-game/v~a/spaceship-game" n))
       #f)))
  (define path (path->string (resolved-module-path-name rmp)))
    
  (with-input-from-file path
    (lambda () (display
                (read-string 100000)))))