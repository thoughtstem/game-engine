#lang racket

(require "../game-entities.rkt")


(provide (except-out (struct-out on-mouse) on-mouse)
         (rename-out (make-on-mouse on-mouse))
         (except-out (struct-out on-mouse-hold) on-mouse-hold)
         (rename-out (make-on-mouse-hold on-mouse-hold))
         (rename-out (on-mouse      struct-on-mouse)
                     (on-mouse-rule struct-on-mouse-rule)
                     (on-mouse-f struct-on-mouse-f))
         get-on-mouse-button)

(component on-mouse (button rule f))

(define (make-on-mouse button #:rule [rule (lambda (g e) #t)] f)
  (new-on-mouse button rule f))

(define (update-on-mouse g e c)
 (if (and (mouse-button-change-down? (on-mouse-button c) g)
          ((on-mouse-rule c) g e))
     ((on-mouse-f c) g e)
     e))

(new-component on-mouse?
               update-on-mouse)

; ==== on-mouse-hold ====
(component on-mouse-hold (button rule f))

(define (make-on-mouse-hold button #:rule [rule (lambda (g e) #t)] f)
  (new-on-mouse-hold button rule f))

(define (update-on-mouse-hold g e c)
 (if (and (mouse-button-down? (on-mouse-hold-button c) g)
          ((on-mouse-hold-rule c) g e))
     ((on-mouse-hold-f c) g e)
     e))

(new-component on-mouse-hold?
               update-on-mouse-hold)

(define (get-on-mouse-button e)
  (on-mouse-button (get-component e on-mouse?)))