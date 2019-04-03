#lang racket

(provide sound?
         rsound?
         sound
         make-sound-stream
         play
         stop-sound-streams)

(define (sound? s)
  #t)

(define rsound? sound?)

(define (sound path-to-sound)
  #t)

(define (make-sound-stream)
  #t)

(define (play stream sound)
  (display (~a "Not playing sound: " sound))
  #t)

(define (stop-sound-streams)
  #t)
