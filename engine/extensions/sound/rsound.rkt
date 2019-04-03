#lang racket

#;
(
 

(provide sound?
         rsound?
         sound
         make-sound-stream
         play
         stop-sound-streams)

(require (prefix-in r: rsound))

(r:default-sample-rate 48000)


(define (sound? s)
  (r:rsound? s))

(define rsound? sound?)

(define (sound path-to-sound)
  (r:resample-to-rate 48000 (r:rs-read path-to-sound)))

(define (make-sound-stream)
  (r:make-pstream))

(define (play stream sound)
  (r:pstream-play stream sound))

(define (stop-sound-streams)
  (r:stop))



 )
