#lang racket

;For real rsound, put this in
;(provide (all-from-out "./sound/rsound.rkt"))
;(require "./sound/rsound.rkt")

;For fake rsound (i.e. no sound), use this:
(displayln "Using fake sound")
(provide (all-from-out "./sound/fake-rsound.rkt"))
(require "./sound/fake-rsound.rkt")

