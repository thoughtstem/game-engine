#lang racket

(require "../main.rkt"
         "./util.rkt"
         "../hotswap.rkt"
         2htdp/image)

;Okay, now make it clean up the temp files, start using it

(hotswap g
  (game 
    input-manager
    (red-square-enemy)
    (red-square-enemy)
    (blue-circle-avatar (posn 100 200))))



