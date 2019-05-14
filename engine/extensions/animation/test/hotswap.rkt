#lang racket

(require "../main.rkt"
         "./util.rkt"
         "../hotswap.rkt"
         2htdp/image)

;Okay, now make it clean up the temp files, start using it
;  Or switch to https://github.com/tonyg/racket-reloadable/tree/master#readme

(hotswap g
  (game 
    input-manager
    (red-square-enemy)
    (red-square-enemy)
    (blue-circle-avatar (posn 100 200))))



