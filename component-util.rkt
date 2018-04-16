#lang racket

(provide do-many)

(define (do-many . funs)
  (lambda (g e)
    (foldl (lambda (next accum)
             (next g accum)) e funs)))

