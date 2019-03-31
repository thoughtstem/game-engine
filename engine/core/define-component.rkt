#lang racket

(provide define-component)

(require "./base.rkt"
         "./crud.rkt")

(require (for-syntax racket))
(require (for-syntax racket/syntax))

(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ name (field ...))
     (with-syntax [(other-constructor (format-id #'name "new-~a" #'name))
                   (name-copy (format-id #'name "~a-copy" #'name)) 
                   (extra-name (format-id #'name "extra-~a" #'name))
                   (name? (format-id #'name "~a?" #'name))
                   (anys  (map (thunk* #'any/c) 
                               (syntax->datum #'(field ...)))) ] 
        #`(begin
             (struct name component (field ...) #:transparent
               #:extra-name extra-name)

             (generate-handlers name extra-name field)
             ...

             (define/contract (other-constructor #:handler (handler (lambda (c) c)) 
                                    #:entity-handler (entity-handler (lambda (e h) e))  
                                    #:game-handler (game-handler (lambda (g e h) g))  
                                    field ...)
              (->* anys 
                   [#:handler (-> name? name?) 
                    #:entity-handler (-> entity? name? entity?)
                    #:game-handler (-> game? entity? name? game?)] 
                   name?)
               (name #f  
                     (lambda (c i)
                       ;TODO: Why is this not working???
                       (struct-copy name c
                                    [id i])) 
                     handler entity-handler game-handler field ...))))]))

(define-syntax (generate-handlers stx)
  (syntax-case stx ()
    [(_ name extra-name field)
     (with-syntax 
       [ (name? (format-id #'name "~a?" #'name))

         ;e.g. update-health-amount
         (update-component-field (format-id #'name "update-~a-~a" #'name #'field))
         ;e.g. update-entity-health-amount
         (update-entity-component-field (format-id #'name "update-entity-~a-~a" #'name #'field))

         ;e.g. update-entity-health
         (update-entity-component (format-id #'name "update-entity-~a" #'name))

         ;e.g. update-entity-first-health
         (update-entity-first-component (format-id #'name "update-entity-first-~a" #'name))
         ;e.g. health-amount
         (getter (format-id #'name "~a-~a" #'name #'field))]
       #`(begin

           (define (update-component-field f)
             (lambda (c)
               (struct-copy name c
                            [field (f (getter c))])))

           (define (update-entity-component-field f)
             (lambda (e c)
               (update-component e c (update-component-field f))))
           
           (define (update-entity-component c2)
             (lambda (e c)
               (update-component e c c2)))

           (define (update-entity-first-component c2)
             (lambda (e c)
               (update-component e name? c2)))

           ) )]))

