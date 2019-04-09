#lang racket

;Read this before you hurt yourself: https://docs.racket-lang.org/guide/pattern-macros.html

(provide define-component)

(require "./base.rkt"
         "./crud.rkt")

(require (for-syntax racket))
(require (for-syntax racket/syntax))

(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ COMPONENT (FIELD ...))
     (with-syntax [(new-COMPONENT (format-id #'COMPONENT "new-~a" #'COMPONENT))
                   (COMPONENT? (format-id #'COMPONENT "~a?" #'COMPONENT))
                   (anys  (map (thunk* #'any/c) 
                               (syntax->datum #'(FIELD ...))))
                   (update-entity-COMPONENT (format-id #'COMPONENT "update-entity-~a" #'COMPONENT )) 
                   (entity-COMPONENT (format-id #'COMPONENT "entity-~a" #'COMPONENT)) 
                   (update:COMPONENT (format-id #'COMPONENT "update:~a" #'COMPONENT))
                   (update:COMPONENT^ (format-id #'COMPONENT "update:~a^" #'COMPONENT))
                   ] 
       #`(begin
           (define (COMPONENT? x) 
             (and (vector? x)
                  (eq? 'COMPONENT (vector-ref x 1))))

           (define/contract (new-COMPONENT id handlers FIELD ...)
                            (-> (or/c number? #f) vector? #,@#'anys COMPONENT?)
                            (vector 'component 'COMPONENT id handlers 
                                    FIELD ...))


           (generate-other-stuff COMPONENT FIELD (FIELD ...))
           ...

           (define/contract (update-entity-COMPONENT e c)
                            (-> entity? 
                                component?
                                entity?)
             (update-component e COMPONENT? c))

           (define (update:COMPONENT e c)
             (update-component e COMPONENT? c))

           (define (update:COMPONENT^ to-update)
             (lambda (g e c)
              (update-component e COMPONENT? to-update)))

           (define/contract (entity-COMPONENT e)
             (-> entity? COMPONENT?)
             (get-component e COMPONENT?))

           (define/contract (COMPONENT 
                              #:update (update #f) 
                              FIELD ...)
                            (->* anys 
                                 [#:update (or/c (-> entity? COMPONENT? entity?) 
                                                 (-> COMPONENT? COMPONENT?) 
                                                 handler? #f)]
                                 COMPONENT?)
                            (new-COMPONENT #f  
                                           (vector (lift-to-handler update)) 
                                           FIELD ...))))]))


(define-syntax (generate-other-stuff stx)
  (syntax-case stx ()
    [(_ COMPONENT FIELD (FIELDS ...))
     (with-syntax 
       [(COMPONENT-FIELD (format-id #'COMPONENT "~a-~a" #'COMPONENT #'FIELD) )
        (COMPONENT-FIELD? (format-id #'COMPONENT "~a-~a?" #'COMPONENT #'FIELD) ) 
        (entity-COMPONENT-FIELD (format-id #'COMPONENT "entity-~a-~a" #'COMPONENT #'FIELD) ) 
        (set-COMPONENT-FIELD (format-id #'COMPONENT "set-~a-~a" #'COMPONENT #'FIELD) ) 
        (entity-COMPONENT-FIELD? (format-id #'COMPONENT "entity-~a-~a?" #'COMPONENT #'FIELD) ) 
        (update-COMPONENT-FIELD (format-id #'COMPONENT "update-~a-~a" #'COMPONENT #'FIELD) ) 
        (update:COMPONENT/FIELD (format-id #'COMPONENT "update:~a/~a" #'COMPONENT #'FIELD) ) 
        (update:COMPONENT/FIELD^ (format-id #'COMPONENT "update:~a/~a^" #'COMPONENT #'FIELD) ) 
        (read:COMPONENT/FIELD (format-id #'COMPONENT "read:~a/~a" #'COMPONENT #'FIELD) ) 
        (read:COMPONENT/FIELD^ (format-id #'COMPONENT "read:~a/~a^" #'COMPONENT #'FIELD) ) 
        (rule:COMPONENT/FIELD (format-id #'COMPONENT "rule:~a/~a" #'COMPONENT #'FIELD) ) 
        (rule:COMPONENT/FIELD^ (format-id #'COMPONENT "rule:~a/~a^" #'COMPONENT #'FIELD) ) 
        (update-entity-COMPONENT-FIELD (format-id #'COMPONENT "update-entity-~a-~a" #'COMPONENT #'FIELD) ) 

        
        
        (COMPONENT? (format-id #'COMPONENT "~a?" #'COMPONENT) ) 
        (i (+ 4 (index-of 
                  (syntax->datum #'(FIELDS ...))
                  (syntax->datum #'FIELD))))]
       #`(begin
           (define/contract (COMPONENT-FIELD x)
             (-> COMPONENT? any/c)

             (vector-ref x i))

           (define/contract (entity-COMPONENT-FIELD e)
             (-> entity? any/c)

             (COMPONENT-FIELD (get-component e COMPONENT?)))


           (define/contract (entity-COMPONENT-FIELD? q)
             (-> (-> any/c boolean?) rule?)
             
             (lambda (g e me)
               (define c (get-component e COMPONENT?))

               (q (COMPONENT-FIELD c))))

           (define/contract (COMPONENT-FIELD? q)
             (-> (-> any/c boolean?) rule?)
             
             (lambda (g e c)
               (q (COMPONENT-FIELD c))))
           
           (define/contract (set-COMPONENT-FIELD x v)
             (-> COMPONENT? any/c COMPONENT?)

             (define temp (vector-copy x))
             
             (vector-set! temp i v)

             temp)

           (define (update-COMPONENT-FIELD f)
             (lambda (c)
               (define copy-c (vector-copy c))

               (vector-set! copy-c
                            i 
                            (f (COMPONENT-FIELD copy-c)))

               copy-c))


           (define/contract (update:COMPONENT/FIELD c f)
               (-> COMPONENT? 
                   any/c
                   COMPONENT?)

               (define copy-c (vector-copy c))

               (vector-set! copy-c
                            i 
                            (if (procedure? f) 
                                (f (COMPONENT-FIELD copy-c))
                              f))

               copy-c)

           (define/contract (update:COMPONENT/FIELD^ f)
               (-> procedure? handler?)

               (lambda (g e c)
                 (update:COMPONENT/FIELD c f)))

           (define/contract (read:COMPONENT/FIELD e-or-c)
               (-> (or/c entity? COMPONENT?)
                   any/c)

               (if (entity? e-or-c)
                 (COMPONENT-FIELD (get-component e-or-c COMPONENT? )) 
                 (COMPONENT-FIELD e-or-c)))

           (define/contract (read:COMPONENT/FIELD^)
               (-> handler?)

               (lambda (g e c)
                 (read:COMPONENT/FIELD c)))

           (define/contract (rule:COMPONENT/FIELD e-or-c pred?)
               (-> (or/c entity? COMPONENT?)
                   (-> any/c any/c)
                   boolean?)

                 (define to-check 
                   (if (entity? e-or-c)
                     (get-component e-or-c COMPONENT?)
                     e-or-c))

                 (pred? (read:COMPONENT/FIELD to-check)))

           (define/contract (rule:COMPONENT/FIELD^ pred?)
               (-> (-> any/c any/c)
                   rule?)
               
               (lambda (g e c)
                 ;Use the entity so rules can be ported from component to component within an entity and continue to work the same (as long as there is only one of COMPONENT type on the entity).
                 (rule:COMPONENT/FIELD e pred?)))


           (define/contract (update-entity-COMPONENT-FIELD e f)
            (-> entity? any/c entity?)
            (update-component e COMPONENT? 
                               (update-COMPONENT-FIELD f)))


           ))]))





