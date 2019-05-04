#lang racket

;Read this before you hurt yourself: https://docs.racket-lang.org/guide/pattern-macros.html

(provide define-component)

(require "./base.rkt"
         "./crud.rkt"
         )

(require (for-syntax racket))
(require (for-syntax racket/syntax))

(define-syntax (define-component stx)
  (syntax-case stx ()
    [(_ COMPONENT (FIELD ...))
     (with-syntax [(new-COMPONENT (format-id #'COMPONENT "new-~a" #'COMPONENT))
                   (COMPONENT? (format-id #'COMPONENT "~a?" #'COMPONENT))
                   (anys  (map (thunk* #'any/c) 
                               (syntax->datum #'(FIELD ...))))
                   (get:COMPONENT (format-id #'COMPONENT "get:~a" #'COMPONENT))
                   (update:COMPONENT (format-id #'COMPONENT "update:~a" #'COMPONENT))
                   (update:COMPONENT^ (format-id #'COMPONENT "update:~a^" #'COMPONENT))
                   ] 
       (quasisyntax/loc stx (begin
                          (define (COMPONENT? x) 
                            (and (vector? x)
                                 (eq? 'COMPONENT (vector-ref x 1))))

                          (define 
                            (new-COMPONENT id handler handler-code FIELD ...)
                            (vector 'component 'COMPONENT id handler handler-code
                                    FIELD ...))


                          (generate-other-stuff COMPONENT FIELD (FIELD ...))
                          ...

                          (define (update:COMPONENT e c)
                            (update-component e COMPONENT? c))

                          (define (get:COMPONENT e)
                            (get-component e COMPONENT?))


                          ;Ummmm. what??
                          (define (update:COMPONENT^ to-update)
                            (lambda (g e c)
                              (update-component e COMPONENT? to-update)))

                          (define/contract 
                            (COMPONENT 
                              #:update (update #f) 
                              FIELD ...)
                            (maybe-contract
                              (->* anys 
                                   [#:update (or/c (-> entity? COMPONENT? entity?) 
                                                   (-> COMPONENT? COMPONENT?) 
                                                   (or/c handler? #f) #f)]
                                   COMPONENT?))

                            (new-COMPONENT (next-id)  
                                           update 
                                           'unknown-update-function
                                           FIELD ...)))))]))


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
        (update:my/COMPONENT/FIELD^ (format-id #'COMPONENT "update:my/~a/~a^" #'COMPONENT #'FIELD) ) 
        (get:COMPONENT/FIELD (format-id #'COMPONENT "get:~a/~a" #'COMPONENT #'FIELD) ) 
        (get:COMPONENT/FIELD^ (format-id #'COMPONENT "get:~a/~a^" #'COMPONENT #'FIELD) ) 
        (rule:COMPONENT/FIELD (format-id #'COMPONENT "rule:~a/~a" #'COMPONENT #'FIELD) ) 
        (rule:COMPONENT/FIELD^ (format-id #'COMPONENT "rule:~a/~a^" #'COMPONENT #'FIELD) ) 
        (update-entity-COMPONENT-FIELD (format-id #'COMPONENT "update-entity-~a-~a" #'COMPONENT #'FIELD) ) 

        
        
        (COMPONENT? (format-id #'COMPONENT "~a?" #'COMPONENT) ) 
        (i (+ 5 ;number of pre-fiends in a component 
              (index-of 
                  (syntax->datum #'(FIELDS ...))
                  (syntax->datum #'FIELD))))]
       (quasisyntax/loc stx
         (begin
           (define/contract (COMPONENT-FIELD x)
             (maybe-contract
               (-> COMPONENT? any/c))

             (vector-ref x i))

           (define/contract (entity-COMPONENT-FIELD e)
             (maybe-contract
               (-> entity? any/c))

             (COMPONENT-FIELD (get-component e COMPONENT?)))


           (define/contract (entity-COMPONENT-FIELD? q)
             (maybe-contract
               (-> (-> any/c boolean?) rule?))

             (lambda (g e me)
               (define c (get-component e COMPONENT?))

               (q (COMPONENT-FIELD c))))

           (define/contract (COMPONENT-FIELD? q)
             (maybe-contract
               (-> (-> any/c boolean?) rule?))

             (lambda (g e c)
               (q (COMPONENT-FIELD c))))

           (define/contract (update:COMPONENT/FIELD c f)
             (maybe-contract
               (-> COMPONENT? 
                   any/c
                   COMPONENT?))

             (define copy-c 
               (if (mutable-state) 
                 c
                 (vector-copy c)))

             (vector-set! copy-c
                          i 
                          (if (procedure? f) 
                            (f (COMPONENT-FIELD copy-c))
                            f))

             copy-c)

           (define/contract (update:COMPONENT/FIELD^ f)
             (maybe-contract
               (-> procedure? handler?))

             (lambda (g e c)
               (update-component e COMPONENT?
                                 (curryr update:COMPONENT/FIELD f))))

           (define/contract (update:my/COMPONENT/FIELD^ f)
             (maybe-contract
               (-> procedure? handler?))

             (lambda (g e c)
               (update-component e c 
                                 (update:COMPONENT/FIELD c f))))

           (define/contract (get:COMPONENT/FIELD e-or-c)
             (maybe-contract
               (-> (or/c entity? COMPONENT?)
                   any/c))

             (if (entity? e-or-c)
               (COMPONENT-FIELD (get-component e-or-c COMPONENT? )) 
               (COMPONENT-FIELD e-or-c)))

           (define #;/contract 
             (get:COMPONENT/FIELD^)

             #;
             (-> handler?)

             (lambda (g e c)
               (get:COMPONENT/FIELD c)))

           (define #;/contract 
             (rule:COMPONENT/FIELD e-or-c pred?)
             #;
             (-> (or/c entity? COMPONENT?)
                 (-> any/c any/c)
                 boolean?)

             (define to-check 
               (if (entity? e-or-c)
                 (get-component e-or-c COMPONENT?)
                 e-or-c))

             (pred? (get:COMPONENT/FIELD to-check)))

           (define #;/contract 
             (rule:COMPONENT/FIELD^ pred?)
             #;
             (-> (-> any/c any/c)
                 rule?)

             (lambda (g e c)
               ;Use the entity so rules can be ported from component to component within an entity and continue to work the same (as long as there is only one of COMPONENT type on the entity).
               (rule:COMPONENT/FIELD e pred?)))


           )))]))





(provide define-signal)
(define-syntax (define-signal stx)
  (syntax-case stx ()
    [(_ COMPONENT KIND?)
     (with-syntax [(new-COMPONENT (format-id #'COMPONENT "new-~a" #'COMPONENT))
                   (COMPONENT? (format-id #'COMPONENT "~a?" #'COMPONENT)) 
                   (get-COMPONENT (format-id #'COMPONENT "get-~a" #'COMPONENT)) 
                   (set-COMPONENT (format-id #'COMPONENT "set-~a" #'COMPONENT)) 
                   ]
       (quasisyntax/loc stx (begin
                              (define (COMPONENT? x) 
                                (and (vector? x)
                                     (eq? 'COMPONENT (vector-ref x 1))))

                              (define 
                                (new-COMPONENT id handler handler-code FIELD)
                                (vector 'component 'COMPONENT id handler handler-code FIELD))

                              (define (get-COMPONENT (c #f))
                                (vector-ref
                                  (if c
                                    c
                                    (get-component (CURRENT-ENTITY) 
                                                   COMPONENT?))
                                  5)
                                
                                )

                              (define (set-COMPONENT e v)
                                (define to-update
                                  (if (entity? e)
                                    (get-component e COMPONENT?)
                                    e))

                                (vector-set!
                                  to-update 
                                  5
                                  v)
                                
                                e)

                              (define-syntax-rule
                                (COMPONENT FIELD update)

                                (let ([update-lambda (thunk update)])

                                  (new-COMPONENT (next-id)  
                                                 ;Prototyping new components (signals) on top of old components....
                                                 (lambda (g e c)

                                                   (define next-val
                                                     (update-lambda))

                                                   (cond 
                                                     [(KIND? next-val)
                                                      (vector-set! c 5 next-val)]
                                                     [(entity? next-val)
                                                      (set! e next-val)]

                                                     [(despawn-me? next-val)
                                                      (add-component e
                                                                     (vector 'component
                                                                             'dead
                                                                             -1

                                                                             #f
                                                                             'none)) ]
                                                     [(spawn-me? next-val)
                                                      ;Hack to get spawning to work on top of the old system
                                                      (add-component e 
                                                                     (vector 'component
                                                                             'spawner
                                                                             -1
                                                                             #f
                                                                             'none
                                                                             (spawn-me-entity next-val)
                                                                             ))]
                                                     [else (raise "What are you returning from your signal update fucntion?")]
                                                     )
                                                   e)
                                                 '(lambda () update)
                                                 FIELD))))))]))


