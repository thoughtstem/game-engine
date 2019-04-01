#lang racket

(require "./crud.rkt"
         "./base.rkt")

(provide component-handler->game-handler
         entity-handler->game-handler
         reverse-params
         do-many
         
         sequence
         for-ticks
         times
         once-each)

(define (component-handler->game-handler ch)
  (lambda (g e c)
    (define new-e (update-component e c ch))
   
    (update-entity g e new-e)))
(define (entity-handler->game-handler eh)
  (lambda (g e c)
    (update-entity g e (curryr eh c))))

(define (any-handler->game-handler h)
  (match (procedure-arity h)
    [1 (component-handler->game-handler h)]
    [2 (entity-handler->game-handler h)]
    [3 h]
    [else (raise "That was not a handler")]))

(define (do-many . ghs)
  (lambda (g e c)
    (define temp-g (copy-game g)) 
    (for ([gh ghs])
      (set! temp-g ((any-handler->game-handler gh) temp-g e c)))

    temp-g))

(define (reverse-params f)
  (define (new-f . ps)
    (apply f (reverse ps)))
  
  new-f)


(define (once-each . ghs)
  (thunk
    (define done #f)
    (lambda (g e c)
      (if done 'done
          (begin
            (set! done #t) 
            ((apply do-many ghs) g e c)))
      )))


;Wow.  I like how short and generalizable these combinators are.  But I don't love the thunk or the inner lambda.  Maybe some syntax for helping to define these??
(define (sequence . gens)
   (thunk
     (define i  0)   
     (define hs (map init-script gens))

     (lambda (g e c)
       (if (= i (length hs))
         'done
         (let* ([h (list-ref hs i)]
                [ret (h g e c)]) 
           (cond 
             [(done? ret) 
              (begin 
                (set! i (add1 i))
                #f)] 
             [(noop? ret) #f] 
             [else ret]))))))

(define (for-ticks n gen)
  (thunk
    (define to-go 0)
    (define h (init-script gen))

    (lambda (g e c)
      (set! to-go (add1 to-go)) 

      ;TODO: Should we be looking at h and bailing early if it finishes early?  Or is the point of for-ticks that it spends that long on it no matter what...

      (if (> to-go n)
        'done
        (h g e c)))))

(define (times n gen)
  (thunk
    (define done #f)
    (define to-go 0)
    (define h (init-script gen))

    (lambda (g e c)
      (define ret (h g e c))

      ;TODO: Rewrite this.  I have no idea if it is really correct.  
      (cond 
        [(done? ret) 
         (if (= to-go n) 
           'done
           (begin
             (set! to-go (add1 to-go))
             (unless (= to-go n)
               (set! h (init-script gen))) 
             (h g e c)))] 
        [(noop? ret) #f] 
        [else ret]))))








