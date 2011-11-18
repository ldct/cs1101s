(require "engine-18.rkt")                        
(require racket/mpair)

;queue data structure

(define (make-queue) (mcons '() '()))
(define (empty-queue? q) (null? (mcar q)))
(define (insert! q item)
  (if (empty-queue? q)
      (begin (set-mcar! q (mcons item '()))
             (set-mcdr! q (mcar q)))
      (begin (set-mcdr! (mcdr q) (mcons item '()))
             (set-mcdr! q (mcdr (mcdr q))))))
(define (delete! q)
  (let ((ret (mcar (mcar q))))
    (set-mcar! q (mcdr (mcar q)))
    ret))

;graph is an alist of node:neighbours

(define (contains? x lst)
  (not (eq? #f (memq x lst))))

(define (insert x lst)
  (if (contains? x lst)
      lst
      (append lst (list x))))

(define (nodes graph)
  (map car graph))

(define (lookup node graph)
  (let ((filtered (filter (lambda (x) (eq? (car x) node))
                          graph)))
    (if (null? filtered)
        '()
        (cadar filtered))))

;breadth-first search
(define (search graph pred start)
  (let ((q (make-queue))
        (visited '()))
    (define (bfs)
      (let* ((vp (delete! q))
             (v (car vp))
             (p (cadr vp)))
        (set! visited (insert v visited))
        (if (pred v)
            vp
            (let* ((nexts (filter (lambda (x) (not (contains? x visited))) (lookup v graph))))
              (map (lambda (x)
                     (set! visited (insert x visited))
                     (insert! q (list x (append p (list v))))) 
                   nexts) ;enque
              (bfs)))))
    (insert! q (list start '()))
    (bfs)))

(define (make&install-player name birthplace)
  (let ((player (make-player name birthplace)))
    (ask (register player) 'install)
    player))

;helper functions

(define (display-list-of-places lop)
  (display (map (lambda (x) (ask x 'name)) lop)))

(define (filter-contains-generator lop) ;lop = list of places
  (car (split-list (lambda (x) 
                     (not (null? (car (split-list (lambda (y) (is-a y 'generator?))
                                                  (ask x 'things))))))
                   lop)))

(init-places)

;;;;;;;;;;;;;;
; Task 1 & 2 ;
;;;;;;;;;;;;;;

(define (make-player name birthplace)
  (let* ((person (make-person name birthplace))
         (my-known-graph '())
         (path '())) ;path I want to explore
    (lambda (message)
      (case message
        ((player?) (lambda (self) #t))
        ((act)
         (lambda (self)
           ((get-method person 'act) self)
           (let* ((lightsaber (pick-random (car (split-list (lambda (x) (is-a x 'weapon?)) (ask self 'possessions))))) ;look for a lightsaber
                  (owned-cards (car (split-list (lambda (x) (is-a x 'key-card?)) (ask self 'possessions)))) ;cards I own
                  (my-place (ask self 'place)) ;place (location) I am at
                  (my-neighbours (ask my-place 'neighbours))
                  (throwaway (set! my-known-graph (insert (list my-place my-neighbours) my-known-graph))) ;let-binding for side-effect
                  (visited (nodes my-known-graph)) ;list of places I have visited
                  (all-neighbours (apply append (map cadr my-known-graph))) ;all neighbours
                  (visited-contains-generators (filter-contains-generator all-neighbours)) ;list of places whose neighbours have generators
                  (my-unvisited-neighbours (car (split-list (lambda (x) (and (not (contains? x visited)) (not (is-a x 'protected-room?)))) my-neighbours)))
                  (my-people (other-people-at-place self my-place)) ;people I with
                  (random-service-bots (car (split-list (lambda (x) (is-a x 'service-bot?)) my-people))) ;choose a random service-bot in the current place
                  (random-security-drones (car (split-list (lambda (x) (is-a x 'security-drone?)) my-people))) ;choose a random service-bot in the current place
                  (charge-turns (ask lightsaber 'charge-turns)) ;how many turns to charge the lightsaber
                  (my-cards (car (split-list (lambda (x) (is-a x 'key-card?)) (ask my-place 'things)))) ;list of cards in my place
                  (my-p-neighbours (car (split-list (lambda (x) (is-a x 'protected-room?)) my-neighbours))) ;list of all protected rooms I am adjacent to                  
                  (my-x-neighbours (filter-contains-generator my-p-neighbours))
                  (idle 0)) ;whether to skip the move-generation
             (set! idle 0)
             (if (and (not (null? my-cards))
                      (null? owned-cards))
                 (ask self 'pick (pick-random my-cards)) ;pick any one up.
                 values)
             (if (and (= charge-turns 0) ;if my lightsaber is not charging
                      (not (null? random-service-bots)) ;and there is a service-bot
                      (null? owned-cards)) ;and I need an access card
                 (begin (ask self 'use lightsaber (pick-random random-service-bots)) ;attack any tthe service bot
                        (set! idle 1)) ;wait 1 turn
                 values)
             (if (and (= charge-turns 0) ;if my lightsaber is not charging
                      (not (null? random-security-drones))) ;and there is a security drone
                 (begin (ask self 'use lightsaber (pick-random random-security-drones)) ;attack it
                        (set! idle 1)) ;wait 1 turn
                 values)
             (if (and (not (null? owned-cards)) ;if I have a card
                      (not (null? my-x-neighbours))) ;and I'm next to a generator
                 (begin (ask self 'move-to (pick-random my-x-neighbours)) ;go to it
                        (set! idle 1))
                 values)
             (if (= idle 0) ;if I haven't attacked or moved to a protected room
                 (let ((move-target (cond ((not (null? path)) ;if there is a path to follow
                                           (let ((ret (car path)))
                                             (set! path (cdr path)) ;shorten the path
                                             ret)) ;follow the first item on the un-shortened path
                                          ((and (not (null? owned-cards)) ;if I have a card
                                                (not (null? visited-contains-generators))) ;and have seen a generator
                                           (let ((path-to-generator (search my-known-graph (lambda (x) (not (null? (filter-contains-generator (list x))))) my-place))) ;find a path to the generator
                                             (set! path (cdadr path-to-generator)) ;follow path
                                             (car path)))
                                          ((not (null? my-unvisited-neighbours))
                                           (pick-random my-unvisited-neighbours))
                                          ((= (length visited) 62) ;if I've visited all 62 passable locations and haven't triggered the generator logic (insanely unlikely)
                                           (pick-random my-neighbours)) ;walk randomly
                                          (else
                                           (let ((path-to-unvisited (search my-known-graph (lambda (x) (and (not (contains? x visited)) (not (is-a x 'protected-room?)))) my-place))) ;look for a place that is neither visited nor a generator room
                                             (set! path (cdadr path-to-unvisited)) ;put it on the path
                                             (car path))))))
                   (ask self 'move-to move-target)) ;move
                 values))))
        (else (get-method person message))))))


(define me (make&install-player 'me JFDI-ship))
(define my-saber (make&install-lightsaber JFDI-ship 30))
(ask me 'take my-saber)

(add-end-game-cond
 (lambda ()
   (eq? (ask me 'place)
        gen-room))
 (lambda ()
   (end-mission)
   (popup-message "You win!" "Congratulations! You have entered the generator room!")))

(start-mission)