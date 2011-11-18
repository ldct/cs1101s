;
; CS1101S --- Programming Methodology
;
; Mission 19
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(require "engine-19.rkt")   ; engine for the game world
(require racket/mpair)

(define (make&install-player name birthplace)
  (let ((player (make-player name birthplace)))
    (ask (register player) 'install)
    player))

(init-places)

;;;;;;;;;;;;;;
; Task 1 & 2 ;
;;;;;;;;;;;;;;

(define (make-player name birthplace)

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
    (if (empty-queue? q)
        #f
        (let ((ret (mcar (mcar q))))
          (set-mcar! q (mcdr (mcar q)))
          ret)))
  
  ;graph is an alist of node:neighbours
  
  (define (contains? x lst)
    (not (eq? #f (memq x lst))))
  
  (define (insert x lst)
    (if (contains? x lst)
        lst
        (append lst (list x))))
  
  (define (unique lst)
    (if (null? lst) lst
        (append (if (memq (car lst) (cdr lst)) '() (list (car lst)))
                (unique (cdr lst)))))
  
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
        (define vp (delete! q))
        (if (not vp)
            #f
            (let* ((v (car vp))
                   (p (cadr vp)))
              (set! visited (insert v visited))
              (if (pred v)
                  (list (car vp)
                        (append (cadr vp) (list (car vp))))
                  (let* ((nexts (filter (lambda (x) (not (contains? x visited))) (lookup v graph))))
                    (map (lambda (x)
                           (set! visited (insert x visited))
                           (insert! q (list x (append p (list v))))) 
                         nexts) ;enque
                    (bfs))))))
      (insert! q (list start '()))
      (bfs)))
  
  (define (within-range graph start range)
    (define visited '())
    (define (helper node range)
      (set! visited (cons node visited))
      (if (= range 0)
          (list node)
          (unique (apply append (cons (list node) (map (lambda (n) (if (contains? n visited)
                                                                       '()
                                                                       (helper n (- range 1))))
                                                       (lookup node graph)))))))
    (helper start range))
  
  
  
  (define (display-list-of-places lop)
    (display (map (lambda (x) (ask x 'name)) lop)))
  
  (define (contains-generator? p)
    (pair? (filter (lambda (y) (is-a y 'generator?)) (ask p 'things))))
  (define (contains-key-card? p)
    (pair? (filter (lambda (y) (is-a y 'key-card?)) (ask p 'things))))
  
  (define (crawl start)
    (define visited '())
    (define (helper node)
      (set! visited (cons node visited))
      (cons (list node (ask node 'neighbours))
            (apply append (map (lambda (n) (if (contains? n visited)
                                               '()
                                               (helper n)))
                               (ask node 'neighbours)))))
    (helper start))
  
  (define (range-dir range dir start)
    (if (or (eq? range 0)
            (not (ask start 'neighbour-towards dir)))
        (list start)
        (cons start (range-dir (- range 1) dir (ask start 'neighbour-towards dir)))))
  
  (let* ((person (make-person name birthplace))
         (path '())
         (visited '())
         (all-neighbours '()))
    (lambda (message)
      (case message
        ((player?) (lambda (self) #t))
        ((act)
         (lambda (self)
           ((get-method person 'act) self)
           (let* ((possessions (ask self 'possessions))
                  (lightsaber (car (filter (lambda (x) (is-a x 'melee-weapon?)) possessions)))
                  (rifle (car (filter (lambda (x) (is-a x 'ranged-weapon?)) possessions)))
                  (lightning (car (filter (lambda (x) (is-a x 'spell?)) possessions)))
                  (bomb (pick-random (filter (lambda (x) (eq? (ask x 'name) 'gen-bomb)) possessions)))
                  (lightsaber-charge (ask lightsaber 'charge-turns))
                  (rifle-charge (ask rifle 'charge-turns))
                  (owned-cards (filter (lambda (x) (is-a x 'key-card?)) (ask self 'possessions))) ;cards I own
                  
                  (my-place (ask self 'place))
                  (my-people (other-people-at-place self my-place))
                  
                  (my-neighbours (ask my-place 'neighbours))
                  (my-unvisited-neighbours (filter (lambda (x) (and (not (contains? x visited)) (not (is-a x 'protected-room?)))) 
                                                   my-neighbours))
                  (service-bots (filter (lambda (x) (is-a x 'service-bot?)) my-people))
                  (security-drones (filter (lambda (x) (is-a x 'security-drone?)) my-people))
                  
                  (my-neighbours-3 (within-range all-neighbours my-place 3))
                  (my-people-3 (apply append (map (lambda (p) (other-people-at-place self p)) my-neighbours-3)))
                  (security-drones-3 (filter (lambda (x) (is-a x 'security-drone?)) my-people-3))
                  (service-bots-3 (filter (lambda (x) (is-a x 'service-bot?)) my-people-3))
                  
                  (my-neighbours-north (range-dir 2 'north my-place))
                  (my-people-north (apply append (map (lambda (p) (other-people-at-place self p)) my-neighbours-north)))
                  (security-drones-north (filter (lambda (x) (is-a x 'security-drone?)) my-people-north))
                  ;(service-bots-north (filter (lambda (x) (is-a x 'service-bot?)) my-people-north))
                  
                  (my-neighbours-south (range-dir 2 'south my-place))
                  (my-people-south (apply append (map (lambda (p) (other-people-at-place self p)) my-neighbours-south)))
                  (security-drones-south (filter (lambda (x) (is-a x 'security-drone?)) my-people-south))
                  ;(service-bots-south (filter (lambda (x) (is-a x 'service-bot?)) my-people-south))
                  
                  (my-neighbours-east (range-dir 2 'east my-place))
                  (my-people-east (apply append (map (lambda (p) (other-people-at-place self p)) my-neighbours-east)))
                  (security-drones-east (filter (lambda (x) (is-a x 'security-drone?)) my-people-east))
                  ;(service-bots-east (filter (lambda (x) (is-a x 'service-bot?)) my-people-east)) 
                  
                  (my-neighbours-west (range-dir 2 'west my-place))
                  (my-people-west (apply append (map (lambda (p) (other-people-at-place self p)) my-neighbours-west)))
                  (security-drones-west (filter (lambda (x) (is-a x 'security-drone?)) my-people-west))
                  ;(service-bots-west (filter (lambda (x) (is-a x 'service-bot?)) my-people-west)) 
                  
                  (my-cards (filter (lambda (x) (is-a x 'key-card?)) (ask my-place 'things))) ;list of cards in my place
                  (my-p-neighbours (filter (lambda (x) (is-a x 'protected-room?)) my-neighbours)) ;list of all protected rooms I am adjacent to                  
                  (my-x-neighbours (filter contains-generator? my-p-neighbours))
                  )
             
             (set! visited (insert my-place visited))
             
             (if (and (pair? my-neighbours)
                      (null? all-neighbours))
                 (set! all-neighbours (crawl my-place)))
             
             (if (and (not (null? my-cards))
                      (null? owned-cards))
                 (ask self 'pick (pick-random my-cards)))
             
             ;lightning
             (if (pair? security-drones-north)
                 (ask self 'use lightning 'north))
             (if (pair? security-drones-south)
                 (ask self 'use lightning 'south))
             (if (pair? security-drones-east)
                 (ask self 'use lightning 'east))
             (if (pair? security-drones-west)
                 (ask self 'use lightning 'west))
             
             ;droids in my room
             (if (and (= lightsaber-charge 0)
                      (not (null? security-drones)))
                 (ask self 'use lightsaber (pick-random security-drones)))
             (if (and (= rifle-charge 0) ;if my lightsaber is not charging
                      (not (null? security-drones))) ;and there is a security drone
                 (ask self 'use rifle (pick-random security-drones)))
             
             ;droids in other rooms
             (if (and (= rifle-charge 0) ;if my lightsaber is not charging
                      (not (null? security-drones-3))) ;and there is a security drone
                 (ask self 'use rifle (pick-random security-drones-3)))
             
             ;bots in my room
             (if (and (= lightsaber-charge 0)
                      (null? owned-cards)
                      (not (null? service-bots)))
                 (ask self 'use lightsaber (pick-random service-bots)))
             
             ;bots in other room
             (if (and (= rifle-charge 0)
                      (null? owned-cards)
                      (not (null? service-bots-3)))
                 (ask self 'use rifle (pick-random service-bots-3)))
             
             (let ((move-target (cond ((contains-generator? my-place)
                                       (ask self 'use bomb)
                                       (set! path (cdadr (search all-neighbours 
                                                                 (lambda (x) (eq? x (car visited)))
                                                                 my-place)))
                                       (car path))
                                      ((and (null? owned-cards)
                                            (pair? (filter (lambda (p) (and (not (eq? p my-place))
                                                                            (contains-key-card? p)))
                                                           my-neighbours-3)))
                                       (define path-to-card (cdadr (search all-neighbours contains-key-card? my-place)))
                                       (if (null? path-to-card)
                                           my-place
                                           (car path-to-card)))
                                      ((not (null? path)) ;if there is a path to follow
                                       (let ((ret (car path)))
                                         (set! path (cdr path)) ;shorten the path
                                         ret)) ;follow the first item on the un-shortened path
                                      ((and (pair? owned-cards)
                                            (not (contains-generator? my-place)))
                                       (let ((path-to-generator (search all-neighbours contains-generator? my-place)))
                                         (set! path (cdadr path-to-generator))
                                         (car path)))
                                      ((pair? my-unvisited-neighbours)
                                       (pick-random my-unvisited-neighbours))
                                      (else
                                       (let ((path-to-unvisited (search all-neighbours 
                                                                        (lambda (x) (and (not (contains? x visited)) (not (is-a x 'protected-room?)))) 
                                                                        my-place)))
                                         (if (path-to-unvisited)
                                             (begin (set! path (cdadr path-to-unvisited)) ;put it on the path
                                                    (car path))
                                             (pick-random my-neighbours))
                                         )))))
               (ask self 'move-to move-target))
             )))
        (else (get-method person message))))))



(define me (make&install-player 'me JFDI-ship))
(apply ask me 'take (list
                     (make&install-lightsaber JFDI-ship 30)
                     (make&install-laser JFDI-ship 30)
                     (make&install-lightning JFDI-ship 30)
                     (make&install-gen-bomb JFDI-ship 30)))


(add-end-game-cond
 (λ ()
   (not (findf (λ(item)
                 (is-a item 'generator?))
               (ask gen-room 'things))))
 (λ ()
   (end-mission)
   (popup-message "You win!" "Congratulations! The generator has been destroyed!")))

(start-mission)
