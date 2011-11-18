;
; CS1101S --- Programming Methodology
;
; Mission 17
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

; After Task 1 is complete, comment out everything that comes before the "Task 2" title
;(require "engine-17a.rkt")   ; engine for the game world
;(init-places)

;(start-mission #f)
;(run-clock 10)
#|
Your answer here
|#
; a) bot1 is more restless
; b) when the robot is called to act it generates a random number from 0 to inertia-1 inclusive and if that number is equal to 0 it
;    moves to a random exit. hence every tick it will move with probability 1/(inertia). hence the lower inertia is the higher
;    the chance that the bot will move on a given turn and the more restless it is.
; c) 1/2 * 1/3 = 1/6. Yes, it happened 2 times when I ran it, which is 1/5 of the time, which is close to 1/6.


;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

; Uncomment the following two lines for Task 2
(require "engine-17b.rkt")   ; engine for the game world
(init-places)


; Remember to comment your changes specific to this task, detailing the changes
; you have made and what your code is trying to achieve!

(define (make-player name birthplace)
  (let ((person (make-person name birthplace)))
    (lambda (message)
      (case message
        ((player?) (lambda (self) #t))
        ((act)
         (lambda (self)
           ((get-method person 'act) self)
           (let* ((lightsaber (pick-random (car (split-list (lambda (x) (is-a x 'weapon?)) (ask self 'possessions))))) ;look for a lightsaber
                  (owned-cards (car (split-list (lambda (x) (is-a x 'key-card?)) (ask self 'possessions)))) ;cards I own
                  (my-place (ask self 'place)) ;place (location) I am at
                  (people (other-people-at-place self my-place)) ;people I with
                  (random-targets (car (split-list (lambda (x) (is-a x 'service-bot?)) people))) ;choose a random service-bot in the current place
                  (charge-turns (ask lightsaber 'charge-turns)) ;how many turns to charge the lightsaber
                  (my-cards (car (split-list (lambda (x) (is-a x 'key-card?)) (ask my-place 'things)))) ;list of cards in my place
                  (my-x-neighbours (car (split-list (lambda (x) (is-a x 'protected-room?)) (ask my-place 'neighbours)))) ;list of all protected rooms I am adjacent to
                  (idle 0)) ;how many actions were skipped    
             (if (pair? my-cards) ;if there are cards in my place
                 (ask self 'pick (pick-random my-cards)) ;pick any one up. spec says I have to pick up even if I already have one.
                 values)
             (if (and (= charge-turns 0) ;if my lightsaber is not charging
                      (not (null? random-targets))) ;and there are some targets
                 (ask self 'use lightsaber (pick-random random-targets)) ;attack any target
                 (set! idle (+ 1 idle)))
             (if (and (pair? owned-cards) ;if I have a card
                      (pair? my-x-neighbours)) ;and there is a protected room next to me
                 (ask self 'move-to (pick-random my-x-neighbours))
                 (set! idle (+ 1 idle)))
           (if (= idle 2) ;if I haven't attacked or moved to a protected room
               (ask self 'move-to (random-neighbour my-place)) ;move randomly
               values))))
        (else (get-method person message))))))

(define (make&install-player name birthplace)
  (let ((player (make-player name birthplace)))
    (ask (register player) 'install)
    player))

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

; Uncomment the following line for Task 2
(start-mission)