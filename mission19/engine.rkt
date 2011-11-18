;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file engine.rkt
;;;;  Used in Chapter 6
;;;;  Defines remaining classes and prepares for init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;
;;  Code for adventure game
;; 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(module engine racket/gui
  (require "classes.rkt")
  (provide (except-out (all-defined-out) bot-locations)
           (except-out (all-from-out "classes.rkt")
                       line-proc process-killrec
                       shift-obj! add-to-draw-queue add-to-tick-list
                       (struct-out room) (struct-out obj)))
  
  (define num-game-stats 4) ; to exclude the traitor tag
  
  (define started-time #f)
  (define run-limit 150000)
  
  (define score-table (make-hash))
  (define full-score-table (make-hash))
  (define points-table #hash(((player? . player?) . 5) ((player? . drone?) . 5) ((player? . bot?) . 10) ((player? . generator?) . 100)))
  
  (define layout-bitmap (make-parameter '()))
  (define random-room-nums (make-parameter #f))
  (define min-bot-inertia (make-parameter 2))
  (define max-bot-inertia (make-parameter 4))
  (define spawn-drone-on-bot-death (make-parameter #f))
  (define spawn-drone-on-card-pick (make-parameter #f))
  (define alarm-cycle (make-parameter 5))
  
  (define max-drones (make-parameter 30))
  
  (define attack-dur 3)
  (define attack-z 5)
  (define level-rows 2)
  (define attack-colour "RED")
  (define attack-thick 0.1)
  (define attack-style 'solid)
  
  (define expl-dur 5)
  
  ;; One-way paths connect individual places in the world.
  
  ;;------------------------------------------------------
  
  (define (can-go from direction to)
    (ask from 'add-neighbour direction to))
  
  (define (can-go-both-ways from direction reverse-direction to)
    (can-go from direction to)
    (can-go to reverse-direction from))
  
  ;; Here we define the places in our world...
  
  ;;------------------------------------------
  (define bot-locations '())
  
  (define (init-places)
    (let ((nums (build-list (expt num-rms 3) add1)))
      (let flrloop ((flr 1)
                    (lst (layout-bitmap)))
        (unless (or (> flr num-rms)
                    (null? lst))
          (let ((x-off (* (+ num-rms 1) (quotient (- flr 1) 2)))
                (y-off (* (+ num-rms 1) (remainder (- flr 1) 2))))
            (let numloop ((r 1)
                          (lst (car lst)))
              (unless (or (> r num-rms)
                          (null? lst))
                (let colloop ((c 1)
                              (lst (car lst)))
                  (unless (or (> c num-rms)
                              (null? lst))
                    (let ((room-ref (symbol-append 'l flr '- r '- c))
                          (room-num (pick-random nums))
                          (room-dat (car lst)))
                      (for-each eval
                                `((define ,room-ref (,(if (zero? (remainder room-dat 2))
                                                          'make-place 'make-protected-room)
                                                     ',(symbol-append 'room- (if (random-room-nums) room-num (+ (* 100 flr) (* 10 r) c)))))
                                  (register-place ,room-ref ,(* rm-size (+ c x-off)) ,(* rm-size (+ r y-off)) rm-size rm-size)))
                      (let bititer ((bitmap (quotient room-dat 2))
                                    (weight 2))
                        (unless (zero? bitmap)
                          (let-values (((quot rem) (quotient/remainder bitmap 2)))
                            (unless (zero? rem)
                              (if (= weight 64)
                                  (set! bot-locations (cons (eval room-ref) bot-locations))
                                  (eval (case weight
                                          ((2)  `(can-go-both-ways ,room-ref 'west 'east ,(symbol-append 'l flr '- r '- (- c 1))))
                                          ((4)  `(can-go-both-ways ,room-ref 'north 'south ,(symbol-append 'l flr '- (- r 1) '- c)))
                                          ((8)  `(can-go-both-ways ,room-ref 'down 'up ,(symbol-append 'l (- flr 1) '- r '- c)))
                                          ((16) `(set-gen-room! ,room-ref))
                                          ((32) `(add-outer-room ,room-ref))))))
                            (bititer quot (* weight 2)))))
                      (set! nums (remq room-num nums)))
                    (colloop (+ c 1) (cdr lst))))
                (numloop (+ r 1) (cdr lst)))))
          (flrloop (+ flr 1) (cdr lst)))))
    (when gen-room
      (make&install-generator gen-room))
    ;; Service bots
    ;;-------------
    (let ((min-inert (min-bot-inertia))
          (max-inert (max-bot-inertia)))
      (let botiter ((locations bot-locations)
                    (inertia min-inert)
                    (count 1))
        (unless (null? locations)
          (eval `(make&install-bot ',(symbol-append 'b count) ,(car locations) ,inertia))
          (botiter (cdr locations) (+ min-inert (modulo (- inertia -1 min-inert) (- max-inert min-inert -1))) (+ count 1))))))
  
  ;; More classes that couldn't be included earlier as they need the places to be defined first
  ;;----------------------------
  
  ;;;;;;;;;;
  ; Weapon ;
  ;;;;;;;;;;
  (define (make-weapon name birthplace cooldown damages-list effects-list)
    (let ((charge-cycle 0)
          (thing (make-thing name birthplace))
          (atk-colour attack-colour)
          (atk-thick 0.1)
          (atk-style 'solid))
      (define (find-damage target key)
        (foldl (λ(a b)
                 (if ((damage-pred a) target)
                     (let ((dmg1
                            (let ((amt (damage-amount a)))
                              (if (procedure? amt)
                                  (key amt)
                                  amt))))
                       (if (number? b)
                           (max dmg1 b)
                           dmg1))
                     b)) #f damages-list))
      (λ (message)
        (case message
          ((weapon?) (λ (self) #t)) ; type identifier
          ((verb) (λ (self) 'use)) ; for new-attack
          ((note-pvp?) (λ (self) #t))
          ((max-damage)
           (λ (self tgt)
             (cond ((find-damage tgt dice-max)
                    => identity)
                   (else 0))))
          ((min-damage)
           (λ (self tgt)
             (cond ((find-damage tgt dice-min)
                    => identity)
                   (else 0))))
          ((use)
           (λ (self wielder #:draw-attack [d-atk #t] . targets) ; wielder uses weapon to attack targets
             (if (ask self 'charging?) ; complain that it's charging
                 (display-message `("||"
                                    ,(ask wielder 'name)
                                    "cannot use"
                                    ,(ask self 'name)
                                    "as it is still charging... ||"))
                 (let ((target-lst (filter (λ (target) ; only attack living-things
                                             (and
                                              (is-a target 'living-thing?)
                                              (findf 
                                               (λ (dmg)
                                                 ((damage-pred dmg) target))
                                               damages-list))) targets)))
                   (display-message (list* "**"
                                           (ask wielder 'name)
                                           "uses"
                                           (ask self 'name)
                                           "to attack:"
                                           (list-names
                                            (filter
                                             (λ (target) ; damage exists?
                                               (findf 
                                                (λ (dmg)
                                                  ((damage-pred dmg) target))
                                                damages-list))
                                             target-lst))))
                   (for-each
                    (λ (target) ; find max applicable damage
                      (let ((damage-found
                             (find-damage target (λ(x)(x)))))
                        (when damage-found
                          (when (and (not (is-a wielder 'drone?))
                                     (not (eq? target wielder))
                                     (ask self 'note-pvp?)
                                     (not (is-a target 'drone?))
                                     (not (is-a target 'bot?))
                                     (not (is-a target 'placeable?)))
                            (ask wielder 'add-mtd 'drone? (λ (self) #t))
                            (display-message `(,(ask wielder 'name) has attacked
                                                                    ,(symbol-append (ask target 'name) '!)))
                            (display-message `(,(ask wielder 'name) declared rogue by JFDI Council!))
                            (hash-update! score-table wielder
                                          (λ (scorelst)
                                            (append scorelst '(traitor)))
                                          '(0 0 0 0)))
                          (when (and d-atk
                                     (obj? target))
                            (draw-attack wielder target attack-dur atk-colour atk-thick atk-style))
                          (when (eq? (ask target 'suffer damage-found) 'i-am-dead)
                            (let/cc c
                              (raise (list wielder target c))))
                          (unless (null? effects-list)
                            (ask target 'add-effect (pick-random
                                                     effects-list))))))
                    target-lst)
                   (set! charge-cycle cooldown)))))
          ((set-atk-style)
           (λ (self style)
             (set! atk-style style)))
          ((set-atk-colour set-atk-color)
           (λ (self colour)
             (set! atk-colour colour)))
          ((set-atk-thick set-atk-thickness)
           (λ (self thick)
             (set! atk-thick thick)))
          ((act)
           (λ (self)
             (when (> charge-cycle 0)
               (set! charge-cycle (- charge-cycle 1)))
             (when (and (> charge-cycle 0)
                        (ask self 'owned?))
               (display-message
                `("||"
                  ,(ask (ask self 'owner) 'name)
                  can only use
                  ,(ask self 'name)
                  after ,charge-cycle more
                  ,(if (= charge-cycle 1)
                       "turn."
                       "turns.")
                  "||")))))
          ((charge-turns)
           (λ (self)
             (if (> charge-cycle 0)
                 charge-cycle
                 0)))
          ((charging?)
           (λ (self)
             (> charge-cycle 0)))
          (else (get-method thing message))))))
  
  ;;;;;;;;;;;;;;;;
  ; Melee Weapon ;
  ;;;;;;;;;;;;;;;;
  (define (make-melee-weapon name birthplace cooldown damages-list effects-list)
    (let ((weapon (make-weapon name birthplace cooldown
                               damages-list effects-list)))
      (λ (message)
        (case message
          ((melee-weapon?) (λ (self) #t)) ; type identifier
          ((verb) (λ (self) 'swing))
          ((use)
           (λ (self wielder target) ; one target
             (let ((things-around (ask (ask self 'place) 'things)))
               (when (memq target things-around)
                 ((get-method weapon 'use) self wielder target)))))
          (else (get-method weapon message))))))
  
  (define (make&install-melee-weapon name birthplace cooldown damages-list effects-list)
    (let ((m-wpn (make-melee-weapon name birthplace cooldown damages-list effects-list)))
      (ask m-wpn 'install)
      m-wpn))
  
  ;;;;;;;;;;;;;;;;;
  ; Ranged Weapon ;
  ;;;;;;;;;;;;;;;;;
  (define (range-test rm range)
    (cons rm (apply append (map (λ(dir)
                                  (dir-range-test (ask rm 'neighbour-towards dir) dir range))
                                (ask rm 'exits)))))
  (define (dir-range-test rm dir range)
    (let ((next-rm (ask rm 'neighbour-towards dir)))
      (cond ((or (= range 0)
                 (is-a rm 'protected-room?)) '())
            ((or (= range 1)
                 (not next-rm)) (list rm))
            (else
             (cons rm (dir-range-test next-rm dir (- range 1)))))))
  
  (define (make-ranged-weapon name birthplace range
                              cooldown damages-list effects-list)
    (let ((weapon (make-weapon name birthplace cooldown
                               damages-list effects-list)))
      (λ (message)
        (case message
          ((ranged-weapon?) (λ (self) #t)) ; type identifier
          ((range) (λ (self) range))
          ((verb) (λ (self) 'fire)) ; for new-attack
          ((use)
           (λ (self wielder target)
             (if (memq target
                       (apply append (map (λ (rm)
                                            (ask rm 'things))
                                          (range-test (ask self 'place) range))))
                 ((get-method weapon 'use) self wielder target)
                 (display-message `("**" ,(ask wielder 'name) "cannot hit" ,(ask target 'name) "with" ,(ask self 'name) "**")))))
          (else (get-method weapon message))))))
  
  (define (make&install-ranged-weapon name birthplace range cooldown damages-list effects-list)
    (let ((r-wpn (make-ranged-weapon name birthplace range cooldown damages-list effects-list)))
      (ask r-wpn 'install)
      r-wpn))
  
  ;;;;;;;;;;;;;;;;
  ; Spell Weapon ;
  ;;;;;;;;;;;;;;;;
  (define (make-spell name birthplace range
                      cooldown damages-list effects-list)
    (let ((weapon (make-weapon name birthplace cooldown
                               damages-list effects-list)))
      (λ (message)
        (case message
          ((spell?) (λ (self) #t)) ; type identifier
          ((range) (λ (self) range))
          ((verb) (λ (self) 'shoot)) ; for new-attack
          ((use)
           (λ (self wielder dir)
             (apply (get-method weapon 'use)
                    self wielder (remq* (list wielder)
                                        (apply append
                                               (map (λ (rm)
                                                      (ask rm 'things))
                                                    (dir-range-test (ask self 'place) dir (+ range 1))))))))
          (else (get-method weapon message))))))
  
  (define (make&install-spell name birthplace range cooldown damages-list effects-list)
    (let ((spell (make-spell name birthplace range cooldown damages-list effects-list)))
      (ask spell 'install)
      spell))
  
  ;;;;;;;;
  ; Bomb ;
  ;;;;;;;;
  (define (make-bomb name birthplace range countdown
                     damages-list effects-list)
    (let ((weapon (make-weapon name birthplace countdown
                               damages-list effects-list))
          (active #f)
          (bomb-loc #f)
          (atk-colour #f)
          (setter #f))
      (λ (message)
        (case message
          ((bomb?) (λ (self) #t)) ; type identifier
          ((range) (λ (self) range))
          ((verb) (λ (self) 'set)) ; for new-attack
          ((note-pvp?) (λ (self) #f))
          ((countdown) (λ (self) countdown))
          ((set-atk-colour set-atk-color)
           (λ (self colour)
             (set! atk-colour colour)))
          ((act)
           (λ (self)
             ((get-method weapon 'act) self)
             (when active
               (set! active (- active 1))
               (when (< active 1)
                 (let ((affected-rms (range-test bomb-loc range))) ; NOTE: fixed from M19, range instead of +1
                   (keyword-apply (get-method weapon 'use)
                                  '(#:draw-attack)
                                  '(#f)
                                  self setter
                                  (apply append (map (λ (rm)
                                                       (ask rm 'things))
                                                     affected-rms)))
                   (for-each (λ (rm)
                               (when (room? rm)
                                 (set-room-expl-col! rm atk-colour)
                                 (set-room-expl-dur! rm expl-dur)
                                 (add-to-draw-queue rm-z expl-dur rm)))
                             affected-rms)
                   (set! active #f))))))
          ((use)
           (λ (self wielder)
             (unless active
               (set! bomb-loc (ask self 'place))
               (set! active countdown)
               (set! setter wielder))))
          (else (get-method weapon message))))))
  
  (define (make&install-bomb name birthplace range countdown damages-list effects-list)
    (let ((bomb (make-bomb name birthplace range countdown damages-list effects-list)))
      (ask bomb 'install)
      bomb))
  
  ;;;;;;;;;;;;;;;;;;;;
  ; Weapon Factories ;
  ;;;;;;;;;;;;;;;;;;;;
  ;; NOTE: Changed for contest: more damage for zapray, one turn cooldown
  (define zapray-damages
    (list (make-damage (λ (person)
                         (and (not (is-a person 'bot?))
                              (not (is-a person 'drone?))
                              (findf (λ (p)
                                       (is-a p 'key-card?))
                                     (ask person 'possessions))))
                       (make-dice 11 30))))
  (define (make&install-zapray place)
    (let ((zapray (make&install-melee-weapon 'zapray place 1 zapray-damages '())))
      (ask zapray 'set-atk-colour "gold")
      (ask zapray 'set-atk-thick 0.5)
      zapray))
  
  
  ;;;;;;;;;;;;;;;;;;;;
  ; Weapon Factories ;
  ;;;;;;;;;;;;;;;;;;;;
  (define (saber-damages lvl)
    (list (make-damage (λ (tgt) (is-a tgt 'person?)) (make-dice (floor (+ 1/2 (* 3/2 lvl))) (floor (+ 1/2 (* 9/5 lvl)))))))
  (define (make&install-lightsaber place . stats)
    (make&install-melee-weapon 'lightsaber place 1 (apply saber-damages stats) '()))
  
  (define (laser-damages lvl)
    (list (make-damage (λ (tgt) (is-a tgt 'person?)) (make-dice (floor (* 3/4 lvl)) (floor (* 3/2 lvl))))))
  
  (define (make&install-laser place . stats)
    (let ((laser (make&install-ranged-weapon 'laser place 3 2 (apply laser-damages stats) '())))
      (ask laser 'set-atk-colour "MediumBlue")
      (ask laser 'set-atk-thick 0.2)
      laser))
  
  (define (lightning-damages lvl)
    (list (make-damage (λ (tgt) (or (is-a tgt 'bot?)
                                    (is-a tgt 'drone?)))
                       (make-dice (floor (/ lvl 4)) (floor (/ lvl 2))))))
  
  (define (make&install-lightning place . stats)
    (let ((lightning (make&install-spell 'lightning place 2 1 (apply lightning-damages stats) '())))
      (ask lightning 'set-atk-colour "MediumVioletRed")
      (ask lightning 'set-atk-thick 0.1)
      lightning))
  
  (define (gen-bomb-damages lvl)
    (list (make-damage (λ (tgt) (is-a tgt 'generator?)) +inf.0)
          (make-damage (λ (tgt) (is-a tgt 'person?)) (make-dice 1 (floor (/ lvl 2))))))
  
  (define (make&install-gen-bomb place . stats)
    (make&install-bomb 'gen-bomb place 1 3 (apply gen-bomb-damages stats) '()))
  
  
  ;; Recording procedures
  ;;-------------------------
  (define (killproc killer victim)
    (unless (eq? killer victim)
      (let ((killer-tag (findf (λ (tag)
                                 (is-a killer tag))
                               '(player? drone? bot?)))
            (victim-tag (findf (λ (tag)
                                 (is-a victim tag))
                               '(generator? drone? player? bot?))))
        (when (and killer-tag victim-tag)
          (when (is-a killer 'player?)
            (hash-update! score-table killer
                          (λ (scorelist)
                            (list*
                             (+ (car scorelist) 1)
                             (cadr scorelist)
                             (+ (caddr scorelist)
                                (hash-ref points-table (cons killer-tag victim-tag) 0))
                             (if (is-a victim 'generator?)
                                 (+ (cadddr scorelist) 1)
                                 (cadddr scorelist))
                             (cddddr scorelist)))
                          '(0 0 0 0))
            (cond ((and (spawn-drone-on-bot-death)
                        (is-a victim 'bot?)
                        (ask victim 'prev-loc))
                   => (λ (prev-loc)
                        (make&install-drone prev-loc)))))
          (when (is-a victim 'player?)
            (hash-update! score-table victim
                          (λ (scorelist)
                            (list*
                             (car scorelist)
                             (+ (cadr scorelist) 1)
                             (cddr scorelist)))
                          '(0 0 0 0)))
          (board-update (sort
                         (hash-map score-table
                                   (λ (key value)
                                     (cons (ask key 'name) value)))
                         (λ (a b)
                           (let ((ptsA (cadddr a))
                                 (ptsB (cadddr b))
                                 (killsA (cadr a))
                                 (killsB (cadr b))
                                 (evacsA (caddr a))
                                 (evacsB (caddr b)))
                             (or (> ptsA ptsB)
                                 (and (= ptsA ptsB)
                                      (or (> killsA killsB)
                                          (and (= killsA killsB)
                                               (<= evacsA evacsB)))))))))))))
  (process-killrec killproc)
  
  (define (find-winners)
    (let* ((table (hash-map score-table cons))
           (sorted-table (sort
                          table
                          (λ (a b)
                            (let ((ptsA (cadddr a))
                                  (ptsB (cadddr b))
                                  (killsA (cadr a))
                                  (killsB (cadr b))
                                  (evacsA (caddr a))
                                  (evacsB (caddr b)))
                              (or (> ptsA ptsB)
                                  (and (= ptsA ptsB)
                                       (or (> killsA killsB)
                                           (and (= killsA killsB)
                                                (<= evacsA evacsB))))))))))
      (for-each (λ (x)
                  (hash-update! full-score-table (car x)
                                (λ (gs)
                                  (map + (take (cdr x) num-game-stats)
                                       (take gs num-game-stats))))) table)
      (displayln "Cumulative:")
      (displayln (hash-map score-table (λ (key val)
                                         (cons (ask key 'name)
                                               (append val
                                                       (obj-stats key))))))
      (map (λ (pl)
             (cons (ask (car pl) 'name) (cdr pl)))
           (cond ((findf (λ (x) (> (car (cddddr x)) 0)) table)
                  => (λ (x) (list x (car (remq x table)))))
                 (else
                  (let ((pts2 (cadddr (cadr table)))
                        (kills2 (cadr (cadr table)))
                        (evacs2 (caddr (cadr table))))
                    (filter (λ (x)
                              (or (>= (cadddr x) pts2)
                                  (and (= (cadddr x) pts2)
                                       (or (>= (cadr x) kills2)
                                           (and (= (cadr x) kills2)
                                                (<= (caddr x) evacs2))))))
                            table)))))))
  
  ;;;;;;;;;;;;;;;;;
  ;; Player Init ;;
  ;;;;;;;;;;;;;;;;;
  #;(define (init-players)
      (let ((vars '(make-player shortname sabercolour lasercolour spellcolour bombcolour)))
        (let iter ((p players))
          (unless (null? p)
            (let ((pname (caar p))
                  (gamestats (list (cadar p) (caddar p) ; kills, evacs, points, wins
                                   (car (cdddar p)) (cadr (cdddar p))))
                  (stats (cddr (cdddar p)))) ; level
              (let undef ((v vars))
                (unless (null? v)
                  (with-handlers ((exn:fail:contract:variable? void))
                    (namespace-undefine-variable! (car v)))
                  (undef (cdr v))))
              (load (string-append "submissions/showdown-" (symbol->string pname) ".rkt"))
              (let ((player (make-player pname JFDI-ship)))
                (set-obj-stats! (register player) stats)
                (ask player 'install)
                (with-handlers ((exn:fail:contract:variable? void))
                  (set-obj-name! player shortname))
                (let ((wpns
                       (map (λ (wpn colour)
                              (when colour
                                (ask wpn 'set-atk-colour colour))
                              wpn)
                            (list
                             (apply make&install-lightsaber JFDI-ship stats)
                             (apply make&install-laser JFDI-ship stats)
                             (apply make&install-lightning JFDI-ship stats)
                             (apply make&install-gen-bomb JFDI-ship stats))
                            (map (λ (c)
                                   (with-handlers ((exn:fail:contract:variable? (λ (exn) #f)))
                                     (send the-color-database find-color (eval c))))
                                 (cddr vars)))))
                  (apply ask player 'take wpns)
                  (ask player 'modify-max-health (* 3 (caddr stats)))
                  (ask player 'modify-regeneration (floor (/ (caddr stats) 20)))
                  (for-each (λ (a b) (ask player 'new-attack a b))
                            '(saber laser lightning bomb)
                            wpns)
                  (hash-update! score-table player identity '(0 0 0 0))
                  (hash-update! full-score-table player identity gamestats))))
            (iter (cdr p))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; Remaining Classes ;;
  ;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-security-drone birthplace)
    (let ((person (make-person (string->symbol
                                (string-append "d"
                                               (number->string drone-count 16)))
                               birthplace))
          (spawn-cycle (alarm-cycle))
          (visited '()))
      (define (spawn threshold)
        (when (and (< drone-count (max-drones))
                   (= (random threshold) 0))
          (make&install-drone birthplace)))
      (set-drone-count! (+ drone-count 1))
      (λ (message)
        (case message
          ((security-drone? drone?) (λ (self) #t))
          ((act)
           (λ (self)
             ((get-method person 'act) self)
             (let* ((my-place (ask self 'place))
                    (others-here (other-people-at-place self my-place)))
               (set! spawn-cycle (- spawn-cycle 1))
               (when (< spawn-cycle 1)
                 (let ((spawn-loc-occ (length (filter (λ (obj)
                                                        (is-a obj 'person?))
                                                      (ask birthplace 'things)))))
                   (set! spawn-cycle (alarm-cycle))
                   (when (< spawn-loc-occ rm-space)
                     (spawn (+ 1 spawn-loc-occ)))))
               (let ((thieves-around
                      (filter (λ (person)
                                (and (not (is-a person 'bot?))
                                     (not (is-a person 'drone?))
                                     (findf (λ (p)
                                              (is-a p 'key-card?))
                                            (ask person 'possessions))))
                              others-here)))
                 (cond ;; NOTE: Changed for contest: will attack only one person
                   ((and (not (null? thieves-around))
                         (findf (λ (item)
                                  (and (is-a item 'weapon?)
                                       (not (is-a item 'charging?))))
                                (ask self 'possessions)))
                    => (λ (wpn)
                         (cond ((pick-drone-target thieves-around)
                                => (λ (thief)
                                     (ask self 'use wpn thief))))))))
               (cond
                 ((findf (λ (item)
                           (and (is-a item 'key-card?)
                                (not (is-a item 'owned?))))
                         (ask my-place 'things))
                  => (λ (kc)
                       (ask self 'take kc))))
               (let* ((neighbours (ask (ask self 'place) 'neighbours))
                      (unvisited
                       (filter
                        (λ (rm)
                          (not (memq rm visited)))
                        neighbours)))
                 (if (null? unvisited)
                     (ask self 'goto (pick-random neighbours))
                     (ask self 'goto (pick-random unvisited)))))))
          ((goto)
           (λ (self place)
             (when place ; NOTE: Changed for bugtrap
               (set! visited (cons place visited))
               (ask self 'move-to place))))
          (else (get-method person message))))))
  
  
  
  (define (make&install-bot name birthplace inertia)
    (let ((bot (make-service-bot name birthplace inertia)))
      (ask (register bot) 'install)
      (ask bot 'take (make&install-key-card (ask bot 'place)))
      bot))
  
  (define (make&install-drone birthplace)
    (let ((drone (make-security-drone birthplace)))
      (ask (register drone) 'install)
      (ask drone 'take (make&install-zapray birthplace))
      (ask drone 'take (make&install-key-card birthplace))
      drone))
  
  (define alarm-gone-off #f)
  (define (make-key-card birthplace)
    (let ((thing (make-thing 'key-card birthplace)))
      (λ (message)
        (case message
          ((key-card?) (λ (self) #t))
          ((set-owner)
           (λ (self new-owner)
             (when (and gen-room
                        (spawn-drone-on-card-pick)
                        (not (eq? new-owner 'nobody))
                        (not alarm-gone-off)
                        (not (is-a new-owner 'bot?))
                        (not (is-a new-owner 'drone?)))
               (set! alarm-gone-off #t)
               (make&install-drone gen-room))
             ((get-method thing 'set-owner) self new-owner)))
          (else (get-method thing message))))))
  
  (define (make&install-key-card birthplace)
    (let ((key-card (make-key-card birthplace)))
      (ask key-card 'install)
      key-card))
  
  ;; Drawing procedures
  ;;-------------------------
  
  (define draw-attack
    (letrec ((full (λ (x1 y1 x2 y2 z d c w s)
                     (when (and x1 y1 x2 y2)
                       (let ((p (new dc-path%))
                             (pen (with-handlers ((exn:fail? (λ(x)#f)))
                                    (send the-pen-list find-or-create-pen c w s))))
                         (send p move-to x1 y1)
                         (send p line-to x2 y2)
                         (apply add-to-draw-queue z d (cons p pen)
                                (let iter ((squares (grid x1 y1 x2 y2))
                                           (rooms '()))
                                  (if (null? squares) rooms
                                      (let ((r (grid-to-room (car squares))))
                                        (if (eq? r full-redraw)
                                            `((,full-redraw-z 1 ,r))
                                            (iter (cdr squares) (cons `(,rm-z 1 ,r) rooms)))))))))))
             (self
              (case-lambda
                ((from to)
                 (self from to attack-dur #f #f #f))
                ((f t dur)
                 (self f t dur #f #f #f))
                ((f t d color width style)
                 (full (obj-x f) (obj-y f) (obj-x t) (obj-y t) attack-z d color width style))
                ((x1 y1 x2 y2 z d c w s)
                 (full x1 y1 x2 y2 z d c w s)))))
      self))
  
  (line-proc draw-attack)
  
  (define (grid-to-room pair)
    (let ((x (floor (/ (car pair) rm-size)))
          (y (floor (/ (cdr pair) rm-size)))
          (sect-size (+ num-rms 1)))
      (if (or (= (remainder x sect-size) 0)
              (= (remainder y sect-size) 0))
          full-redraw
          (eval (symbol-append 'l (+ 1 (* level-rows (floor (/ x sect-size)))
                                     (floor (/ y sect-size))) '-
                                                              (remainder y sect-size) '-
                                                              (remainder x sect-size))))))
  
  (define (grid x1 y1 x2 y2)
    (let-values (((x1 y1 x2 y2)
                  (if (< x2 x1)
                      (values x2 y2 x1 y1)
                      (values x1 y1 x2 y2))))
      (if (= x1 x2)
          (let ((lower (floor (/ (min y1 y2) rm-size)))
                (upper (ceiling (/ (max y1 y2) rm-size)))
                (x-val (* (floor (/ x1 rm-size)) rm-size)))
            (build-list (- upper lower)
                        (λ (x)
                          (cons x-val
                                (* (+ lower x) rm-size)))))
          (let ((line (λ (x)
                        (+ (* (/ (- y2 y1)
                                 (- x2 x1))
                              x)
                           y1
                           (* (/ (- y1 y2)
                                 (- x2 x1))
                              x1)))))
            (let-values (((top-sel btm-sel)
                          (if (> y2 y1)
                              (values (λ (l r) r) (λ (l r) l))
                              (values (λ (l r) l) (λ (l r) r)))))
              (let iter ((curx x1))
                (if (> curx x2)
                    '()
                    (let* ((lb (* rm-size (floor (/ curx rm-size))))
                           (ub (+ rm-size lb)))
                      (append
                       (let ((left-v (line curx))
                             (right-v (line (min ub x2))))
                         (let ((top (top-sel left-v right-v))
                               (btm (btm-sel left-v right-v)))
                           (build-list (- (ceiling (/ top rm-size)) (floor (/ btm rm-size)))
                                       (λ (n)
                                         (cons lb
                                               (* rm-size (+ (floor (/ btm rm-size)) n)))))))
                       (iter ub)))))))))))