;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file classes.rkt
;;;;  Used in the Contest
;;;;  Contains definition of various classes used by the
;;;;  game.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module classes racket
  (require "oop-lib.rkt")
  (provide (except-out (all-defined-out)
                       ROOM-MAX-CAP
                       BEAM-DURATION)
           (all-from-out "oop-lib.rkt"))
  
  (define ROOM-MAX-CAP 5)
  (define BEAM-DURATION 3)
  
  (define gen-room #f)
  (define drone-count 0)
  
  (define outer-rooms '())
  (define faint-dur 3)
  
  (define-struct effect (name duration repeated proc) #:transparent) ; status effect
  (define-struct damage (pred amount) #:transparent)
  
  (define-struct dice (min max)
    #:transparent
    #:guard
    (λ (mn mx type)
      (cond ((not (exact-nonnegative-integer? mn))
             (raise-type-error type "exact-nonnegative-integer" 0 mn mx))
            ((not (exact-nonnegative-integer? mx))
             (raise-type-error type "exact-nonnegative-integer" 1 mn mx))
            (else
             (values (min mn mx) (max mn mx)))))
    #:property prop:procedure
    (λ (self)
      (let ((mn (dice-min self)))
        (+ mn (random (- (dice-max self) mn -1))))))
  
  ;;; General functions
  (define (set-gen-room! g-r)
    (unless (or gen-room
                (not (is-a g-r 'place?)))
      (set! gen-room g-r)))
  
  (define (set-drone-count! d-c)
    (set! drone-count d-c))
  
  (define (open-output-file-wrapper fname)
    (open-output-file fname #:mode 'text #:exists 'truncate/replace))
  
  ;;; Persons, places, and things will all be kinds of named objects
  (define (make-named-object name)
    (λ (message) 
      (case message
        ((name) (λ (self) name))
        ((named-obj?) (λ (self) #t))
        (else (no-method name)))))
  
  ;;; Persons and things are mobile since their places can change
  (define (make-mobile-object name location)
    (let ((named-obj (make-named-object name)))
      (lambda (message)
        (case message
          ((place) (λ (self) location))
          ((mobile-obj?) (λ (self) #t))
          ((install)
           (λ (self)
             (ask location 'add-thing self)))
          ;; Following method should never be called by the user...
          ;;  it is a system-internal method.
          ;; See CHANGE-PLACE instead
          ((set-place)
           (λ (self new-place)
             (set! location new-place)
             'place-set))
          (else (get-method named-obj message))))))
  
  (define (make&install-mobile-object name place)
    (let ((mobile-obj (make-mobile-object name place)))
      (ask mobile-obj 'install) mobile-obj))
  
  ;;; A thing is something that can be owned
  (define (make-thing name birthplace)
    (let ((owner 'nobody)
          (mobile-obj (make-mobile-object name birthplace)))
      (λ (message)
        (case message
          ((owner) (λ (self) owner))
          ((thing? ownable?) (λ (self) #t))
          ((owned?) (λ (self) (not (eq? owner 'nobody))))
          ;; Following method should never be called by the user...
          ;;  it is a system-internal method.
          ;; See TAKE and LOSE instead.
          ((set-owner)
           (λ (self new-owner)
             (set! owner new-owner)
             'owner-set))
          (else (get-method mobile-obj message))))))
  
  (define (make&install-thing name birthplace)	
    (let ((thing (make-thing name birthplace)))
      (ask thing 'install)
      thing))
  
  ;;; A placeable is a thing that has health
  (define (make-placeable name birthplace health)
    (let ((core (make-living-thing name birthplace health 0))
          (thing (make-thing name birthplace)))
      (λ (message)
        (case message
          ((ownable?) (λ (self) #f))
          ((placeable?) (λ (self) #t))
          (else
           (let ((core-mtd (get-method core message)))
             (if (method? core-mtd)
                 core-mtd
                 (get-method thing message))))))))
  
  (define (make&install-placeable name birthplace health)
    (let ((p (make-placeable name birthplace health)))
      (ask p 'install)
      p))
  
  ;;; Special Placeable: Generator
  (define (make-generator birthplace)
    (let ((placeable (make-placeable 'generator birthplace 10)))
      (λ (message)
        (case message
          ((generator?) (λ (self) #t))
          (else (get-method placeable message))))))
  
  (define (make&install-generator birthplace)
    (let ((g (make-generator birthplace)))
      (ask g 'install)
      g))
  
  ;;; Implementation of places
  (define w-exits '(north east south west))
  (define a-exits '(up down))
  (define (make-place name)
    (let ((neighbour-map '())		
          (things       '())
          (named-obj (make-named-object name))
          (max-cap ROOM-MAX-CAP))
      (define (space-left self)
        (- max-cap (length (filter (λ(x)(is-a x 'person?)) things))))
      (λ (message)
        (case message
          ((place?) (λ (self) #t))
          ((things) (λ (self) things))
          ((neighbors neighbours)
           (λ (self) (map cdr neighbour-map)))
          ((exits)
           (λ (self) (map car neighbour-map)))
          ((neighbor-towards neighbour-towards)
           (λ (self direction)
             (cond ((assq direction neighbour-map) => cdr)
                   (else #f))))
          ((add-neighbor add-neighbour)
           (λ (self direction new-neighbour)
             (cond ((assq direction neighbour-map)
                    (display-message (list "Direction already assigned"
                                           direction name))
                    #f)
                   (else (set! neighbour-map
                               (cons (cons direction new-neighbour) neighbour-map))
                         (let ((exits (ask self 'exits)))
                           (set-room-wall-widths! self
                                             (map (λ (exit)
                                                    (if (memq exit exits)
                                                        wall-width
                                                        wall-blocked-width))
                                                  w-exits))
                           (set-room-wall-colours! self
                                                   (map (λ (exit)
                                                          (cond ((ask self 'neighbour-towards exit)
                                                                 => (λ (n)
                                                                      (if (or (is-a self 'protected-room?)
                                                                              (is-a n 'protected-room?))
                                                                          wall-protected-colour
                                                                          default-colour)))
                                                                (else default-colour)))
                                                        w-exits))
                           (set-room-arrows! self
                                             (map (λ (exit)
                                                    (memq exit exits))
                                                  a-exits))
                           (set-room-arrow-colours! self
                                                    (map (λ (exit)
                                                           (cond ((ask self 'neighbour-towards exit)
                                                                  => (λ (n)
                                                                       (if (is-a n 'protected-room?)
                                                                           wall-protected-colour
                                                                           default-colour)))
                                                                 (else default-colour)))
                                                         a-exits)))
                         #t))))
          ((space-left?) space-left)
          ((accept-person?)
           (λ (self person)
             (positive? (space-left self))))
          ((resize)
           (λ (self new-size)
             (set! max-cap new-size)))
          ;; Following two methods should never be called by the user...
          ;;  they are system-internal methods. See CHANGE-PLACE instead.
          ((add-thing)
           (λ (self new-thing)
             (cond ((memq new-thing things)
                    (display-message (list (ask new-thing 'name)
                                           "is already at" name))
                    #f)
                   (else (set! things (cons new-thing things))
                         #t))))
          ((del-thing)
           (λ (self thing)
             (cond ((not (memq thing things))
                    (display-message (list (ask thing 'name)
                                           "is not at" name))
                    #f)
                   (else (set! things (remq thing things))
                         #t))))
          (else (get-method named-obj message))))))
  
  (define (make-protected-room name)
    (let ((place (make-place name)))
      (λ (message)
        (case message
          ((protected-room?) (λ (x) #t))
          ((accept-person?)
           (λ (self person)
             (and (findf (λ (x)
                           (is-a x 'key-card?))
                         (ask person 'possessions))
                  ((get-method place 'accept-person?) self person))))
          (else
           (get-method place message))))))
  
  ;;; Implementation of living-thing
  (define (make-living-thing name birthplace health regeneration)
    (let ((mobile-obj (make-mobile-object name birthplace))
          (max-health health)
          (effects '())
          (activate-in 1))
      (define (parse-effects self effect-list)
        (for-each (λ (effct)
                    (when (or (<= (effect-duration effct) 0) ; expired
                              (effect-repeated effct))
                      ((effect-proc effct) self (effect-duration effct))))
                  effect-list)
        (set! effects (filter (λ (effct) ; update list to remove expired ones
                                (> (effect-duration effct) 0))
                              effect-list)))
      (define (add-effect self effct)
        (if (<= (effect-duration effct) 0) ; one-off, run it instantly
            ((effect-proc effct) self (effect-duration effct))
            (set! effects (cons effct effects)))) ; otherwise add to effect list
      (λ (message)
        (case message
          ((living-thing?) (λ (self) #t))
          ((suffer) (λ (self damage) ; person suffers damage
                      (cond ((= damage +inf.0) ; Instant KO!
                             (set! health 0)
                             (display-message `(,(ask self 'name)
                                                "has been killed instantly!"))
                             (evacuate self)
                             'i-am-dead)
                            ((and (number? damage) ; normal damage
                                  (positive? damage)
                                  (exact? damage))
                             (set! health (- health damage))
                             (display-message `(,(ask self 'name)
                                                "takes" ,damage "damage!"))
                             (when (<= health 0) ; dead
                               (evacuate self)
                               'i-am-dead))
                            (else
                             'invalid-damage))))
          ((heal) (λ (self amount)
                    (cond ((= amount +inf.0) ; Full Heal!
                           (set! health max-health)
                           (display-message `(,(ask self 'name)
                                              "has been fully healed!"))
                           #t)
                          ((and (number? amount) ; normal heal
                                (positive? amount)
                                (exact? amount))
                           (set! health (if (> (+ health amount) max-health)
                                            max-health
                                            (+ health amount)))
                           (display-message `(,(ask self 'name)
                                              "heals" ,amount ,(if (= amount 1)
                                                                   "point!"
                                                                   "points!")))
                           #t)
                          (else
                           'invalid-heal))))
          ((faint stun immobilise immobilize)
           (λ (self dur)
             (set! activate-in (if (and (number? dur)
                                        (positive? dur))
                                   dur
                                   1))))
          ((faint-dur stun-dur immobile-turns)
           (λ (self) activate-in))
          ((act) (λ (self) ; clock procedure, processes regeneration and statuses
                   (define (do-regen amount)
                     (set! health (+ health amount))
                     (display-message `(,(ask self 'name)
                                        "regenerates" ,amount ,(if (= amount 1)
                                                                   "point"
                                                                   "points")))
                     (display-message `(,(ask self 'name)
                                        "has" ,health
                                        "out of" ,max-health "health points"))
                     #t)
                   ; decrease effect times and execute repeated / expired ones
                   (let ((new-effect-lst (map (λ (effct)
                                                (make-effect
                                                 (effect-name effct)
                                                 (sub1 (effect-duration effct))
                                                 (effect-repeated effct)
                                                 (effect-proc effct))) effects)))
                     (parse-effects self new-effect-lst))
                   (when (< 0 health max-health) ; regenerate only if between 0 and max-health
                     (let ((new-health (+ health regeneration)))
                       (do-regen
                        (if (> new-health max-health)
                            (- max-health health)
                            regeneration))))
                   (when (number? activate-in)
                     (set! activate-in (- activate-in 1))
                     (when (< activate-in 1)
                       (set! activate-in #f))
                     (check-active self))))
          ((add-effect)
           (λ (self effct)
             (when (effect? effct) ; process new effect
               (add-effect self effct))))
          ((remove-effect)
           (λ (self remove-pred)
             (set! effects (filter (negate remove-pred) effects))))
          ((health)
           (λ (self) health))
          ((max-health)
           (λ (self) max-health))
          ((modify-max-health)
           (λ (self new-max)
             (set! max-health new-max)))
          ((modify-regeneration)
           (λ (self new-regen)
             (set! regeneration new-regen)))
          (else (get-method mobile-obj message))))))
  
  (define (make-person name birthplace)
    (let ((living-thing  (make-living-thing name birthplace 50 1))
          (possessions '())
          (weapon-cmds '())
          (extra-cmds '())
          (prev-loc #f))
      (λ (message)
        (case message
          ((person?) (λ (self) #t))
          ((possessions) (λ (self) possessions))
          ((say)
           (λ (self list-of-stuff)
             (display-message
              (append (list "At" (ask (ask self 'place) 'name)
                            ":"  name "says --")
                      (if (null? list-of-stuff)
                          '("Oh, nevermind.")
                          list-of-stuff)))
             self))           
          ((take pick)
           (λ (self . things)
             (let-values (((can-take cannot-take)
                           (partition
                            (λ (thing)
                              (and (let ((things-at-place (ask (ask self 'place) 'things)))
                                     (memq thing things-at-place))
                                   (is-a thing 'ownable?)
                                   (not (ask thing 'owned?)))) things)))
               (unless (null? can-take)
                 (ask self 'say
                      (list* "I take" (list-names can-take)))
                 (for-each (λ (thing)
                             (ask thing 'set-owner self))
                           can-take)
                 (set! possessions (append can-take possessions)))
               (unless (null? cannot-take)
                 (display-message
                  (list* name "cannot take" (list-names cannot-take)))))))
          ((lose drop)
           (λ (self . things)
             (let-values (((can-drop cannot-drop)
                           (partition
                            (λ (thing)
                              (eq? self (ask thing 'owner)))
                            things)))
               (unless (null? can-drop)
                 (set! possessions (remq* can-drop possessions))
                 (for-each (λ (thing)
                             (ask thing 'set-owner 'nobody))
                           can-drop)
                 (ask self 'say
                      (list* "I lose" (list-names can-drop))))
               (unless (null? cannot-drop)
                 (display-message (list* name "does not own"
                                         (list-names cannot-drop)))))))
          ((use)
           (λ (self thing . effect-arguments)
             [cond ((eq? self (ask thing 'owner))
                    (let ((effect (get-method thing 'use)))
                      (if (method? effect)
                          (apply effect thing self effect-arguments)
                          (display-message `("~~ Use:"
                                             ,(ask thing 'name)
                                             "does not do anything! ~~"))))
                    #t)
                   (else
                    (display-message (list name "does not own"
                                           (ask thing 'name)))
                    #f)]))
          ((move-to)
           (λ (self new-place #:test-connected [tc #t])
             (let ((old-place (ask self 'place)))
               (cond ((eq? new-place old-place)
                      (display-message (list name "is already at"
                                             (ask new-place 'name)))
                      #f)
                     ((and (ask new-place 'accept-person? self)
                           (or (not tc)
                               (memq new-place (ask old-place 'neighbours))))
                      (change-place self new-place)
                      (set! prev-loc old-place)
                      (display-message
                       (list name "moves from" (ask old-place 'name)
                             "to" (ask new-place 'name)))
                      (for-each (lambda (p) (change-place p new-place))
                                possessions))
                     (else
                      (display-message (list name "can't move to"
                                             (ask new-place 'name)))
                      #f)))))
          ((prev-loc) (λ (self) prev-loc))
          ((go)
           (λ (self direction)
             (let ((old-place (ask self 'place)))
               (let ((new-place (ask old-place 'neighbour-towards direction)))
                 (cond (new-place
                        (ask self 'move-to new-place))
                       (else
                        (display-message (list name "cannot go" direction
                                               "from" (ask old-place 'name)))
                        #f))))))
          ((new-attack)
           (λ (self weapon-cmd the-weapon)
             (if (eq? self (ask the-weapon 'owner))
                 (if (is-a the-weapon 'weapon?)
                     (let ((new-cmd (symbol-append (ask the-weapon 'verb)
                                                   '- weapon-cmd)))
                       (set! weapon-cmds
                             `((,new-cmd . ,the-weapon)
                               ,@(filter
                                  (λ (cmd) ; over-write if necessary
                                    (not (eq? (car cmd) new-cmd)))
                                  weapon-cmds)))
                       (display-message `("~~"
                                          ,(ask the-weapon 'name)
                                          "can now be used by the"
                                          ,new-cmd
                                          "command. ~~")))
                     (display-message `("**"
                                        ,(ask the-weapon 'name)
                                        "is not a weapon **")))
                 (display-message (list name "does not own"
                                        (ask the-weapon 'name))))))
          ((act)
           (λ (self)
             ((get-method living-thing 'act) self)
             (for-each (λ (item)
                         (when (method? (get-method item 'act))
                           (ask item 'act)))
                       possessions)))
          ((install)
           (λ (self)
             (add-to-clock-list self)
             ((get-method living-thing 'install) self)))
          ((add-mtd)
           (λ (self tag mtd)
             (set! extra-cmds (cons (cons tag mtd) extra-cmds))))
          (else
           (cond ((assq message weapon-cmds)
                  => (λ (cmd)
                       (λ (self . attack-args)
                         (apply ask self 'use (cdr cmd) attack-args))))
                 ((assq message extra-cmds)
                  => cdr)
                 (else
                  (get-method living-thing message))))))))
  
  (define (make-service-bot name birthplace inertia)
    (let ((person (make-person name birthplace)))
      (λ (message)
        (case message
          ((service-bot? bot?) (λ (self) #t))
          ((act)
           (λ (self)
             ((get-method person 'act) self)
             (when (= (random inertia) 0)
               (ask self 'go (pick-random (ask (ask self 'place) 'exits))))))
          (else (get-method person message))))))
  
  
  
  
  ;; Game related procedures
  ;;---------------------------
  (define JFDI-ship
    (make-place 'JFDI-ship))
  (ask JFDI-ship 'resize +inf.0)
  
  (define (add-outer-room rm)
    (set! outer-rooms (cons rm outer-rooms)))
  
  (define (activated? person)
    (not (eq? (ask person 'place) JFDI-ship)))
  
  (define (check-active person)
    (cond ((ask person 'stun-dur)
           (raise not-activated))
          ((not (activated? person))
           (activate person))))
  
  (define (activate person)
    (let ((new-room (let iter ((o-r outer-rooms))
                      (let ((rm (pick-random o-r)))
                        (if (or (not rm)
                                (ask rm 'accept-person? person))
                            rm
                            (iter (remq rm o-r)))))))
      (when new-room
        (when (< (ask person 'health) (ask person 'max-health))
          (ask person 'heal +inf.0))
        (ask person 'say '(Entering enemy territory))
        (ask person 'move-to new-room #:test-connected #f)
        (set-obj-moved! person #f))))
  
  (define (evacuate person)
    (with-handlers (((list/c moved-message continuation?)
                     (λ (exn)
                       ((cadr exn)))))
      (cond ((is-a person 'player?)
             (ask person 'say '(I need help here!))
             (apply ask person 'lose (filter (λ (item)
                                               (not (is-a item 'weapon?)))
                                             (ask person 'possessions)))
             (ask person 'faint faint-dur)
             (ask person 'move-to JFDI-ship #:test-connected #f))
            ((or (is-a person 'bot?)
                 (is-a person 'drone?))
             (ask person 'say '(ENERGY LEVELS CRITICAL. SHUTTING DOWN.))
             (apply ask person 'lose (filter (λ (item)
                                               (not (is-a item 'weapon?)))
                                             (ask person 'possessions)))
             (when (is-a person 'drone?)
               (set! drone-count (- drone-count 1)))
             (remove-from-clock-list person)
             (let ((bin (make-place 'bin)))
               (change-place person bin)))
            ((is-a person 'placeable?)
             (remove-from-clock-list person)
             (let ((bin (make-place 'bin)))
               (change-place person bin)))))))