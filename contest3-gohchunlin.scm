;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file game.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ---------------------------------------------------------------------------

;;; Simple object system with inheritance

(define (ask object message . args)  ;; See your Scheme manual to explain `.'
  (let ((method (get-method object message)))
    (if (method? method)
	(apply method (cons object args))
	(error "No method" message (cadr method)))))

(define (get-method object message)
  (object message))

(define (no-method name)
  (list 'no-method name))

(define (method? x)
  (not (no-method? x)))

(define (no-method? x)
  (if (pair? x)      
      (eq? (car x) 'no-method)
      #f))

;;; ----------------------------------------------------------------------------

;;; Persons, places, and things will all be kinds of named objects
(define (make-named-object name)
  (lambda (message) 
    (case message
      ((name) (lambda (self) name))
      (else (no-method name)))))

;;; Persons and things are mobile since their places can change
(define (make-mobile-object name location)
  (let ((named-obj (make-named-object name)))
    (lambda (message)
      (case message
        ((place) (lambda (self) location))
        ((install)
         (lambda (self)
           (ask location 'add-thing self)))
        ;; Following method should never be called by the user...
        ;;  it is a system-internal method.
        ;; See CHANGE-PLACE instead
        ((set-place)
         (lambda (self new-place)
           (set! location new-place)
           'place-set))
        (else (get-method named-obj message))))))

(define (make&install-mobile-object name place)
  (let ((mobile-obj (make-mobile-object name place)))
    (ask mobile-obj 'install) mobile-obj))

;;; A thing is something that can be owned
(define (make-thing name birthplace)
  (let ((owner 'nobody)
	(mobile-obj (make-mobile-object name birthplace))
        (obj-name name)
        (birthplace2 birthplace))
    (lambda (message)
      (case message
        ((birth-place) (lambda (self) birthplace2))
        ((owner) (lambda (self) owner))
        ((ownable?) (lambda (self) true))
        ((owned?) (lambda (self) (not (eq? owner 'nobody))))
        ;; Following method should never be called by the user...
        ;;  it is a system-internal method.
        ;; See TAKE and LOSE instead.
        ((name?) (lambda (self) obj-name))
        ((set-owner)
         (lambda (self new-owner)
           (set! owner new-owner)
           'owner-set))
        (else (get-method mobile-obj message))))))

(define (make-big-brother)
  (let ((time-now 0)
        (id-store 0))
    (lambda (message)
      (case message
        ((inform) (lambda (self id place)
                    (if (and (equal? id id-store) (equal? *the-time* time-now))
                        (report-stolen-card id);(display id)
                        (begin
                          (set! time-now *the-time*)
                          (set! id-store id))
                        )))))))
(define big-brother (make-big-brother))

(define (make&install-thing name birthplace)	
  (let ((thing (make-thing name birthplace)))
    (ask thing 'install)
    thing))

;;; Implementation of places
(define (make-place name)
  (let ((neighbor-map '())		
	(things       '())
	(named-obj (make-named-object name)))
    (lambda (message)
      (case message
        ((place?) (lambda (self) neighbor-map))
        ((things) (lambda (self) things))
        ((neighbors)
         (lambda (self) (map cdr neighbor-map)))
        ((exits)
         (lambda (self) (map car neighbor-map)))
        ((neighbor-towards)
         (lambda (self direction)
           (let ((places (assq direction neighbor-map)))
             (if places
                 (cdr places)
                 #f))))
        ((add-neighbor)
         (lambda (self direction new-neighbor)
           (cond ((assq direction neighbor-map)
                  (display-message (list "Direction already assigned"
                                         direction name))
                  #f)
                 (else (set! neighbor-map
                            (cons (cons direction new-neighbor) neighbor-map))
                      #t))))
        ((accept-person?)
         (lambda (self person)
           #t))
        ;; Following two methods should never be called by the user...
        ;;  they are system-internal methods. See CHANGE-PLACE instead.
        ((add-thing)
         (lambda (self new-thing)
           (cond ((memq new-thing things)
                  (display-message (list (ask new-thing 'name)
                                         "is already at" name))
                  #f)
                 (else (set! things (cons new-thing things))
                       #t))))
        ((del-thing)
         (lambda (self thing)
           (cond ((not (memq thing things))
                  (display-message (list (ask thing 'name)
                                         "is not at" name))
                  #f)
                 (else (set! things (delq thing things))	;; DELQ defined
                       #t))))                                   ;; below
        (else (get-method named-obj message))))))

(define (wuguo-card-checking possessions)
  (if (null? possessions)
      #f
      (if (is-a (car possessions) 'sd-card?)
          (car possessions)
          (wuguo-card-checking (cdr possessions)))))
(define (make-card-locked-place name)
  (let ((place (make-place name)))
    (lambda (message)
      (case message
        ((accept-person?)
         (lambda (self person)
           (if (wuguo-card-checking (ask (eval person) 'possessions))
               (begin
                 #t
                 (ask big-brother 'inform (ask (eval (wuguo-card-checking (ask (eval person) 'possessions))) 'id) name))
               #f)))
;         (lambda (person)
;           (wuguo-card-checking (ask (eval person) 'possessions))))
        (else (get-method place message))))))

(define (wuguo-id-checker id id-list)
  (if (null? id-list)
      #f
      (if (equal? (car id-list) id)
          #t
          (wuguo-id-checker id (cdr id-list)))))
(define (make-student-residence name)
  (let ((place (make-place name))
        (id-list '()))
    (lambda (message)
      (case message
        ((accept-person?)
         (lambda (self person)
           (if (wuguo-card-checking (ask (eval person) 'possessions))
               (wuguo-id-checker (ask (eval (wuguo-card-checking (ask (eval person) 'possessions))) 'id) id-list)
               #f)))
        ((register-card)
         (lambda (self thing)
           (if (and (equal? (cadr ((ask (eval thing) 'owner) thing)) 'you) (not (wuguo-id-checker (ask (eval thing) 'id) id-list)))
               (begin
                 (set! id-list (cons (ask (eval thing) 'id) id-list))
                 (display "You have register your card. The id number of the card is: ")
                 (display id-list)))))
        (else (get-method place message))))))


;;; ----------------------------------------------------------------------------
;;; Implementation of people
(define (association-procedure proc select)
  (define (helper lst)
    (cond ((null? lst) '())
          ((proc (car lst)) (select (car lst)))
          (else (helper (cdr lst)))))
  (lambda (list)
    (helper list)))

(define find-object
  (lambda (name objlist)
    ((association-procedure
      (lambda (obj) (equal? (ask obj 'name) name))
      (lambda (obj) obj)) objlist)))

(define (make-person name birthplace threshold)
  (let ((possessions '())
	(mobile-obj  (make-mobile-object name birthplace))
        (name2 name))
    (lambda (message)
      (case message
        ((name?) (lambda (self) name2))
        ((person?) (lambda (self) true))
        ((possessions) (lambda (self) possessions))
        ((list-possessions)
         (lambda (self)
           (ask self 'say
                (cons "I have"
                      (if (null? possessions)
                          '("nothing")
                          (map (lambda (p) (ask p 'name))
                               possessions))))
           possessions))
        ((say)
         (lambda (self list-of-stuff)
           (display-message
            (append (list "At" (ask (ask self 'place) 'name)
                          ":"  name "says --")
                    (if (null? list-of-stuff)
                        '("Oh, nevermind.")
                        list-of-stuff)))
           'said))
        ((have-fit)
         (lambda (self)
           (ask self 'say '("Yaaaah! I am upset!"))
           'I-feel-better-now))
        ((look-around)
         (lambda (self)
           (let ((other-things
                  (map (lambda (thing) (ask thing 'name))
                       (delq self                       ;; DELQ
                             (ask (ask self 'place)     ;; defined
                                  'things)))))          ;; below
             (ask self 'say (cons "I see" (if (null? other-things)
                                              '("nothing")
                                              other-things)))
             other-things)))           
        ((take)
         (lambda (self thing)
           (cond ((symbol? thing) ; referencing object by name 
                  (let ((obj (find-object thing (ask (ask self 'place) 'things))))
                    (if (null? obj)
                        #f
                        (ask self 'take obj))))
                 ((memq thing possessions)
                  (ask self 'say
                       (list "I already have" (ask thing 'name)))
                  
                  #t)
                 ((and (let ((things-at-place (ask (ask self 'place) 'things)))
                         (memq thing things-at-place))
                       (is-a thing 'ownable?))
                  (if (ask thing 'owned?)
                      (let ((owner (ask thing 'owner)))
                        (begin 
                          (ask owner 'lose thing)
                          (ask owner 'have-fit)))
                      'unowned)
                  (ask thing 'set-owner self)
                  (set! possessions (cons thing possessions))
                  (ask self 'say
                       (list "I take" (ask thing 'name)))
                  #t)
                 (else
                  (display thing)
                  (display-message
                   (list "You cannot take" (ask thing 'name)))
                  #f))))
        ((lose)
         (lambda (self thing)
           (cond ((symbol? thing) ; referencing object by name 
                  (let ((obj (find-object thing (ask (ask self 'place) 'things))))
                    (if (null? obj)
                        #f
                        (ask self 'lose obj))))
                 ((eq? self (ask thing 'owner))
                  (set! possessions (delq thing possessions)) ;; DELQ
                  (ask thing 'set-owner 'nobody)              ;; defined
                  (ask self 'say                              ;; below
                       (list "I lose" (ask thing 'name)))
                  #t)
                 (else
                  (display-message (list name "does not own"
                                         (ask thing 'name)))
                 #f))))
        ((move)
         (lambda (self)
           (cond ((= (random threshold) 0)
                  (ask self 'act)
                  #t))))
        ((act)
         (lambda (self)
           (let ((new-place (random-neighbor (ask self 'place))))
             (if new-place
                 (ask self 'move-to new-place)
                 #f))))		; All dressed up and no place to go
        ((move-to)
         (lambda (self new-place)
           (let ((old-place (ask self 'place)))
             (cond ((eq? new-place old-place)
                    (display-message (list name "is already at"
                                           (ask new-place 'name)))
                    #f)
                   ((ask new-place 'accept-person? self)
                    (change-place self new-place)
                    (for-each (lambda (p) (change-place p new-place))
                              possessions)
                    (display-message
                     (list name "moves from" (ask old-place 'name)
                           "to"         (ask new-place 'name)))
                    (greet-people self (other-people-at-place self new-place))
                    #t)
                   (else
                    (display-message (list name "can't move to"
                                           (ask new-place 'name))))))))
        ((go)
         (lambda (self direction)
           (let ((old-place (ask self 'place)))
             (let ((new-place (ask old-place 'neighbor-towards direction)))
               (cond (new-place
                      (ask self 'move-to new-place))
                     (else
                      (display-message (list name "cannot go" direction
                                             "from" (ask old-place 'name)))
                      #f))))))
        ((install)
         (lambda (self)
           (add-to-clock-list self)
           ((get-method mobile-obj 'install) self)))
        ((give)         
             (lambda (self person thing)
               (cond ((symbol? thing) ; referencing object by name 
                      (let ((obj (find-object thing (ask (ask self 'place) 'things))))
                        (if (null? obj)
                            #f
                            (if (and (not (equal? (ask (eval thing) 'owner) 'nobody)) (equal? (ask (ask (eval thing) 'owner) 'name?) 'you))
                                (ask (eval person) 'take thing))))))))
        ((register-my-card)
         (lambda (self thing)
           (if (equal? ((ask (eval thing) 'birth-place) thing) ((ask self 'place) self))
               (ask (eval (ask self 'place)) 'register-card thing))))
        ((report) 
         (lambda (self lost-id)
           (report-stolen-card lost-id)))
        ((fire-super-weapon)
         (lambda (self person)
           (if (and (not (equal? (ask self 'name) 'you)) (not (equal? (ask (ask self 'place) 'name) 'heaven)))
               (begin
                 (ask self 'say
                      (list "I am going to kill"
                            person))
                 (ask self 'say
                      (list "Ion Canon fires!!!"))
                 (go-to-heaven (eval person))))))
        ((instant-move) (lambda (self place)
                          (if (and (not (equal? (ask self 'name) 'you)) (not (equal? (ask (ask self 'place) 'name) 'heaven)))
                              (ask self 'move-to (eval place)))))
        ((mind-control) (lambda (self person . instruction)
                          (if (and (not (equal? (ask self 'name) 'you)) (not (equal? (ask (ask self 'place) 'name) 'heaven)) 
                                   (not (equal? (ask (ask (eval person) 'place) 'name) 'heaven)))
                              (begin
                                 (ask (eval person) (car instruction) (cadr instruction))
                                 (ask self 'say (list "I control your mind actually, Lin. Hahaha......")))
                              (ask self 'say (list "Tralala......")))))
        (else (get-method mobile-obj message))))))

(define (make&install-person name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (ask person 'install)
    person))

;;; A troll is a kind of person (but not a kind person!)
(define (report-stolen-card id)
  (define ogre (make&install-troll 'ogre secret-portal 1 id))
  'ogre)

(define (make-troll name birthplace threshold id-eater)
  (let ((person (make-person name birthplace threshold))
        (name2 name))
    (lambda (message)
      (case message
        ((name?) (lambda (self) name2))
        ((act)
         (lambda (self)
           (let ((others (other-people-at-place self (ask self 'place))))
             (if (not (null? others))
                 (ask self 'eat-person (pick-random others))
                 ((get-method person 'act) self)))))
        ((eat-person)
         (lambda (self person)
           (if (equal? id-eater 0)
               ;proffy
               (begin
                 (ask self 'say
                      (list "Growl.... I'm going to eat you,"
                            (ask person 'name)))
                 (go-to-heaven person)
                 (ask self 'say
                      (list "Chomp chomp." (ask person 'name)
                            "tastes yummy!"))
               '*burp*)
               ;ogre
               (if (and (wuguo-card-checking (ask (eval person) 'possessions)) (equal? (ask (eval (wuguo-card-checking (ask (eval person) 'possessions))) 'id) id-eater))
                   (begin
                     (ask self 'say
                          (list "You are the culprit.... I'm going to eat you,"
                                (ask person 'name)))
                     (go-to-heaven person)
                     (ask self 'say
                          (list "Chomp chomp." (ask person 'name)
                                "tastes yummy! Wahahaha......"))
                     '*burp*)))))
        (else (get-method person message))))))

(define (make&install-troll name birthplace threshold id-eater)
  (let ((troll  (make-troll name birthplace threshold id-eater)))
    (ask troll 'install)
    troll))

(define (go-to-heaven person)
  (for-each (lambda (item) (ask person 'lose item))
	    (ask person 'possessions))
  (ask person 'say
       '("
                   Dulce et decorum est 
                   pro computatore mori!
                   WuGuo will be general......"
	 ))
  (ask person 'move-to heaven)
  (remove-from-clock-list person)
  'game-over-for-you-dude)

(define heaven (make-place 'heaven))		; The point of no return



;;; --------------------------------------------------------------------------

;;; Clock routines
(define *clock-list* '())
(define *the-time* 0)

(define (initialize-clock-list)
  (set! *clock-list* '())
  'initialized)

(define (add-to-clock-list person)
  (set! *clock-list* (cons person *clock-list*))
  'added)

(define (remove-from-clock-list person)
  (set! *clock-list* (delq person *clock-list*))  ;; DELQ defined below
  'removed)

(define killed-person '())
(define over? 0)
(define features 0)
(define (choose-from-one name-list)
  (define (iter-helper remainder-list counter)
    (if (= counter 0)
        (car remainder-list)
        (iter-helper (cdr remainder-list) (- counter 1))))
  (iter-helper name-list (random (length name-list))))
(define (clock)
  (set! features (random 4))
  ;(display "features = ")
  ;(display features)
  ;(display (map (lambda(x) (ask x 'name)) *clock-list*))
  (newline)
  (display "---Tick---")
  (set! *the-time* (+ *the-time* 1))
  (set! killed-person (choose-from-one people-list))
  (if (> *the-time* 23)
      (set! over? 1)
      (if (= features 0)
          ((lambda (person) 
             (cond ((equal? (ask person 'name) 'WuGuo) (ask WuGuo 'mind-control 'Lin 'fire-super-weapon (if (equal? killed-person 'WuGuo)
                                                                                                            'Lin
                                                                                                            killed-person)))
                   ((equal? (ask person 'name) 'sand-proffy) (ask sand-proffy 'instant-move (choose-from-one world-place)))
                   ((equal? (ask person 'name) 'Lin) (ask Lin 'fire-super-weapon killed-person))
                   ))
           (choose-from-one
            (if (member you *clock-list*)
                (reverse (cdr (reverse *clock-list*)))
                *clock-list*)))
          (for-each (lambda (person) (ask person 'move))
                    *clock-list*))  )
  'tick-tock)

(define (current-time)
  *the-time*)

(define (run-clock n)
  (cond ((zero? n) 'done)
	(else (clock)
	      (run-clock (- n 1)))))

;;; --------------------------------------------------------------------------

;;; Miscellaneous procedures
(define (is-a object property)
  (let ((method (get-method object property)))
    (if (method? method)
	(ask object property)
	#f)))

(define (change-place mobile-object new-place)	; Since this bridges the gap
  (let ((old-place (ask mobile-object 'place))) ; between MOBILE-OBJECT and
    (ask mobile-object 'set-place new-place)	; PLACE, it is best it not
    (ask old-place 'del-thing mobile-object))	; be internal to either one.
  (ask new-place 'add-thing mobile-object)
  'place-changed)

(define (other-people-at-place person place)
  (filter (lambda (object)
	    (if (not (eq? object person))
		(is-a object 'person?)
		#f))
          (ask place 'things)))

(define (greet-people person people)
  (if (not (null? people))
      (ask person 'say
	   (cons "Hi"
		 (map (lambda (p) (ask p 'name))
                      people)))
      'sure-is-lonely-in-here))

(define (display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (display s) (display " "))
	    list-of-stuff)
  'message-displayed)

(define (random-neighbor place)
  (pick-random (ask place 'neighbors)))

(define (filter predicate lst)
  (cond ((null? lst) '())
	((predicate (car lst))
	 (cons (car lst) (filter predicate (cdr lst))))
	(else (filter predicate (cdr lst)))))

(define (pick-random lst)
  (if (null? lst)
      #f      
      (list-ref lst (random (length lst)))))  ;; See manual for LIST-REF

(define (delq item lst)
  (cond ((null? lst) '())
	((eq? item (car lst)) (delq item (cdr lst)))
	(else (cons (car lst) (delq item (cdr lst))))))

;;; -------------------------------------------------------------------

;;; Other interesting procedures
(define (make&install-sd-card name birthplace id)
  (let ((card (make-sd-card name birthplace id)))
    (ask card 'install)
    card))

(define (make-sd-card name birthplace idnumber)
  (let ((id idnumber)
        (thing (make-thing name birthplace)))
    (lambda (message)
      (case message
        ((sd-card?) (lambda (self) true))
        ((id) (lambda (self) id))
        (else (get-method thing message))))))

(define (copy-sd-card card)
  (let ((name   (symbol-append 'copy-of- (ask card 'name)))
	(place  (ask card 'place))
	(id     (ask card 'id)))
    (make&install-sd-card name place id)))

;;; -------------------------------------------------------------------

;;; symbol-append is available in MIT-GNU Scheme but not in DrScheme
(define (symbol-append sym1 sym2)
  (string->symbol (string-append
                   (symbol->string sym1)
                   (symbol->string sym2))))

;;; -------------------------------------------------------------------

;;; show-thing needs to be ported over to DrScheme

;(define (show thing)
;  (define (global-environment? frame)
;    (environment->package frame))
;  (define (pp-binding name value width)
;    (let ((value* (with-string-output-port
;		   (lambda (port)
;		     (if (pair? value)
;			 (pretty-print value port #F (+ width 2))
;			 (display value port))))))
;      (newline)
;      (display name)
;      (display ": ")
;      (display (make-string (- width (string-length name)) #\Space))
;      (if (pair? value)
;	  (display (substring value* (+ width 2) (string-length value*)))
;	  (display value*))))
;  (define (show-frame frame)
;    (if (global-environment? frame)
;	(display "\nGlobal Environment")
;	(let* ((bindings (environment-bindings frame))
;	       (parent   (environment-parent frame))
;	       (names    (cons "Parent frame"
;			       (map symbol->string (map car bindings))))
;	       (values   (cons (if (global-environment? parent)
;				   'global-environment
;				   parent)
;			       (map cadr bindings)))
;	       (width    (reduce max 0 (map string-length names))))
;	  (for-each (lambda (n v) (pp-binding n v width))
;                    names values))))
;  (define (show-procedure proc)
;    (fluid-let ((*unparser-list-depth-limit* 4)
;		(*unparser-list-breadth-limit* 4))
;      (newline)
;      (display "Frame:")
;      (newline)
;      (display "  ")
;      (if (global-environment? (procedure-environment proc))
;	  (display "Global Environment")
;	  (display (procedure-environment proc)))
;      (newline)
;      (display "Body:")
;      (newline)
;      (pretty-print (procedure-lambda proc) (current-output-port) #T 2)))
;  
;  (define (print-nicely thing)
;    (newline)
;    (display thing)
;    (cond ((equal? #f thing)
;	   'uninteresting)
;	  ((environment? thing)
;	   (show-frame thing))
;	  ((compound-procedure? thing)
;	   (show-procedure thing))
;	  (else 'uninteresting)))
;  
;  (print-nicely
;   (or (if (integer? thing)
;	   (object-unhash thing)
;	   thing)
;       thing)))


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;
;;  Code for adventure game
;; 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(initialize-clock-list)

;; Here we define the places in our world...

;;------------------------------------------
(define world-place '(bus-terminal residence3 amphitheatre gsa children-playground residence4 residence5 bbq1 bbq2 cafe sbs-bus-stop basketball-court plaza food-court fast-food residence2 main-entrance minimarket canteen pond residence1 residence6 multipurpose-hall computer-lab secret-portal))
(define bus-terminal (make-card-locked-place 'bus-terminal))
(define residence3 (make-place 'residence3))
(define amphitheatre (make-place 'amphitheatre))
(define gsa (make-place 'gsa))
(define children-playground (make-place 'children-playground))
(define residence4 (make-place 'residence4))
(define residence5 (make-place 'residence5))
(define bbq1 (make-place 'bbq1))
(define bbq2 (make-place 'bbq2))
(define cafe (make-card-locked-place 'cafe))
(define sbs-bus-stop (make-place 'sbs-bus-stop))
(define basketball-court (make-place 'basketball-court))
(define plaza (make-place 'plaza))
(define food-court (make-place 'food-court))
(define fast-food (make-place 'fast-food))
(define residence2 (make-place 'residence2))
(define main-entrance (make-place 'main-entrance))
(define minimarket (make-place 'minimarket))
(define canteen (make-place 'canteen))
(define pond (make-place 'pond))
(define residence1 (make-card-locked-place 'residence1))
(define residence6 (make-place 'residence6))
(define multipurpose-hall (make-card-locked-place 'multipurpose-hall))
(define computer-lab (make-student-residence 'computer-lab))
(define secret-portal (make-student-residence 'secret-portal))



;; One-way paths connect individual places in the world.

;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

(define (can-go-both-ways from direction reverse-direction to)
  (can-go from direction to)
  (can-go to reverse-direction from))


(can-go-both-ways bus-terminal 'east 'west residence3)
(can-go-both-ways residence3 'east 'west amphitheatre)
(can-go-both-ways residence3 'north 'south fast-food)
(can-go-both-ways amphitheatre 'east 'west gsa)
(can-go-both-ways gsa 'east 'west children-playground)
(can-go-both-ways gsa 'north 'south residence4)
(can-go-both-ways residence4 'north 'south residence5)
(can-go-both-ways residence5 'east 'west bbq1)
(can-go-both-ways residence5 'west 'east plaza)
(can-go-both-ways bbq1 'north 'south bbq2)
(can-go-both-ways bbq2 'east 'west sbs-bus-stop)
(can-go-both-ways bbq2 'west 'east cafe)
(can-go-both-ways cafe 'north 'south basketball-court)
(can-go-both-ways plaza 'north 'south minimarket)
(can-go-both-ways plaza 'west 'east food-court)
(can-go-both-ways food-court 'south 'north fast-food)
(can-go-both-ways food-court 'north 'south residence2)
(can-go-both-ways residence2 'west 'east main-entrance)
(can-go-both-ways residence2 'north 'south residence1)
(can-go-both-ways residence1 'east 'west pond)
(can-go-both-ways pond 'east 'west canteen)
(can-go-both-ways canteen 'east 'west residence6)
(can-go-both-ways canteen 'south 'north minimarket)
(can-go-both-ways minimarket 'west 'east residence2)
(can-go-both-ways minimarket 'down 'up multipurpose-hall)
(can-go-both-ways multipurpose-hall 'south 'north computer-lab)
(can-go-both-ways computer-lab 'west 'east secret-portal)



;; The important critters in our world...
;;---------------------------------------


(define you (make&install-person 'you main-entrance 9999))   ;; Your avatar
(define WuGuo (make&install-person 'WuGuo residence5 1))   
(define sand-proffy (make&install-troll 'sand-proffy children-playground 4 0))
(define Lin (make&install-person 'Lin sbs-bus-stop 2))

(define people-list '(you WuGuo sand-proffy))

(define lost-pgp-residence-card
  (make&install-sd-card 'lost-pgp-residence-card minimarket '888-12-3456))

(define yupi-bear
  (make&install-thing 'yupi-bear basketball-court))

;;
;; Interactive game
;;

;; Helper procedure to tokenize an input string

(define (tokenize s)
  (define (helper s begin end)
    (if (equal? s "") 
        '()        
        (let ((char (string-ref s (- end 1))))
          (cond ((= end (string-length s))
                 (cons begin end))
                ((and (= (+ 1 begin) end) 
                      (char-whitespace? char))
                 (helper s (+ 1 begin) (+ 1 end)))
                ((char-whitespace? char)
                 (cons begin (- end 1)))
                (else (helper s begin (+ 1 end)))))))
  (define (helper2 s)
    (if (null? s)
        '()
        (let ((next-token (helper s 0 1)))
          (if (null? next-token)
              '()
              (let* ((begin (car next-token))
                     (end (cdr next-token)))
                (cons (substring s begin end) (helper2 (substring s end (string-length s)))))))))
  (map string->symbol (if (equal? s "") 
                          '()
                          (helper2 s))))

(define (play-game-interactive)
  (display-game-state)
  (if (= over? 0)
      (begin
        (apply ask (cons you (tokenize (prompt "Command:"))))
        (clock)
        (play-game-interactive))
      (if (and (equal? (ask you 'place) bus-terminal) (equal? (ask yupi-bear 'owner) you))
          (display "Mission Accompalished: Jack has taken his yupi-bear.")
          (display "Game Over: Jack cannot get back his yupi-bear. He is upset......"))))

(define (prompt prompt-string)
  (display prompt-string)
  (read-line))

(define (display-game-state)
  (newline)
  (display "You are at: ")
  (display (ask (ask you 'place) 'name))
  (newline)
  (display "You see: ")
  (display (map (lambda (p) (ask p 'name)) (ask (ask you 'place) 'things)))
  (newline)
  (display "Exits: ")
  (display (ask (ask you 'place) 'exits))
  (newline)
  (newline)
  (newline))

;;; Discription of the game
(display "Story: You are only allowed to take the yupi-bear which is abandoned at a place and bring it to bus-terminal. After that, wait at the bus-terminal until Jack comes. You have to complete this mission before Jack reaches bus-terminal, otherwise you will lose since Jack will leave PGP soon. Watch out three persons who are trying to kill you. They are WuGuo, sand-proffy and Lin. However, they will kill one another, too. This is the final war of the war series. Good luck.")
(newline)
(display "Characters:")
(newline)
(display "WuGuo: He is minor in Biology and he recently try to develop a new technology to control other's mind. He is only able to control Lin's mind now.")
(newline)
(display "sand-proffy: It was first found in SoC1 in year 2007. It can move to any place in a short time and eat person.")
(newline)
(display "Lin: He is minor in Physics and has successfully developed a new super weapon that can kill anybody in anywhere, anytime.")
(newline)
(display "Jack: The owner of yupi-bear.")
(newline)
(newline)

; Uncomment line below to play in interactive mode
(play-game-interactive)


;; The beginning of an ever-expanding automatic game script
;;----------------------------------------------------------

(define (play-game)
  (ask you 'go 'east)
  (ask you 'go 'east)
  (ask you 'look-around)
  (ask you 'take lost-pgp-residence-card)
  (ask you 'go 'south)
  (ask you 'go 'east)
  (ask you 'go 'east)
  (ask you 'go 'north)
  (ask you 'go 'west)
  (ask you 'go 'north)
  (ask sand-proffy 'instant-move 'main-entrance)
  (ask you 'look-around)
  (ask you 'take yupi-bear)
  (ask you 'go 'south)
  (ask you 'go 'east)
  (ask you 'go 'south)
  (ask WuGuo 'mind-control 'Lin 'fire-super-weapon 'sand-proffy)
  (ask you 'go 'west)
  (ask Lin 'go 'west)
  (ask you 'go 'south)
  (ask you 'go 'south)
  (ask Lin 'fire-super-weapon 'WuGuo)
  (ask you 'go 'west)
  (ask you 'go 'west)
  (ask Lin 'fire-super-weapon 'you))

; Now, run the scripted game!
;(play-game)