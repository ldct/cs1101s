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
	(mobile-obj (make-mobile-object name birthplace)))
    (lambda (message)
      (case message
        ((owner) (lambda (self) owner))
        ((ownable?) (lambda (self) true))
        ((owned?) (lambda (self) (not (eq? owner 'nobody))))
        ;; Following method should never be called by the user...
        ;;  it is a system-internal method.
        ;; See TAKE and LOSE instead.
        ((set-owner)
         (lambda (self new-owner)
           (set! owner new-owner)
           'owner-set))
        (else (get-method mobile-obj message))))))

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
        ((place?) (lambda (self) true))
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

(define (make-line name stations)
  (let ((named-obj (make-named-object name)))
    (lambda (message)
      (case message
        ((stations) (lambda (self) stations))
        (else (get-method named-obj message))))))

(define (make-track name)
  (let ((place (make-place name)))
    (lambda (message)
      (case message
        ((track?) (lambda (self) true))
        ((accept-person?)
         (lambda (self person)
           (and (not (is-a (ask person 'place) 'track?)) (not (null? (filter (lambda (x) (is-a x 'train?)) (ask self 'things)))))))
        ((accept-train?)
         (lambda (self train)
           (null? (filter (lambda (x) (is-a x 'train?)) (ask self 'things)))))
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
	(mobile-obj  (make-mobile-object name birthplace)))
    (lambda (message)
      (case message
        ((check) (lambda (self package)
                   (cond ((symbol? package)
                          (let ((obj (find-object package (ask (ask self 'place) 'things))))
                            (if (null? obj)
                                #f
                                (ask self 'check obj))))
                         (else (if (is-a (eval package) 'package?)
                                   (display-message (list (ask package 'name) "is worth" (ask package 'value) "points and is to be delivered to" (ask (ask package 'destination) 'name))))))))
        ((wait) (lambda (self) (display-message (list "./) you whistle merrily doing nothing in particular ./)"))))
        ((hop-onto) (lambda (self direction)
                      (let* ((place (ask self 'place))
                             (new-place (if place (ask place 'neighbor-towards direction) #f))
                             (things (if new-place (ask new-place 'things) #f))
                             (trains (if things (filter (lambda (t) (is-a t 'train?)) things) #f)))
                        (if trains
                            (cond ((null? trains) (clock) (ask self 'hop-onto direction))
                                  (else (ask self 'go direction)))
                            (display-message (list "You cannot find any train at" direction "to hop onto from" (ask (ask self 'place) 'name)))))))
        ((travel-to) (lambda (self station)
                       (let* ((line (substring (symbol->string station) 0 2))
                              (station-number (string->number (substring (symbol->string station) 2)))
                              (current-line (substring (symbol->string (ask (ask self 'place) 'name)) 0 2))
                              (valid-line (cond ((equal? line current-line) (ask (eval (string->symbol line)) 'stations))))
                              (valid-station (if (and valid-line station-number) (and (>= station-number 1) (<= station-number valid-line)) #f))
                              (destination (if valid-station (eval (symbol-append station '-p)) #f)))
                         (if (and destination (is-a (ask self 'place) 'track?))
                             (cond ((memq destination (ask (ask self 'place) 'neighbors)) (ask self 'move-to destination))
                                   (else (clock) (ask self 'travel-to station)))
                             (display-message (list station "is not a valid destination from" (ask (ask self 'place) 'name)))))))
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
                        (ask owner 'lose thing)
                        (ask owner 'have-fit))
                      'unowned)
                  (ask thing 'set-owner self)
                  (set! possessions (cons thing possessions))
                  (ask self 'say
                       (list "I take" (ask thing 'name)))
                  (ask self 'check thing)
                  #t)
                 (else
                  (display thing)
                  (display-message
                   (list "You cannot take" (ask thing 'name)))
                  #f))))
        ((deliver)
         (lambda (self package)
           (cond ((symbol? package)
                  (let ((obj (find-object package (ask (ask self 'place) 'things))))
                    (if (null? obj)
                        #f
                        (ask self 'deliver obj))))
                 (else (if (equal? (ask self 'place) (ask package 'destination))
                           (begin
                             (set! possessions (delq package possessions))
                             (ask package 'set-owner 'nobody)
                             (ask package 'deliver)
                             (display-message (list (ask package 'name) "successfully delivered! You gain" (ask package 'value) "points and" (ask package 'bonus) "extra ticks!"))
                             #t)
                           (display-message (list "You cannot deposit" (ask package 'name) "here")))))))
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
                      (display-message (list "You cannot go" direction
                                             "from" (ask old-place 'name)))
                      #f))))))
        ((install)
         (lambda (self)
           (add-to-clock-list self)
           ((get-method mobile-obj 'install) self)))
        (else (get-method mobile-obj message))))))

(define (make&install-person name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (ask person 'install)
    person))

(define (make-train birthplace dir1 dir2)
  (let ((mobile-obj (make-mobile-object 'MRT birthplace))
        (passengers '()))
    (lambda (message)
      (case message
        ((train?) (lambda (self) true))
        ((move) (lambda (self)
                  (cond ((< (random 10) 9)
                         (ask self 'act)
                         #t))))
        ((act)
         (lambda (self)
           (let ((new-place (ask (ask self 'place) 'neighbor-towards dir1)))
               (if new-place 
                   (ask self 'move-to new-place)
                   (ask self 'go dir2)))))
        ((go)
         (lambda (self direction)
           (let ((old-place (ask self 'place)))
             (let ((new-place (ask old-place 'neighbor-towards direction)))
               (cond (new-place
                      (ask self 'move-to new-place))
                     (else
                      (display-message (list "MRT cannot go" direction
                                             "from" (ask old-place 'name)))
                      #f))))))
        ((move-to)
         (lambda (self new-place)
           (let ((old-place (ask self 'place)))
             (if (is-a new-place 'track?)
                 (if (ask new-place 'accept-train? self)
                     (begin
                       (for-each (lambda (p) (change-place p new-place))
                                 (ask (ask self 'place) 'things))
                       (display-message
                        (list "MRT moves from" (substring (symbol->string (ask old-place 'name)) 0 4)
                              "to"         (substring (symbol->string (ask new-place 'name)) 0 4)))
                       #t)
                     (display-message
                      (list "Traffic jam! MRT is unable to proceed to" (ask new-place 'name))))
                 (display-message (list "MRT can't move to"
                                        (ask new-place 'name)))))))
        ((install)
         (lambda (self)
           (add-to-clock-list self)
           ((get-method mobile-obj 'install) self)))
        (else (get-method mobile-obj message))))))

(define (make&install-train birthplace dir1 dir2)
  (let ((train (make-train birthplace dir1 dir2)))
    (ask train 'install)
    train))

(define (make-package name birthplace destination value bonus)
  (let ((thing (make-thing name birthplace)))
    (lambda (message)
      (case message
        ((package?) (lambda (self) true))
        ((value) (lambda (self) value))
        ((bonus) (lambda (self) bonus))
        ((destination) (lambda (self) destination))
        ((deliver) (lambda (self) 
                     (set! score (+ value score))
                     (set! total-time (+ bonus total-time))
                     (ask (ask self 'place) 'del-thing self)))
        (else (get-method thing message))))))

(define (make&install-package name birthplace destination value bonus)
  (let ((package (make-package name birthplace destination value bonus)))
    (ask package 'install)
    package))


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

(define (clock)
  (newline)
  (display "---Tick---")
  (cond ((= total-time *the-time*) (display-message (list "Game Over - Your score is" score)) #f)
        (else
         (set! *the-time* (+ *the-time* 1))
         (for-each (lambda (person) (ask person 'move))
                   *clock-list*)
         'tick-tock
         #t)))

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
;(define central-library (make-place 'central-library))

(define EW (make-line (string->symbol "East West Line") 27))

(define e-hanger (make-track (string->symbol "East Hangar")))

(define EW1-e (make-track (string->symbol "EW1 Pasir Ris Terminal - Eastbound Track")))
(define EW1-p (make-place (string->symbol "EW1 Pasir Ris Terminal - Platform")))
(define EW1-w (make-track (string->symbol "EW1 Pasir Ris Terminal - Westbound Track")))

(define EW2-e (make-track (string->symbol "EW2 Tampines - Eastbound Track")))
(define EW2-p (make-place (string->symbol "EW2 Tampines - Platform")))
(define EW2-w (make-track (string->symbol "EW2 Tampines - Westbound Track")))

(define EW3-e (make-track (string->symbol "EW3 Simei - Eastbound Track")))
(define EW3-p (make-place (string->symbol "EW3 Simei - Platform")))
(define EW3-w (make-track (string->symbol "EW3 Simei - Westbound Track")))

(define EW4-e (make-track (string->symbol "EW4 Tanah Merah Interchange - Eastbound Track")))
(define EW4-p (make-place (string->symbol "EW4 Tanah Merah Interchange - Platform")))
(define EW4-c (make-track (string->symbol "EW4 Tanah Merah Interchange - Centre Track")))
(define EW4-w (make-track (string->symbol "EW4 Tanah Merah Interchange - Westbound Track")))

(define EW5-e (make-track (string->symbol "EW5 Bedok - Eastbound Track")))
(define EW5-p (make-place (string->symbol "EW5 Bedok - Platform")))
(define EW5-w (make-track (string->symbol "EW5 Bedok - Westbound Track")))

(define EW6-e (make-track (string->symbol "EW6 Kembangan - Eastbound Track")))
(define EW6-p (make-place (string->symbol "EW6 Kembangan - Platform")))
(define EW6-w (make-track (string->symbol "EW6 Kembangan - Westbound Track")))

(define EW7-e (make-track (string->symbol "EW7 Eunos - Eastbound Track")))
(define EW7-p (make-place (string->symbol "EW7 Eunos - Platform")))
(define EW7-w (make-track (string->symbol "EW7 Eunos - Westbound Track")))

(define EW8-e (make-track (string->symbol "EW8 Paya Lebar - Eastbound Track")))
(define EW8-p (make-place (string->symbol "EW8 Paya Lebar - Platform")))
(define EW8-w (make-track (string->symbol "EW8 Paya Lebar - Westbound Track")))

(define EW9-e (make-track (string->symbol "EW9 Aljunied - Eastbound Track")))
(define EW9-p (make-place (string->symbol "EW9 Aljunied - Platform")))
(define EW9-w (make-track (string->symbol "EW9 Aljunied - Westbound Track")))

(define EW10-e (make-track (string->symbol "EW10 Kallang - Eastbound Track")))
(define EW10-p (make-place (string->symbol "EW10 Kallang - Platform")))
(define EW10-w (make-track (string->symbol "EW10 Kallang - Westbound Track")))

(define EW11-e (make-track (string->symbol "EW11 Lavender - Eastbound Track")))
(define EW11-p (make-place (string->symbol "EW11 Lavender - Platform")))
(define EW11-w (make-track (string->symbol "EW11 Lavender - Westbound Track")))

(define EW12-e (make-track (string->symbol "EW12 Bugis - Eastbound Track")))
(define EW12-p (make-place (string->symbol "EW12 Bugis - Platform")))
(define EW12-w (make-track (string->symbol "EW12 Bugis - Westbound Track")))

(define EW13-e (make-track (string->symbol "EW13 City Hall Interchange - Eastbound Track")))
(define EW13-p (make-place (string->symbol "EW13 City Hall Interchange - Platform")))
(define EW13-w (make-track (string->symbol "EW13 City Hall Interchange - Westbound Track")))

(define EW14-e (make-track (string->symbol "EW14 Raffles Place Interchange - Eastbound Track")))
(define EW14-p (make-place (string->symbol "EW14 Raffles Place Interchange - Platform")))
(define EW14-w (make-track (string->symbol "EW14 Raffles Place Interchange - Westbound Track")))

(define EW15-e (make-track (string->symbol "EW15 Tanjong Pagar - Eastbound Track")))
(define EW15-p (make-place (string->symbol "EW15 Tanjong Pagar - Platform")))
(define EW15-w (make-track (string->symbol "EW15 Tanjong Pagar - Westbound Track")))

(define EW16-e (make-track (string->symbol "EW16 Outram Park Interchange - Eastbound Track")))
(define EW16-p (make-place (string->symbol "EW16 Outram Park Interchange - Platform")))
(define EW16-w (make-track (string->symbol "EW16 Outram Park Interchange - Westbound Track")))

(define EW17-e (make-track (string->symbol "EW17 Tiong Bahru - Eastbound Track")))
(define EW17-p (make-place (string->symbol "EW17 Tiong Bahru - Platform")))
(define EW17-w (make-track (string->symbol "EW17 Tiong Bahru - Westbound Track")))

(define EW18-e (make-track (string->symbol "EW18 Redhill - Eastbound Track")))
(define EW18-p (make-place (string->symbol "EW18 Redhill - Platform")))
(define EW18-w (make-track (string->symbol "EW18 Redhill - Westbound Track")))

(define EW19-e (make-track (string->symbol "EW19 Queenstown - Eastbound Track")))
(define EW19-p (make-place (string->symbol "EW19 Queenstown - Platform")))
(define EW19-w (make-track (string->symbol "EW19 Queenstown - Westbound Track")))

(define EW20-e (make-track (string->symbol "EW20 Commonwealth - Eastbound Track")))
(define EW20-p (make-place (string->symbol "EW20 Commonwealth - Platform")))
(define EW20-w (make-track (string->symbol "EW20 Commonwealth - Westbound Track")))

(define EW21-e (make-track (string->symbol "EW21 Buona Vista - Eastbound Track")))
(define EW21-p (make-place (string->symbol "EW21 Buona Vista - Platform")))
(define EW21-w (make-track (string->symbol "EW21 Buona Vista - Westbound Track")))

(define EW22-e (make-track (string->symbol "EW22 Dover - Eastbound Track")))
(define EW22-p (make-place (string->symbol "EW22 Dover - Platform")))
(define EW22-w (make-track (string->symbol "EW22 Dover - Westbound Track")))

(define EW23-e (make-track (string->symbol "EW23 Clementi - Eastbound Track")))
(define EW23-p (make-place (string->symbol "EW23 Clementi - Platform")))
(define EW23-w (make-track (string->symbol "EW23 Clementi - Westbound Track")))

(define EW24-e (make-track (string->symbol "EW24 Jurong East Interchange - Eastbound Track")))
(define EW24-p (make-place (string->symbol "EW24 Jurong East Interchange - Platform")))
(define EW24-w (make-track (string->symbol "EW24 Jurong East Interchange - Westbound Track")))

(define EW25-e (make-track (string->symbol "EW25 Chinese Garden - Eastbound Track")))
(define EW25-p (make-place (string->symbol "EW25 Chinese Garden - Platform")))
(define EW25-w (make-track (string->symbol "EW25 Chinese Garden - Westbound Track")))

(define EW26-e (make-track (string->symbol "EW26 Lakeside - Eastbound Track")))
(define EW26-p (make-place (string->symbol "EW26 Lakeside - Platform")))
(define EW26-w (make-track (string->symbol "EW26 Lakeside - Westbound Track")))

(define EW27-e (make-track (string->symbol "EW27 Boon Lay Terminal - Eastbound Track")))
(define EW27-p (make-place (string->symbol "EW27 Boon Lay Terminal - Platform")))
(define EW27-w (make-track (string->symbol "EW27 Boon Lay Terminal - Westbound Track")))

(define w-hanger (make-track (string->symbol "West Hangar")))






;; One-way paths connect individual places in the world.

;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

(define (can-go-both-ways from direction reverse-direction to)
  (can-go from direction to)
  (can-go to reverse-direction from))

(define (link-stations prefix start change end suffix dir)
  (if (not (= start end))
      (let ((point1 (symbol-append (symbol-append prefix (string->symbol (number->string start))) suffix))
            (point2 (symbol-append (symbol-append prefix (string->symbol (number->string (+ start change)))) suffix)))
        (can-go (eval point1) dir (eval point2))
        (link-stations prefix (+ start change) change end suffix dir))))

(link-stations 'EW 1 1 27 '-w 'west)
(link-stations 'EW 27 -1 1 '-e 'east)

(can-go EW1-e 'east e-hanger)
(can-go e-hanger 'west EW1-w)

(can-go EW27-w 'west w-hanger)
(can-go w-hanger 'east EW27-e)

(define (build-station prefix start end suffix1 suffix2 dir1 dir2)
  (if (not (> start end))
      (let* ((station (symbol-append prefix (string->symbol (number->string start))))
             (point1 (symbol-append station suffix1))
             (point2 (symbol-append station suffix2)))
        (can-go-both-ways (eval point1) dir1 dir2 (eval point2))
        (build-station prefix (+ start 1) end suffix1 suffix2 dir1 dir2))))

(build-station 'EW 1 27 '-e '-p 'south 'north)
(build-station 'EW 1 27 '-p '-w 'south 'north)



;; The important critters in our world...
;;---------------------------------------

(define you (make&install-person 'you EW21-p 999))   ;; Your avatar
(define train1 (make&install-train e-hanger 'east 'west))
(define train2 (make&install-train EW9-e 'east 'west))
(define train3 (make&install-train EW20-e 'east 'west))
(define train4 (make&install-train w-hanger 'east 'west))
(define train5 (make&install-train EW19-w 'east 'west))
(define train6 (make&install-train EW11-w 'east 'west))

(define (random-platform)
  (cond ((= 0 (random 1)) (eval (string->symbol (string-append "EW" (number->string (+ (random 27) 1)) "-p")))) ; Only EW line is activated
        ((= 0 (random 3)) (eval (string->symbol (string-append "ns" (number->string (+ (random 27) 1)) "-p"))))
        ((= 0 (random 2)) (eval (string->symbol (string-append "ne" (number->string (+ (random 17) 1)) "-p"))))
        (else (eval (string->symbol (string-append "cc" (number->string (+ (random 29) 1)) "-p"))))))

(define (random-name)
  (cond ((= 0 (random 11)) "Fruitips")
        ((= 0 (random 10)) "Pez")
        ((= 0 (random 9)) "M&Ms")
        ((= 0 (random 8)) "Smarties")
        ((= 0 (random 7)) "Fruitella")
        ((= 0 (random 6)) "Warhead")
        ((= 0 (random 5)) "Fox")
        ((= 0 (random 4)) "Skittles")
        ((= 0 (random 3)) "Hacks")
        ((= 0 (random 2)) "MonCheri")
        (else "Rocher")))

(define (generate-package n)
  (cond ((> n 0) (make&install-package (string->symbol (random-name))
                                       (random-platform)
                                       (random-platform)
                                       (+ 5 (random 10))
                                       (+ 5(random 10)))
                 (generate-package (- n 1)))))

(generate-package 20)

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

(define (play-game-i)
  (display-game-state)
  (apply ask (cons you (tokenize (prompt "Command:"))))
  (if (clock)
      (play-game-i)))

(define (prompt prompt-string)
  (display prompt-string)
  (read-line))

(define (display-game-state)
  (newline)
  (display "Time remaining: ")
  (display (- total-time (current-time)))
  (newline)
  (display "Score: ")
  (display score)
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
  (for-each (lambda (x) (begin (display "To the ") (display x) (display ": ") (display (map (lambda (p) (ask p 'name)) (ask (ask (ask you 'place) 'neighbor-towards x) 'things))) (newline))) (reverse (ask (ask you 'place) 'exits)))
  (newline))

; Uncomment line below to play in interactive mode
;(play-game-interactive)


;; The beginning of an ever-expanding automatic game script
;;----------------------------------------------------------

(define score 0)
(define total-time 100)

(define (play-game-interactive)
  (display "Welcome to CandyRunner 1.1.")
  (newline)
  (newline)
  (display "You are a candy runner whose job is to scavange for candies strewn across Singapore and take them to where they belong. Each candy you deliver will earn you points as well as add some bonus time to your clock.")
  (newline)
  (newline)
  (display "In this beta version, your playing field will be the East-West line. You will start at Buona Vista platform. To your north would be the east-bound track and your south would be the west-bound track. You cannot move onto the tracks unless there is a train there. There are 6 trains servicing the East-West line at any point in time and they start equally spaced out throughout the entire line. At any time, there is a 10% chance that these trains might face technical errors and refuse to move.")
  (newline)
  (newline)
  (display "Your support for this game will see that the North-South line, North-East line and Circle line be added in the near future.")
  (newline)
  (newline)
  (display "Commands: go <direction>, take <candy>, lose <candy>, check <candy>, deliver <candy>, hop-onto <direction>, travel-to <destination>, wait")
  (newline)
  (newline)
  (display "Have fun!")
  (newline)
  (newline)
  (display "Change log: Added hop-onto <direction>    - Use this to get onto the first train that arrives at <direction>")
  (newline)
  (display "            Added travel-to <destination> - Use this to get off at <destination>. Use the 3 or 4 character station code")
  (newline)
  (display "            Amended take <candy> to include checking")
  (newline)
  (display "            Fixed check <candy> grammar error")
  (newline)
  (display "            Amended deliver <candy> display information")
  (newline)
  (display "            Fixed misspelling Hecks to Hacks")
  (newline)
  (newline)
  (play-game-i))

(define (play-game)
  (make&install-package 'Smarties EW21-p EW1-p (+ 5 (random 10)) (+ 5(random 10)))
  (ask you 'look-around)
  (ask you 'take 'Smarties)
  (ask you 'check 'Smarties)
  (ask you 'wait)
  (ask train1 'move-to EW21-e)
  (ask you 'go 'north)
  (ask train1 'move-to EW1-e)
  (ask you 'go 'south)
  (ask you 'deliver 'Smarties)
  )

(play-game-interactive)