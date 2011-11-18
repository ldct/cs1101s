(load "classes.rkt")
(load "spaceport.rkt")
(load "museum.rkt")
(load "shipyard.rkt")
(load "crash-site.rkt")
(load "black-forest.rkt")
(load "mountains.rkt")
(load "construction-site.rkt")

(define DAY 0)
(define KNOWN-LOCATIONS '())
(define HEALTH 100)
(define ROUSE 0)
(define CRON '())

(define (add-cron! tag day func)
  (append! CRON (list tag day func)))

(define (advance-day! n) (set! DAY (+ DAY n)))

(define (toplevel)
  (display "It is now day ")
  (display DAY)
  (display ". Your health is ")
  (display HEALTH)
  (display "/100.\n")
  (define up (filter (lambda (x) (<= (cadr x) DAY))
                     CRON))
  (map (lambda (x) ((caddr x))) up)
  (set! CRON (filter (lambda (x) (> (cadr x) DAY)) CRON))
  )

(define (add-location! loc)
  (display "the ")
  (display (car loc))
  (display " has been added to the list of places you can travel to!\n")
  (append! KNOWN-LOCATIONS loc))

(define (travel)
  (display "Where do you want to travel to?\n")
  (map (lambda (x) 
         (display (car x))
         (display ": ")
         (display (cadr x))
         (display "\n"))
       KNOWN-LOCATIONS)
  (advance-day! 1)
  (define msg (read-line))
  ((eval (string->symbol msg))))

(define (start)
  (display "Hello JFDI Warrior. You have been tasked with tracking down the locations of Darth Ben, whoose ship was last seen crashing onto this planet. We have given you the coordinates of his crash site.

Commands that you can type will be listed in brackets: for example 

(T) travel
 
means that you should type 'T' to brind up the travel menu. When travelling, destinations are given like this:

spaceport: Wyl spaceport

you should type 'spaceport' to travel there. Note that travelling consumes one day of your time. Travelling to your current location simply makes you idle for one day.

Remember to check back on locations you've already visited - things constantly change.

We do not know what Darth Ben is planning to do on this planet. Act fast to find out, but remember that the JFDI forces are not officially allowed to be on this planet. Do not alert the inhabitants unnecessarily.

We'll be dropping you off at Wyl spaceport - from there on, you are alone.

May the force be with you.
\n")
  (add-location! '(spaceport  "Spaceport. Where spaceships come and go."))
  (add-location! '(crash-site "The place Darth Ben crashed his ship"))
  (add-cron! 'murder 5 
             (lambda () 
               (display "A mysterious death due to unknown causes has occurred on this peaceful planet. You should investigate.\n")
               (add-location! '(construction-site "Construction site - mysterious death was reported here"))))
  (display "\n")
  (spaceport))

(start)