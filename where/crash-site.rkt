(load "classes.rkt")
(load "black-forest.rkt")

(define hut-guy-state 0) ;alive/hostile/killed by you/killed by darth
;TODO: darth's book

(define (crash-site)
  (toplevel)
  (display "You are in a lush jungle region and found the thin hull of a spaceship. There is only scrap metal remaining. In this relatively deserted place, your JFDI force senses two paths that have recently been used by living creatures. One reeks strongly of the dark forces.
(A) follow the path that Darth used
(B) follow the other path
(T) travel
")
  (define msg (read-line))
  (cond
    ((equal? msg "A")
     (black-forest))
    ((equal? msg "B")
     (crash-site-hut))
    ((equal? msg "T")
     (travel))
    (else
     (display "I don't understand you.")
     (crash-site))))

(define (crash-site-hut)
  (display "You are standing before a small straw hut.
(A) Knock on the door
(B) Go back\n")
  (define msg (read-line))
  (cond
    ((equal? msg "A")
     (cond ((eq? hut-guy-state 0)
            (display "The door opens and a tall, thin man with narrow eyes comes out.\n")
            (talk-crash-site-hut))
           ((eq? hut-guy-state 1)
            (display "The door opens a bit and immediately shuts.\n")
            (fight-crash-site-hut))
           ((eq? hut-guy-state 2)
            (display "The hut was as you had left it. You silently pray for the man who died because of your recklessness.\n"))
           ((eq? hut-guy-state 3)
            (display "The place has been left untouched, but you sense that the occupant did not leave on his own will. You should go back.\n")))
     (crash-site-hut))
    ((equal? msg "B")
     (display "You walk back to the crash site, more puzzled than ever.\n")
     (crash-site))
    (else
     (display "I don't understand you.")
     (crash-site))))

(define (talk-crash-site-hut)
  (define convo (make-conversation '()))
  (define Y `("Y" 
              "(Y) I'm from this planet, if that's what you mean."
              ,(lambda ()
                (display "Well, then there's something you should know about us. We don't deal with liars.
He swings the door shut\n")
                 (set! hut-guy-state 1)
                 (crash-site-hut))))
  (define N `("N" 
              "(N) No, you're right. I'm not."
              ,(lambda ()
                 (display "You're mighty suspicious. Anyway, suppose I did see a ship crash, and suppose you wanted to buy it, I only accept payment in the form of yittrium ore. Come back when you have some.\n")
                 (set! hut-guy-state 1)
                 (crash-site-hut)
                 )))
  (define A `("A" 
              "(A) Have you seen a ship crash into here?" 
              ,(lambda () 
                 (display "You're not from around, aren't you?")
                 (convo 'clear-topics)
                 (convo 'add-topics Y)
                 (convo 'add-topics N)
                 )))
  (define B `("B" 
              "(B) Have you seen a guy dressed in full black?"
              ,(lambda () 
                 (display "Hell no.")
                 (convo 'sub-topics B)
                 )))
  (define X '("X" 
              "(X) Goodbye."
              ,(lambda () 
                 (display "Hmph.\n")
                 (set! hut-guy-state 1)
                 (crash-site-hut))))
  (convo 'add-topics A B X)
  (convo 'repl))

(define (fight-crash-site-hut)
  (define convo (make-conversation '()))
  (define K `("K" 
              "(K) Kill the man before the door closes"
              ,(lambda () 
                (display "With a swing of your lightsaber the man drops dead. You enter the hut but find nothing to suggest he was anything but a small-time smuggler.\n")
                (set! hut-guy-state 2)
                (crash-site-hut)
                )))
  (define X `("X" 
              "(X) Do nothing."
              ,(lambda () 
                (display "You stand before the door, wondering if you should not have done something.\n")
                (crash-site-hut))))
  (convo 'add-topics K X)
  (convo 'repl))