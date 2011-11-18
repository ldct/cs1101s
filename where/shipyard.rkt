(define hostility 0)
(define shipyard-state 0) ;nothing/darth visited

(define (shipyard)
  (display "You arrive at the shipyard. There are engineers walking around as well as hangars to the east.
(A) talk to an engineer
(B) sneak into hangars
(T) travel
")
  (define msg (read-line))
  (cond ((equal? msg "A")
         (talk-hangar-guy))
        ((equal? msg "B")
         (if (= hostility 1)
             (hostile-hangar)
             (hangar)))
        ((equal? msg "T")
         (travel)))
  (museum)
  )

(define (hangar)
  (define convo (make-conversation '()))
  (display "Hey! What do you think you're doing?\n")
  (define A `("A"
              "(A) Bribe the engineer with some money ($100)"
              ,(lambda ()
                 (display "Trying to bribe me? Guards, throw him out!
You are thrown out rather unceremoniously to the shipyard.
")
                 (set! hostility 1)
                 (shipyard)
                 )))
  (define B `("B" 
              "(B) Bribe the engineer with some money ($200)"
              ,(lambda ()
                 (display "Well, don't do it again.\n")
                 (shipyard)
                 )))
  (define K `("K"
              "(K) Kill the engineer"
              ,(lambda () 
                 (display "You silently kill him and escape to the shipyard before anyone notices.\n")
                 (shipyard)
                 )))
  (convo 'add-topics A B K)
  (convo 'repl)
  )

(define (hostile-hangar)
  (define convo (make-conversation '()))
  (display "You again!\n")
  (define K `("K"
              "(K) Kill the engineer"
              ,(lambda () 
                 (display "You silently kill him and escape to the shipyard before anyone notices.\n")
                 )))
  (define A `("A"
              "(A) Accept your fate"
              ,(lambda () 
                 (display "You are thrown out. Quite sad for a JFDI knight, really.\n")
                 )))
  (convo 'add-topics A K)
  (convo 'repl)
)

(define (talk-hangar-guy)
  (define convo (make-conversation '()))
  (display "Looking to repair a ship?\n")
  (define R `("R"
              "(R) Bribe the engineer with some money ($200))"
              ,(lambda ()
                 (display "Well, since you're so, uh, sincere, I'll tell you."))))
  (define A `("A" 
              "(A) Has anyone tried to repair ships here recently?"
              ,(lambda ()
                 (display "Sorry, we don't give out that kind of information.\n")
                 (convo 'add-topics R)
                 (convo 'sub-topics A)
                 )))
  (define B `("B"
              "(B) What would it cost to repair a ship here?"
              ,(lambda () 
                 (display "Most repairs cost less than $20,000\n.")
                 (convo 'sub-topics B)
                 )))
  (define X `("X" 
              "(X) Goodbye."
              ,(lambda () 
                 (display "Nice doing business with you.\n")
                 (shipyard))))

  (convo 'add-topics A B X)
  (convo 'repl)
)