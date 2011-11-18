(define (spaceport)
  (toplevel)
  (display
"You are at the spaceport. You see an inquiries desk.
(A) speak to the inquiries desk.
(T) travel
")
  (define msg (read-line))
  (cond 
    ((equal? msg "A")
     (display "You walk up to a short man in a red suit and cap. His face is curiously featureless and unremarkable.\n")
     (talk-to-inquiries))
    ((equal? msg "T")
     (travel))
    (else
     (display "I don't understand your command.\n")
     (spaceport))))

(define (talk-to-inquiries)
  (define convo (make-conversation '()))
  (define D `("D" 
              "(D) Is this the only spaceport on the planet?"
              ,(lambda ()
                 (display "Why yes it is. Only way in or out.\n")
                 )))
  (define E `("E"
              "(E) I'm looking for someone with a damaged ship who would be wishing to leave the planet."
              ,(lambda () 
                 (display "In that case, our shipyards located near the spaceport, but I really suggest that you report the matter to law enforcement in that case...\n")
                 (add-location! '(shipyard "For reparing ships"))
                 (convo 'sub-topics E)
                 )))
  (define A `("A" 
              "(A) Where exactly am I?" 
              ,(lambda ()
                (display "The federal spaceport of the planet of Kamino II.\n")
                 (convo 'add-topics C)
                 (convo 'add-topics D)
                 )))
  (define C `("C" 
              "(C) I wish to know more about your planet. Where are your archives?"
              ,(lambda ()
                 (display "Our planetary archives and national museum are about a day's journey from here. Here's the address.\n")
                 (add-location! '(museum "Museum and national archives of Kamino II"))
                )))
  (define B `("B" 
              "(B) Do you know where one could repair a spaceship?"
              ,(lambda () 
                 (display "Well, sir, the ship you came is in perfect condition, I assure you.\n")
                 (convo 'add-topics E)
                 (convo 'sub-topics B)
                )))
  
  (define X `("X" 
              "(X) Goodbye."
              ,(lambda () 
                 (display "Good day.\n")
                 (spaceport))))

  (convo 'add-topics A B X)
  (convo 'repl))