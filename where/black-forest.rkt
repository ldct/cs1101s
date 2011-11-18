(define (black-forest)
  (toplevel)
  (display "The trees above you grow denser. You can hardly see anything. Perhaps it would be better if you had a torchlight. You might also want to draw a map...\n
You see well-trodden paths to the North-West and slightly more disused tracks to the North-East.
(NE) Go North-East
(NW) Go North-West
(B) Go back to crash site.
")
  (define msg (read-line))
  (cond ((equal? msg "NE")
         (bf2))
        ((equal? msg "NW")
         (bf1))
        ((equal? msg "B")
         (crash-site))
        (else
         (black-forest)))
  )

(define (bf1)
  (display "You hear a soft rustle. Perhaps it is a wild deer.
There are tracks to the North, the North-East and South-East. The South-East track seems oddly reassuring.
(NE) Go North-East
(NN) Go North
(SE) Go South-East")
  (define msg (read-line))
  (cond ((equal? msg "SE")
         (black-forest))
        ((equal? msg "NN")
         (bf3))
        ((equal? msg "NE")
         (bf4))
        (else
         (bf1))))

(define (bf2)
  (display "bf2.
(SW)
(SE)
(NN)
(EE)")
  (define msg (read-line))
  (cond ((equal? msg "SW")
         (black-forest))
        ((equal? msg "SE")
         (bf5))
        ((equal? msg "NN")
         (bf4))
        ((equal? msg "EE")
         (bf6)
         (else
          (bf2)))))

(define (bf3)
  (display "bf3.
(SS)
(NE)")
  (define msg (read-line))
  (cond ((equal? msg "SS")
         (bf1))
        ((equal? msg "NE")
         (bf8))
        (else
         (bf3))))

(define (bf4)
  (display "bf4.
(SS)
(SW)
(SE)
(NE)
(NW)")
  (define msg (read-line))
  (cond ((equal? msg "SS")
         (bf2))
        ((equal? msg "SW")
         (bf1))
        ((equal? msg "SE")
         (bf6))
        ((equal? msg "NE")
         (bf9))
        ((equal? msg "NW")
         (bf8))
        (else
         (bf4))))

(define (bf5)
  (display "bf5
(NE)
(NW)")
  (define msg (read-line))
  (cond ((equal? msg "NE")
         (bf6))
        ((equal? msg "NW")
         (bf2))

        (else
         (bf5))))

(define (bf6)
  (display "bf6
(SW)
(WW)
(NW)
(NN)")
  (define msg (read-line))
  (cond ((equal? msg "SW")
         (bf5))
        ((equal? msg "WW")
         (bf2))
        ((equal? msg "NW")
         (bf4))
        ((equal? msg "NN")
         (bf10))
        (else
         (bf6))))

(define (bf8)
  (display "bf8
(NE)
(SE)
(SW)
(NW)")
  (define msg (read-line))
  (cond ((equal? msg "NE")
         (bf9))
        ((equal? msg "NW")
         (bf12))
        ((equal? msg "SW")
         (bf3))
        ((equal? msg "SE")
         (bf4))
        (else
         (bf8))))

(define (bf9)
  (display "bf9
(WW)
(SW)
(SSW)
(EE)")
  (define msg (read-line))
  (cond ((equal? msg "WW")
         (bf12))
        ((equal? msg "EE")
         (bf10))
        ((equal? msg "SSW")
         (bf4))
        ((equal? msg "SW")
         (bf8))
        (else
         (bf9))))

(define (bf10)
  (display "bf10
(WW)
(SS)")
  (define msg (read-line))
  (cond ((equal? msg "WW")
         (bf9))
        ((equal? msg "SS")
          (bf6))
        (else
         (bf10))))

(define (bf12)
  (display "bf12
(EE)
(SE)
(WW)")
  (define msg (read-line))
  (cond ((equal? msg "EE")
         (bf9))
        ((equal? msg "SE")
         (bf8))
        ((equal? msg "WW")
         (rebel-base))
        (else
         (bf12))))
           
