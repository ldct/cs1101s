(define (museum)
  (display "You arrive at the Kamino II planatery archives.
(A) Read about the history of Kamino II
(B) Read about the geography of Kamino II
(T) travel
")
  (define msg (read-line))
  (cond ((equal? msg "A")
         (display "Kamino II was first discovered by settlers from Odeon Orbis in 5034, during the height of the expansionist push to colonise the outer Galactic Rim. The settlers were then preparing to push through the vast expanse of the galaxy known as the Kessel sector (sector RA-607), a famously sparse area with little stars and fewer habitable planet. It was for this purpose only that a small colony was founded on Kamino II, and a spaceport was built to service ships that stopped on the planet before making the dangerous journey past the Kessel sector.

For years Kamino II's population was stabalized at roughly 40,000, with about half of the population being born here; the rest were settlers stopping over for temporary periods of time. However, a mere 5 planets of the Kessel sector had been colonised, and a greater number of ships were disappearing for no reason. In 5107, when the population had dwindled to 25,000, the presence of Sith forces on barren wasteland planets in the sector was revealed, and desperate refugees fled from the sector, swelling Kamino II and stretching its resources thin. The settlers bravely fought off waves of Sith attack and many died in the process, especially since the Republic was unable to defend a planet so far from where their military presence lay.  As a result, a small group of rebels who fight for independence from the Republic still exist in the black forests, protected from government forces by the density of the forests.
"))
        ((equal? msg "B")
         (display "Kamino II is the second planet from its sun, a single yellow stage-2 star (IR-532). The temperature range is from 314-326 K, averaging 324 K. The climate changes in mild, almost non-seasonal cycles due to the very small tilt of the axis. Archeological evidence, however, suggests that the planet was once plunged in ice ages with temperatures as low as 250 K, leading to speculation that an extra-planetary collision with a comet had occurred.

Much of the planet is covered by the black forest which, despite its name, consists mainly of temperate life-forms, green in colour. Small areas are cleared but much remains unexplored. These plants can range up to 80m tall in the thickest parts of the forest, blocking out almost all sunlight. The land masses are all connected by a small and narrow land passage, on which the spaceport is located. The location was chosen because it was, ironically, easier to land into the water than anywhere in the forest, and that strip of land also was particularly high and thus less forrested.

There are also two major oceans on Kamino II, Iris and Siri, with Iris covering 24% of the land area and Siri, 34%. There is also a single large mountain range roughly opposite the spaceport, thought to contain Yittrium deposits.
")
         (add-location! '(mountains "Large mountain range."))
         )
        ((equal? msg "T")
         (travel)))
  (museum)
  )