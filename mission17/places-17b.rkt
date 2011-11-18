;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file places.rkt
;;;;  Defines the places in the game world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;
;;  Code for adventure game
;; 
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Here we define the places in our world...

;;------------------------------------------

; 1: protected, 2: link west, 4: link north, 8: link down, 16: generator, 32: start, 64: bot
(module places-17 racket
  (provide layout)
  (define layout
    '(; L1
      ((0  2  2  98)
       (4  6  6  38)
       (4  70 6  38)
       (4  6  70 38))
      ; L2
      ((0  2  2  10)
       (4  4  68 4)
       (4  21 4  4)
       (4  6  6  6))
      ; L3
      ((0  2  2  2)
       (4  2  2  6)
       (68 2  2  14)
       (4  2  2  6))
      ; L4
      ((0  2  2  66)
       (4  70 6  10)
       (4  6  70 2)
       (36 38 38 38)))))