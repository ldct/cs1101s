;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file engine-17a.rkt
;;;;  Used in Chapter 6 M17
;;;;  Loads places and changes settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module engine-17a racket
  (require "engine.rkt")
  (require "places-17a.rkt")
  (provide (except-out (all-from-out "engine.rkt")
                       act-method-limits random-room-nums print-errors
                       spawn-drone-on-bot-death spawn-drone-on-card-pick
                       min-bot-inertia max-bot-inertia alarm-cycle))
  (layout-bitmap layout))