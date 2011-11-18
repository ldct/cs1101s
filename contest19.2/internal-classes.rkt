;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 6 Internal Classes ;;;
;;;         Mission 19         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module internal-classes scheme/gui
  
  ; export
  (provide dc-path? split-list
           (struct-out room) (struct-out obj)
           (rename-out [api-register-place register-place]
                       [api-register-object register-object])
           human-path bot-path shift-obj! add-to-draw-queue
           add-to-tick-list rm-size num-rms rm-space
           default-colour full-redraw full-redraw-z rm-z atk-z
           wall-width wall-blocked-width wall-protected-colour
           run-game display-intermediate line-proc
           popup-message stop-game identity λ keyword-apply
           board-update)
  
  (define (identity x) x)
  (define split-list (procedure-rename (compose cons partition) 'split-list))
  
  ; constants
  (define vp-width 120)
  (define vp-height 120)
  (define quit-x 175)
  (define quit-y 100)
  
  (define scale-factor 7)
  
  (define sboard-width (* scale-factor 80))
  (define sboard-height (* scale-factor 50))
  (define sboard-font (make-object font% (floor (* 2 scale-factor)) 'modern))
  
  (define x-offset 5)
  (define y-offset 5)
  (define update-interval 50)
  
  (define icon-rad 1/2)
  (define arrow-len 1)
  
  (define rm-size 10)
  (define num-rms 4)
  
  (define full-redraw 'FULL-REDRAW)
  (define full-redraw-z -inf.0)
  (define rm-z 1)
  (define atk-z 5)
  
  (define MAX-NAME-LEN 3)
  
  (define offsets
    '((5 1) (2 4) (8 4) (3 7) (7 7)))
  
  (define rm-space (length offsets))
  
  (define arrow-offsets '((85/10 5/10) (5/10 85/10))) ; up, down
  
  (define default-colour "BLACK")
  
  (define-values (shadow-z shadow-dur shadow-colour shadow-width shadow-style)
    (values 4 1 "GRAY" 1 'solid))
  
  (define-values (expl-colour expl-rad-ratio)
    (values "OrangeRed" 10))
  
  (define-values (wall-width wall-blocked-width wall-protected-colour)
    (values 0 0.5 "Red"))
  
  (define btn-running-lbl "Suspend Mission")
  (define btn-paused-lbl "Resume Mission")
  (define btn-step-lbl "Next Action")
  
  ; globals
  (define rooms '())
  
  (define draw-queue '())
  
  (define objects '())
  
  ; internal variables
  (define timer-on #f)
  (define timer-paused #t)
  
  ; general functions
  (define-syntax push!
    (syntax-rules ()
      ((_ q) (void))
      ((_ q x y ...)
       (begin
         (set! q (cons x q))
         (push! q y ...)))))
  
  (define-syntax push-unique!
    (syntax-rules ()
      ((_ q) (void))
      ((_ q x y ...)
       (begin
         (unless (memq x q)
           (push! q x))
         (push-unique! q y ...)))))
  
  (define-syntax pop!
    (syntax-rules ()
      ((_ q)
       (let ((r (car q)))
         (set! q (cdr q))
         r))))
  
  ; predicates
  (define dc-path?
    (is-a?/c dc-path%))
  
  ; structs
  (define (make-guard-proc checklst)
    (λ arglst
      (let-values (((argvals type-name)
                    (split-at-right arglst 1)))
        (let check ((n 0)
                    (args argvals)
                    (checks checklst))
          (unless (or (null? args)
                      (null? checks))
            (let ((arg (car args))
                  (check (car checks)))
              (unless ((cadr check) arg)
                (apply raise-type-error (car type-name) (car check)
                       n
                       argvals)))
            (check (+ n 1)
                   (cdr args)
                   (cdr checks))))
        (apply values argvals))))
  
  (define-struct base (proc)
    #:guard
    (λ (proc type-name)
      (unless (procedure? proc)
        (error type-name
               "bad proc: ~e"
               proc))
      proc)
    #:property prop:procedure
    (struct-field-index proc))
  
  
  (define-struct (room base) (x y width height [objs #:mutable #:auto] [slots #:mutable #:auto]
                                [wall-widths #:mutable #:auto] ; top, right, bottom, left
                                [wall-colours #:mutable #:auto] ; top, right, bottom, left
                                [arrows #:mutable #:auto] ; up, down
                                [arrow-colours #:mutable #:auto] ; up, down
                                [expl-dur #:mutable #:auto] [expl-col #:mutable #:auto])
    #:auto-value '()
    #:guard
    (let ((non-negative-num?
           (λ (x)
             (and (number? x)
                  (>= x 0))))
          (positive-num?
           (λ (x)
             (and (number? x)
                  (> x 0)))))
      (make-guard-proc
       `(("procedure" ,procedure?)
         ("nonnegative-number" ,non-negative-num?)
         ("nonnegative-number" ,non-negative-num?)
         ("positive-number" ,positive-num?)
         ("positive-number" ,positive-num?)))))
  
  (define-struct (obj base) (path [name #:mutable] [room #:mutable] [x #:mutable #:auto] [y #:mutable #:auto] [stats #:mutable #:auto] [moved #:mutable #:auto])
    #:guard
    (make-guard-proc
     `(("procedure" ,procedure?)
       ("dc-path%" ,dc-path?)
       ("string" ,string?)
       ("room" ,(λ(x)
                  (or (room? x)
                      (eq? x #f)))))))
  
  ; parameters
  (define line-proc (make-parameter #f))
  
  ; graphics
  (define f (new frame%
                 [label "Main drawing"]
                 [style '(no-resize-border no-system-menu)]))
  (define f2 (new frame%
                  [label "Controls"]
                  [style '(no-resize-border no-system-menu)]
                  [parent f]
                  [width 250]))
  (define f3 (new frame%
                  [label "Scoreboard"]
                  [style '(no-resize-border no-system-menu)]
                  [parent f]
                  [width sboard-width]
                  [height sboard-height]))
  
  (define (run-game [auto #t])
    (set! timer-on
          (case auto
            ((#f stepped manual)
             #f)
            (else #t)))
    (set! timer-paused (not timer-on))
    (send f show #t)
    (send f2 show #t)
    (send f3 show #t)
    (send controlbtn set-label
          (if timer-on
              btn-running-lbl
              btn-step-lbl))
    (when timer-on
      (send controlbtn set-label btn-running-lbl)
      (send timer start update-interval)))
  (define (stop-game)
    (progress #t #t)
    (set! timer-paused #t)
    (send timer stop)
    (send controlbtn enable #f))
       
  
  (define (draw-stage c dc)
    (send dc set-background (make-object color% 220 200 255))
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-origin x-offset y-offset)
    (send dc set-scale scale-factor scale-factor)
    
    (send dc set-pen default-colour 0 'solid)
    (set! draw-queue (append draw-queue (map (λ(rm)
                                               `(,rm-z 1 ,rm)) rooms)))
    (progress))
  
  (define (display-intermediate)
    (progress #t))
  
  (define (progress [intermediate #f] [last #f])
    (send dc set-pen default-colour 0 'solid)
    (set! draw-queue
          (let ((new-queue 
                 (sort
                  (if intermediate
                      draw-queue
                      (foldr
                       (λ (x y)
                         (let ((z-i (car x))
                               (dur (cadr x))
                               (obj (caddr x))
                               (nxt (cdddr x)))
                           (if (<= dur 0)
                               (append nxt y)
                               (cons `(,z-i ,(- dur 1) ,obj ,@nxt) y))))
                       '() draw-queue))
                      (λ(a b)
                        (< (car a)
                           (car b))))))
            (cond ((and (not intermediate)
                        (memf (λ (i)
                                (eq? (caddr i) full-redraw))
                              new-queue))
                        => (λ (lst)
                             (send dc clear)
                             (append (map (λ(rm)
                                            `(,rm-z ,(cadar lst) ,rm))
                                          rooms)
                                     (cond ((memf (λ (i)
                                                    (> (car i) rm-z))
                                                  lst) => identity)
                                           (else '())))))
                  (else new-queue))))
                                                                   
    (let ((cur-pen (send dc get-pen)))
      (let redraw ((q draw-queue))
        (unless (null? q)
          (let ((d-obj (caddar q)))
            (cond ((room? d-obj)
                   (let ((widths (room-wall-widths d-obj)))
                     (send dc set-pen (send the-pen-list find-or-create-pen default-colour 0
                                            (if (null? widths)
                                                'solid
                                                'transparent)))
                     (let ((x (room-x d-obj))
                           (y (room-y d-obj))
                           (wd (room-width d-obj))
                           (ht (room-height d-obj)))
                       (send dc draw-rectangle x y wd ht)
                       (unless (null? widths)
                         (let* ((c (room-wall-colours d-obj))
                                (c (cond ((and (not (null? c)) c) => identity)
                                         (else (build-list 4 (const default-colour))))))
                           (send dc set-pen (send the-pen-list find-or-create-pen (car c) (car widths) 'solid))
                           (send dc draw-line x y (+ x wd) y)
                           (send dc set-pen (send the-pen-list find-or-create-pen (cadr c) (cadr widths) 'solid))
                           (send dc draw-line (+ x wd) y (+ x wd) (+ y ht))
                           (send dc set-pen (send the-pen-list find-or-create-pen (caddr c) (caddr widths) 'solid))
                           (send dc draw-line (+ x wd) (+ y ht) x (+ y ht))
                           (send dc set-pen (send the-pen-list find-or-create-pen (cadddr c) (cadddr widths) 'solid))
                           (send dc draw-line x (+ y ht) x y)))
                       (let ((arrows (room-arrows d-obj))
                             (colours (room-arrow-colours d-obj)))
                         (let a-iter ((a arrows)
                                      (c (cond ((and (not (null? colours)) colours) => identity)
                                               (else (build-list 2 default-colour))))
                                      (offsets arrow-offsets)
                                      (paths (list arrow-up-path arrow-down-path)))
                           (unless (null? a)
                             (when (car a)
                               (send dc set-pen (send the-pen-list find-or-create-pen (car c) 0 'solid))
                               (send dc draw-path (car paths) (+ x (caar offsets)) (+ y (cadar offsets))))
                             (a-iter (cdr a) (cdr c) (cdr offsets) (cdr paths)))))))
                   (send dc set-pen cur-pen)
                   (let ((occupants (room-objs d-obj)))
                     (let ((e-dur (room-expl-dur d-obj))
                           (e-col (room-expl-col d-obj)))
                       (let ((expl-colour (cond ((or (and (is-a? e-col color%)
                                                          e-col)
                                                     (and (string? e-col)
                                                          (send the-color-database find-color e-col)))
                                                => identity)
                                               (else expl-colour))))
                         (when (and e-dur
                                    (not (null? e-dur)))
                           (when (positive? e-dur)
                             (let ((cur-brush (send dc get-brush)))
                               (send dc set-pen (send the-pen-list find-or-create-pen expl-colour 0 'solid))
                               (send dc set-brush (send the-brush-list find-or-create-brush expl-colour 'solid))
                               (let ((radius (* (/ e-dur expl-rad-ratio) (min (room-width d-obj)
                                                                              (room-height d-obj)))))
                                 (send dc draw-ellipse
                                       (+ (room-x d-obj)
                                          (/ (- (room-width d-obj) radius) 2))
                                       (+ (room-y d-obj)
                                          (/ (- (room-height d-obj) radius) 2))
                                       radius
                                       radius))
                               (send dc set-pen cur-pen)
                               (send dc set-brush cur-brush)))
                           (set-room-expl-dur! d-obj (if (<= e-dur 1) #f
                                                         (- e-dur 1))))))
                     (let draw-occupants ((occupants occupants))
                         (unless (null? occupants)
                           (let ((_occ (car occupants)))
                             (let* ((_x (- (obj-x _occ)
                                           icon-rad))
                                    (_y (- (obj-y _occ)
                                            icon-rad)))
                               (send dc draw-path (obj-path _occ) _x _y)
                               (let ((_name (obj-name _occ)))
                                 (unless (eq? _name "")
                                   (send dc draw-text
                                         (if (> (string-length _name) MAX-NAME-LEN)
                                             (substring _name 0 MAX-NAME-LEN)
                                             _name)
                                         (- _x (* 5/2 icon-rad)) (+ _y (* 2 icon-rad)))))
                               (draw-occupants (cdr occupants))))))))
                  (else
                   (if (cdr d-obj)
                       (send dc set-pen (cdr d-obj))
                       (send dc set-pen cur-pen))
                   (send dc draw-path (car d-obj)))))
          (redraw (cdr q)))))
    (if intermediate
        (when (and timer-on
                   (not last))
          (sleep (/ update-interval 1000)))
        (get-updates)))
  
  (define (get-updates)
    (for-each
     (λ(x)(x)) tick-list))
  
  (define (btncallback btn evt)
    (cond ((not timer-on)
           (progress))
          (timer-paused
           (set! timer-paused #f)
           (send controlbtn set-label btn-running-lbl)
           (send timer start update-interval))
          (else
           (set! timer-paused #t)
           (send controlbtn set-label btn-paused-lbl)
           (send timer stop))))
  
  (define controlbtn (new button%
                          [label (make-string 10 #\W)]
                          [parent f2]
                          [callback btncallback]))
  (define exitbtn (new button%
                       [label "Abort Mission"]
                       [parent f2]
                       [callback (λ (btn evt)
                                   (exit))]))
  
  (define boardtext (new text-field% ; NOTE: Update for Contest
                         [label #f]
                         [parent f3]
                         [enabled #f]
                         [min-width sboard-width]
                         [min-height sboard-height]
                         [font sboard-font]))
  
  (define (board-update datum)
    (send boardtext set-value
          (let iter ((d datum))
            (if (null? d) ""
                (string-append (format "~a\n" (car d)) (iter (cdr d)))))))
  
  (define (popup-message title message [style '(ok)])
    (message-box title message f style))
  
  (define (shift-obj! o new-rm)
    (let ((new-rm (if (room? new-rm) new-rm
                      #f))
          (x (obj-x o))
          (y (obj-y o)))
      (for-each (λ (x)
                  (push-unique! draw-queue `(,rm-z 1 ,x)))
                (filter room? `(,(obj-room o)
                                ,new-rm)))
      (let ((old-room (obj-room o)))
        (when old-room
          (set-room-objs! old-room (remq o (room-objs old-room)))
          (when (and x y)
            (set-room-slots! old-room (remove `(,(- x
                                                    (room-x old-room))
                                                ,(- y
                                                    (room-y old-room)))
                                              (room-slots old-room))))))
      (if new-rm
          (let ((pos (car (remove* (room-slots new-rm) offsets))))
            (let ((_x (+ (room-x new-rm)
                             (car pos)))
                  (_y (+ (room-y new-rm)
                             (cadr pos))))
            (set-room-objs! new-rm (cons o (room-objs new-rm)))
            (set-room-slots! new-rm (cons pos (room-slots new-rm)))
            (set-obj-x! o _x)
            (set-obj-y! o _y)
            (when (and x y (line-proc))
              ((line-proc) x y _x _y shadow-z shadow-dur shadow-colour shadow-width shadow-style))))
          (begin
            (set-obj-x! o #f)
            (set-obj-y! o #f)))
      (set-obj-room! o new-rm)))
  
  (define c
    (new canvas%
         [parent f]
         [paint-callback draw-stage]))
  (send c min-client-width (* vp-width scale-factor))
  (send c min-client-height (* vp-height scale-factor))
  
  (define dc (send c get-dc))
  
  (define timer (new timer%
                     [notify-callback progress]))
  
  (define human-path
    (let ((p (new dc-path%)))
      (send p ellipse 0 0 (* 2 icon-rad) (* 2 icon-rad))
      p))
  
  (define bot-path
    (let ((p (new dc-path%)))
      (send p rectangle 0 0 (* 2 icon-rad) (* 2 icon-rad))
      p))
  
  (define arrow-up-path
    (let ((p (new dc-path%)))
      (send p move-to 0 arrow-len)
      (send p line-to arrow-len 0)
      (send p line-to (/ arrow-len 2) 0)
      (send p line-to arrow-len (/ arrow-len 2))
      (send p line-to arrow-len 0)
      (send p close)
      p))
  
  (define arrow-down-path
    (let ((p (new dc-path%)))
      (send p move-to 0 arrow-len)
      (send p line-to (/ arrow-len 2) arrow-len)
      (send p line-to 0 (/ arrow-len 2))
      (send p line-to 0 arrow-len)
      (send p line-to arrow-len 0)
      (send p close)
      p))
  
  (define d-font
    (send the-font-list find-or-create-font 1 "Courier New" 'default 'normal 'normal))
  (send dc set-font d-font)
  
  (define (add-to-draw-queue z-i dur item . nxt)
    (push! draw-queue `(,z-i ,dur ,item ,@nxt)))
  
  ; functions
  (define (register-place new-room)
    (push! rooms new-room)
    (push! draw-queue `(,rm-z 1 ,new-room))
    new-room)
  
  (define (register-object new-object the-room)
    (push! objects new-object)
    (shift-obj! new-object the-room)
    new-object)
  
  ; api
  
  (define-syntax api-register-place
    (syntax-rules ()
      ((_ place-proc x y width height)
       (let ((new-room
              (cond ((findf (λ(rm)
                              (eq? (base-proc rm)
                                   (if (room? place-proc)
                                       (base-proc place-proc)
                                       place-proc))) rooms) => identity)
                    (else (register-place (make-room place-proc x y width height))))))
         (set! place-proc new-room)
         new-room))))
  
  (define-syntax api-register-object
    (syntax-rules ()
      ((_ proc path name room)
       (let ((object 
              (cond ((findf (λ(obj)
                              (eq? (base-proc obj)
                                   (if (obj? proc)
                                       (base-proc proc)
                                       proc))) objects) => identity)
                    (else (register-object (make-obj proc path name #f) room)))))
         (set! proc object)
         object))))
  
  ; tick list
  (define tick-list '())
  (define (add-to-tick-list proc)
    (when (procedure? proc)
      (push! tick-list proc))))