;
; CS1101S --- Programming Methodology
;
; Sidequest 22.1
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "oop.rkt")
(load "streams.rkt")

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define (make-window name capacity)
  (let ((items '()))
    (define (add! self x)
      (set! items (append items (list x)))
      (if (> (length items) capacity)
          (set! items (cdr items))))
    (define (size self)
      (length items))
    (define (get-window self)
      items)
    (define (dispatch op)
      (cond ((equal? op 'size)
             size)
            ((equal? op 'get-window)
             get-window)
            ((equal? op 'add)
             add!)
            (else
             "Error!")))
    dispatch))

(define w (make-window 'w 4))
(ask w 'size)
(ask w 'get-window)
(ask w 'add 1)
(ask w 'size)
(ask w 'get-window)
(ask w 'add 'a)
(ask w 'add 3)
(ask w 'add "a")
(ask w 'add 4)
(ask w 'size)
(ask w 'get-window)

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define (make-filtering-window name capacity . opt-filter)
  (let ((window (make-window name capacity))
        (e-filter (if (null? opt-filter) (lambda (x) #t) (car opt-filter))))
    (define (set-filter! self new-filter)
      (set! e-filter new-filter))
    (define (add! self x)
      (if (e-filter x)
          (ask window 'add x)))
    (define (dispatch op)
      (cond ((equal? op 'set-filter)
             set-filter!)
            ((equal? op 'add)
             add!)
            ((member op '(size get-window add))
             (get-method window op))))
    dispatch))


(define w (make-filtering-window 'w 4))
(ask w 'size)
(ask w 'get-window)
(ask w 'add 1)
(ask w 'size)
(ask w 'get-window)
(ask w 'add 'a)
(ask w 'add 3)
(ask w 'add "a")
(ask w 'add 4)
(ask w 'size)
(ask w 'get-window)
(ask w 'set-filter (lambda (x) #f))
(ask w 'add 4)
(ask w 'get-window)

;;;;;;;;;;
; Task 3 ;
;;;;;;;;;;

; sample force stream given in the question
(define force-stream (list->stream '(#f 1 0 1 0 #f 1 0 1 0 #f #f 1 1 0 1 1 #f #f #f 1 #f 0 1 1 1 0 1 )))

(define (same-item? a b)
  (if (and (pair? a) (pair? b))
      (same-list? a b)
      (equal? a b)))

(define (same-list? a b)
  (if (null? a)
      (null? b)
      (and (not (null? b))
           (same-item? (car a) (car b))
           (same-list? (cdr a) (cdr b)))))

(define (scan force-stream signature distance)
  (define w (make-filtering-window 'a (length signature) values))
  (define (dist lst)
    (if (null? lst)
        -inf.0
        (begin (ask w 'add (car lst))
               (if (same-item? signature (ask w 'get-window))
                   1
                   (+ 1 (dist (cdr lst)))))))
  (define ans (dist (filter values (eval-stream force-stream distance))))
  (if (= ans -inf.0)
      #f
      ans))

(scan force-stream '(1 0 1 0 1) 200)
(scan force-stream '(0 1 0 1 0) 200)
(scan force-stream '(0 1 1 1 0) 200)
(scan force-stream '(0 1 1 1 0) 10)   