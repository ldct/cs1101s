;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file oop-lib.rkt
;;;;  Used in the Contest
;;;;  Contains utility and clock functions for the OOP game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module oop-lib racket
  (require "internal-classes.rkt")
  (require racket/sandbox)
  (provide (except-out (all-defined-out)
                       *sort-clock-list* *cur-clock-list*
                       *sort-move-index* *sort-target-index*
                       *end-game-conds*)
           (except-out (all-from-out "internal-classes.rkt")
                       run-game stop-game)
           (rename-out [run-game start-mission]
                       [stop-game end-mission])
           make-hash hash-ref hash-map hash-update!)
  
  (define moved-message 'person-has-moved!)
  (define not-activated 'person-not-active!)
  
  (define act-method-limits (make-parameter '(#f #f))) ; secs, mb
  
  ; OOP system
  
  ;;; Simple object system with inheritance
  
  (define ask
    (make-keyword-procedure
     (λ (kws kw-args object message . args)
       (let ((method (get-method object message)))
         (if (method? method)
             (keyword-apply method
                            kws
                            kw-args
                            object args)
             (error "No method" message (cadr method)))))))
  
  (define (get-method object message)
    (object message))
  
  (define (no-method name)
    `(no-method ,name))
  
  (define (method? x)
    (not (no-method? x)))
  
  (define (no-method? x)
    (if (pair? x)      
        (eq? (car x) 'no-method)
        (not (procedure? x))))
  
  
  ;;; -------------------------------------------------------------------
  
  ;;; symbol-append is available in MIT-GNU Scheme but not in DrScheme
  (define (symbol-append . sym-lst)
    (string->symbol (apply string-append
                           (map (λ(x)
                                  (if (number? x)
                                      (number->string x)
                                      (symbol->string x))) sym-lst))))
  
  (define (list-names things)
    (foldr (λ (a b)
             (if (null? b)
                 (list (ask a 'name))
                 (cons (symbol-append (ask a 'name) '\,) b)))
           '() things))
  
  ;;; --------------------------------------------------------------------------
  
  ;;; Recording
  (define process-killrec (make-parameter #f))
  (define print-errors (make-parameter #f))
  
  ;;; --------------------------------------------------------------------------
  
  ;;; Clock routines
  (define *clock-list* '())
  (define *sort-clock-list* '())
  (define *cur-clock-list* '())
  (define *end-game-conds* '())
  (define *the-time* 0)
  
  (define *sort-move-index* 1)
  (define *sort-target-index* 1) ; previously 5 for luck, now both level
  
  (define (sort-on idx [comp <=])
    (λ (a b)
      (and (obj-stats a)
           (or (not (obj-stats b))
               (comp (random (list-ref (obj-stats a) idx))
                     (random (list-ref (obj-stats b) idx)))))))
  
  (define (initialize-clock-list)
    (set! *clock-list* '())
    (set! *sort-clock-list* '()))
  
  (define (add-to-clock-list person)
    (set! *clock-list* (cons person *clock-list*))
    (set! *sort-clock-list* '()))
  
  (define (remove-from-clock-list person)
    (set! *clock-list* (remq* (list person) *clock-list*))
    (set! *sort-clock-list* (remq* (list person) *sort-clock-list*))
    (set! *cur-clock-list* (remq* (list person) *cur-clock-list*)))
  
  (define (add-end-game-cond pred proc)
    (set! *end-game-conds* (cons (cons pred proc) *end-game-conds*)))
  
  (define (clock) 
    (unless (null? *clock-list*)
      (when (null? *cur-clock-list*)
        (when (null? *sort-clock-list*)
          (set! *sort-clock-list*
                (sort *clock-list*
                      (sort-on *sort-move-index* >=))))
        (set! *the-time* (+ *the-time* 1))
        (set! *cur-clock-list* *sort-clock-list*)
        (newline)
        (display "---Tick---"))
      (let ((limits (act-method-limits)))
        (call-with-limits
         (car limits) (cadr limits)
         (λ ()
           (with-handlers ((continuation?
                            (λ (c)
                              (display-intermediate)
                              (c)))
                           ((or/c not-activated (list/c moved-message continuation?)) void)
                           ((list/c procedure? procedure? continuation?)
                            (λ (killrec)
                              (when (process-killrec)
                                ((process-killrec) (car killrec) (cadr killrec)))
                              ((caddr killrec))))
                           ((λ (e) (and (print-errors) (exn:fail? e))) (compose displayln exn-message)))
             (let ((cur (car *cur-clock-list*)))
               (set-obj-moved! cur #f)
               
               (ask (car *cur-clock-list*) 'act))))))
      (unless (check-end-game)
        (set! *cur-clock-list* (cdr *cur-clock-list*)))))
  
  (define (current-time)
    *the-time*)
  
  (define (run-clock n)
    (let iter ((n (* n (length *clock-list*))))
      (unless (zero? n)
        (clock)
        (iter (- n 1)))))
  
  (define (check-end-game)
    (let iter ((lst *end-game-conds*))
      (cond ((null? lst)
             #f)
            ((let ((condition (car lst)))
               (let ((check-results (call-with-values (car condition) list)))
                 (if (car check-results)
                     (begin
                       (apply (cdr condition) (cdr check-results))
                       #t)
                     #f))))
            (else
             (iter (cdr lst))))))
  ;;; --------------------------------------------------------------------------
  
  ;;; Miscellaneous procedures
  (define (is-a object property)
    (let ((method (get-method object property)))
      (if (method? method)
          (ask object property)
          #f)))
  
  (define (change-place mobile-object new-place)
    (when (is-a mobile-object 'person?)
      (let/cc c
        (when (obj-moved mobile-object)
          (raise (list moved-message c) #f))
        (raise c #f))
      (shift-obj! mobile-object new-place)
      (set-obj-moved! mobile-object #t))
    (let ((old-place (ask mobile-object 'place)))
      (ask mobile-object 'set-place new-place)
      (ask old-place 'del-thing mobile-object))
    (ask new-place 'add-thing mobile-object))
  
  (define (other-people-at-place person place)
    (filter (λ (object)
              (if (not (eq? object person))
                  (is-a object 'person?)
                  #f))
            (ask place 'things)))
  
  (define (greet-people person people)
    (if (not (null? people))
        (ask person 'say
             (cons "Hi"
                   (map (λ (p) (ask p 'name))
                        people)))
        'sure-is-lonely-in-here))
  
  (define (display-message list-of-stuff)
    (newline)
    (for-each (λ (s) (display s) (display " "))
              list-of-stuff)
    'message-displayed)
  
  (define (random-neighbour place)
    (pick-random (ask place 'neighbours)))
  
  
  (define (pick-random lst)
    (if (null? lst)
        #f      
        (list-ref lst (random (length lst)))))  ;; See manual for LIST-REF
  
  (define (pick-drone-target lst)
    (if (null? lst)
        #f
        (car (sort lst
                   (sort-on *sort-target-index*)))))
  
  (define-syntax register
    (syntax-rules ()
      ((_ m-obj)
       (register-object m-obj
                        (cond ((is-a m-obj 'bot?) bot-path)
                              ((is-a m-obj 'drone?) bot-path)
                              (else human-path))
                        (symbol->string (ask m-obj 'name))
                        (ask m-obj 'place)))
      ((_ m-obj path)
       (register-object m-obj
                        (if (dc-path? path) path
                            human-path)
                        (symbol->string (ask m-obj 'name))
                        (ask m-obj 'place)))))
  
  (add-to-tick-list clock))