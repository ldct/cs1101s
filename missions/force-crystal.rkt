;; CS1101S Force Crystal contest

; use start-test to test your force-crystal solution 
; e.g call (start-test 10 2 '(10 10) '(3 8)) after you have implemented (force-crystal)


(define (start-test number-of-crystals number-of-params param-upper-bound param-answer)
  
  (set! CURRENT_CRYSTALS number-of-crystals)
  (set! NUMBER_OF_GUESSES 0)
  (set! NUMBER_OF_CRYSTALS number-of-crystals)
  (set! NUMBER_OF_PARAMS number-of-params)
  (set! PARAM_UPPERBOUND param-upper-bound)
  (set! PARAM_ANSWER param-answer)
  
  (force-crystal NUMBER_OF_CRYSTALS NUMBER_OF_PARAMS PARAM_UPPERBOUND))
  

(define NUMBER_OF_CRYSTALS 10)
(define NUMBER_OF_PARAMS 3)
(define PARAM_UPPERBOUND '(10 18 19))
(define PARAM_ANSWER '(3 8 5))
(define CURRENT_CRYSTALS NUMBER_OF_CRYSTALS)
(define NUMBER_OF_GUESSES 0)

(define (destroy-crystal)
  (set! CURRENT_CRYSTALS (- CURRENT_CRYSTALS 1))
  #f)

(define (compare-force lst1 lst2)
  (if (null? lst1)
      #t
      (if (> (car lst1) (car lst2))
          (destroy-crystal)
          (compare-force (cdr lst1) (cdr lst2)))))

(define (force-aura lst)
  (set! NUMBER_OF_GUESSES (+ 1 NUMBER_OF_GUESSES))
  (if (< CURRENT_CRYSTALS 1)
      (raise 'NO_MORE_CRYSTAL)
      (if (and (list? lst) (eq? (length lst) NUMBER_OF_PARAMS))
          (compare-force lst PARAM_ANSWER)
          #f)))

(define (force-aura-finalize lst)
  (letrec ((checker (lambda (lst1 lst2)
                      (if (null? lst1)
                          (printf "You have guessed correctly with ~a guesses\n" NUMBER_OF_GUESSES)
                          (and (eq? (car lst1) (car lst2))
                               (checker (cdr lst1) (cdr lst2)))))))
    (and (list? lst)
         (eq? (length lst) NUMBER_OF_PARAMS)
         (checker lst PARAM_ANSWER))))

