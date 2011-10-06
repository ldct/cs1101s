;
; CS1101S --- Programming Methodology
;
; Mission 12
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "generic-arith.rkt")


;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

; (a)
; Generic-num -> Generic-Num

; (b)
; Because it must satisfy the contract (square x) == (mul x x), and defining (square x) as (mul x x) makes it satisfy the contract
; automatically. Also with the second method we would have to implement square 3 times, one for each representation of numbers.


;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

; makenumber :: Sch-Num->RepNum
; negate :: RepNum->RepNum
; zero? :: RepNum->Bool


;;;;;;;;;;
; Task 3 ;
;;;;;;;;;;

; because most procedures take two arguments but make-number takes one.
; apply-generic uses dotted notation (apply-generic op . args) to make it a variadic function
; and uses apply (apply proc (map contents arg)) because proc might have different arity depending on what 
; op is passed in.
; incidentally I must say that this is the first time the word "keyed" appears in the entire paper and it's not previously defined


;;;;;;;;;;;;;
; Task 4 & 5;
;;;;;;;;;;;;;

(define (install-number-package)
  (define (tag x)
    (attach-tag 'number x))
  (define (make-number x) (tag x))
  (define (negate x) (tag (- x)))
  (define (zero? x) (= x 0))
  (define (add x y) (tag (+ x y)))
  (define (sub x y) (tag (- x y)))
  (define (mul x y) (tag (* x y)))
  (define (div x y) (tag (/ x y)))
  
  ;; Task 4 Answer should go below
  (define =number? =)
  ;; End of Task 4 Answer
  ; =number? :: RepNum->RepNum
  
  (put 'make 'number make-number)
  (put 'negate '(number) negate)
  (put '=zero? '(number) zero?)
  (put 'add '(number number) add)
  (put 'sub '(number number) sub)
  (put 'mul '(number number) mul)
  (put 'div '(number number) div)

  ;; Task 5 Answer goes below
  (put 'equ? '(number number) =number?)
  ;; End of Task 5 Answer

  
  'done)

(install-number-package)

;;Test
(define n2 (create-number 2))
(define n4 (create-number 4))
(define n6 (create-number 6))

; Expression should return true
(equ? n4 (sub n6 n2))