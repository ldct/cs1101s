;
; CS1101S --- Programming Methodology
;
; Mission 15
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "generic-arith.rkt")

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

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
  (define =number? =)
 
  (put 'make 'number make-number)
  (put 'negate '(number) negate)
  (put '=zero? '(number) zero?)
  (put 'add '(number number) add)
  (put 'sub '(number number) sub)
  (put 'mul '(number number) mul)
  (put 'div '(number number) div)
  (put 'equ? '(number number) =number?)
  'done)

(define (install-rational-package)
  (define (make-rat n d) (cons n d))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (define (make-rational n d) (tag (make-rat n d)))
  (define (add-rational x y) (tag (add-rat x y)))
  (define (sub-rational x y) (tag (sub-rat x y)))
  (define (mul-rational x y) (tag (mul-rat x y)))
  (define (div-rational x y) (tag (div-rat x y)))
  
  (define (repnum->reprat num)
    (make-rat (create-number num) (create-number 1)))
  
  (define (RRmethod->NRmethod method)
    (lambda (num rat)
      (method (repnum->reprat num)
              rat)))
  
  (define (RRmethod->RNmethod method)
    (lambda (rat num)
      (method rat
              (repnum->reprat num)))) 
  
  (define (negate-rational x) (tag (make-rat (negate (numer x)) (denom x))))
  (define (=zero-rational? x) (=zero? (numer x)))
  (define (=rational? x y) (equ? (mul (numer x) (denom y))
                                 (mul (denom x) (numer y))))
    
  (put 'negate '(rational) negate-rational)
  (put '=zero? '(rational) =zero-rational?)
  
  (put 'equ? '(rational rational) =rational?)
  (put 'equ? '(number rational) (RRmethod->NRmethod =rational?))
  (put 'equ? '(rational number) (RRmethod->RNmethod =rational?))

  (put 'make 'rational make-rational)
  
  (put 'add '(rational rational) add-rational)
  (put 'add '(number rational) (RRmethod->NRmethod add-rational))
  (put 'add '(rational number) (RRmethod->RNmethod add-rational))

  (put 'sub '(rational rational) sub-rational)
  (put 'sub '(number rational) (RRmethod->NRmethod sub-rational))
  (put 'sub '(rational number) (RRmethod->RNmethod sub-rational))

  (put 'mul '(rational rational) mul-rational)
  (put 'mul '(number rational) (RRmethod->NRmethod mul-rational))
  (put 'mul '(rational number) (RRmethod->RNmethod mul-rational))

  (put 'div '(rational rational) div-rational)
  (put 'div '(number rational) (RRmethod->NRmethod div-rational))
  (put 'div '(rational number) (RRmethod->RNmethod div-rational))

  'done)

(define (install-complex-package)
  (define (make-com r i) (cons r i))
  (define (real x) (car x))
  (define (imag x) (cdr x))
  (define (add-com x y)
    (make-com (add (real x) (real y))
              (add (imag x) (imag y))))
  (define (sub-com x y)
    (make-com (sub (real x) (real y))
              (sub (imag x) (imag y))))
  (define (mul-com x y) 
    (make-com (sub (mul (real x) (real y)) 
                   (mul (imag x) (imag y)))
              (add (mul (real x) (imag y))
                   (mul (real y) (imag x)))))
  (define (div-com x y)  
    (let ((com-conj (complex-conjugate y)))
      (let ((x-times-com-conj (mul-com x com-conj))
            (y-times-com-conj (mul-com y com-conj)))
        (make-com (div (real x-times-com-conj) (real y-times-com-conj))
                  (div (imag x-times-com-conj) (real y-times-com-conj))))))
  (define (complex-conjugate x)
    (make-com (real x) 
	      (negate (imag x))))
  
  (define (tag x) (attach-tag 'complex x))
  (define (make-complex r i) (tag (make-com r i)))
  (define (add-complex x y) (tag (add-com x y)))
  (define (sub-complex x y) (tag (sub-com x y)))
  (define (mul-complex x y) (tag (mul-com x y)))
  (define (div-complex x y) (tag (div-com x y)))
  
  (define (negate-complex x) (tag (make-com (negate (real x)) (negate (imag x)))))
  (define (=zero-complex? x) (and (=zero? (real x)) (=zero? (imag x))))  
  (define (=complex? x y) (=zero-complex? (sub-com x y)))

  (define (repnum->repcom num)
    (make-com (create-number num) (create-number 0)))
  
  (define (CCmethod->NCmethod method)
    (lambda (num com)
      (method
       (repnum->repcom num)
       com)))
  
  (define (CCmethod->CNmethod method)
    (lambda (com num)
      (method
       com
       (repnum->repcom num))))
  
  (put 'make 'complex (lambda (x y) ;x, y = {rational . {...}}
                        (cond ((and (eq? 'complex (type-tag x)) (eq? 'complex (type-tag y)))
                               (add x (mul (create-complex (create-number 0) (create-number 1))
                                           y)))
                              ((and (eq? 'rational (type-tag x)) (eq? 'rational (type-tag y)))
                               (make-complex x y))
                              ((and (eq? 'rational (type-tag x)) (eq? 'complex (type-tag y)))
                               (create-complex (create-complex x (create-rational (create-number 0) (create-number 1))) y))
                              ((and (eq? 'complex (type-tag x)) (eq? 'rational (type-tag y)))
                               (create-complex x 
                                               (create-complex y (create-rational (create-number 0) (create-number 1)))))
                              (else (make-complex x y)))))
  
  (put 'add '(complex complex) add-complex)
  (put 'add '(number complex) (CCmethod->NCmethod add-complex))
  (put 'add '(complex number) (CCmethod->CNmethod add-complex))
  
  (put 'sub '(complex complex) sub-complex)
  (put 'sub '(number complex) (CCmethod->NCmethod sub-complex))
  (put 'sub '(complex number) (CCmethod->CNmethod sub-complex))
  
  (put 'mul '(complex complex) mul-complex) 
  (put 'mul '(number complex) (CCmethod->NCmethod mul-complex))
  (put 'mul '(complex number) (CCmethod->CNmethod mul-complex))
  
  (put 'div '(complex complex) div-complex)
  (put 'div '(number complex) (CCmethod->NCmethod div-complex))
  (put 'div '(complex number) (CCmethod->CNmethod div-complex))
  
  (put 'negate '(complex) negate-complex)
  (put '=zero? '(complex) =zero-complex?)
  
  (put 'equ? '(complex complex) =complex?)
  (put 'equ? '(number complex) (CCmethod->NCmethod =complex?))
  (put 'equ? '(complex number) (CCmethod->CNmethod =complex?))
    
  
  ;Task 1
  
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (reprat->repcom rat)
    (make-com (create-rational (numer rat) (denom rat))
              (create-rational (create-number 0)
                               (create-number 1))))
  
  (put 'equ? '(complex rational) (lambda (c r) (=complex? c (reprat->repcom r))))
  (put 'equ? '(rational complex) (lambda (r c) (=complex? (reprat->repcom r) c)))
  
  'done)



(install-number-package)
(install-rational-package)
(install-complex-package)



;Some test cases
(define n0 (create-number 0))
(define n7 (create-number 7))
(define n1 (create-number 1))
(define n14 (create-number 14))
(define n2 (create-number 2))
(define r7 (create-rational n14 n2))
(define r0 (create-rational n0 n1))
(define c7 (create-complex r7 r0))

(equ? n1 n1)
;#t
(equ? n1 r7)
;#f
(equ? n1 c7)
;#f
(equ? r7 n7)
;#t
(equ? r7 r7)
;#t
(equ? r7 c7)
;#t
(equ? c7 n1)
;#f
(equ? c7 r7)
;#t
(equ? c7 n7)
;#t

;;Test
(install-number-package)
(install-rational-package)
(install-complex-package)

(define c1+i2 (create-complex (create-number 1) (create-number 2)))
(define c3+i4 (create-complex (create-number 3) (create-number 4)))
(create-complex c1+i2 c3+i4)  ;Should return a complex number -3 + 5i
(create-complex (create-rational (create-number 2) (create-number 4)) c3+i4) ; Should return a complex number -(14/4) + (3/1)i
