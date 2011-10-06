;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file generic-arith.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nil '())
(require scheme/mpair)

;;; The bottom level typing system

(define attach-tag mcons)

(define (type-tag datum)
  (if (mpair? datum)
      (mcar datum)
      (error "Bad typed datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (mpair? datum)
      (mcdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))

;;; The apply-generic mechanism.  
;;;  Note that we don't deal with coercion here.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for the given types -- APPLY-GENERIC"
		 (mlist op type-tags))))))

;;; Code for creating the table, you don't need to worry about this.
(define (make-table)
  (let ((local-table (mlist '*table*)))
    
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                      (set-mcdr! subtable
                                 (mcons (mcons key-2 value)
                                        (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;; GENERIC ARITHMETIC OPERATIONS

;;; Generic-Num = ({number} X RepNum) U ({rational} X RepRat) U 
;;;               ({complex} X RepCom)

; add,sub,mul,div: (Generic-Num, Generic-Num) -> Generic-Num
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; negate: Generic-Num -> Generic-Num
(define (negate x) (apply-generic 'negate x))

; =zero?: Generic-Num -> Sch-Bool
(define (=zero? x) (apply-generic '=zero? x))

; equ?: (Generic-Num, Generic-Num) -> Sch-Bool
(define (equ? x y) (apply-generic 'equ? x y))

;;; a sample compound generic operation
(define (square x) (mul x x))


;;; generic ordinary number package
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
  (put 'make 'number make-number)
  (put 'negate '(number) negate)
  (put '=zero? '(number) zero?)
  (put 'add '(number number) add)
  (put 'sub '(number number) sub)
  (put 'mul '(number number) mul)
  (put 'div '(number number) div)
  'done)

;;; Generic Ordinary Number Package User Interface

;;; A convenient external procedure for building a generic
;;; ordinary number from Scheme numbers.

;;; Sch-Num --> ({number} X RepNum)
(define (create-number x) 
  ((get 'make 'number) x))


;;; generic rational number package
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
  ; procedures to do coercion
  (define (RRmethod->NRmethod method)
    (lambda (num rat)
      (method
       (repnum->reprat num) ; repnum->reprat not implemented yet
       rat)))
  (put 'make 'rational make-rational)
  (put 'add '(rational rational) add-rational)
  (put 'sub '(rational rational) sub-rational)
  (put 'mul '(rational rational) mul-rational)  
  (put 'div '(rational rational) div-rational)
  'done)

;;; Generic Rational Number Package User Interface

;;; A convenient procedure for building a generic rational
;;; number from generic numbers.

;;;    (GN, GN) --> ({rational} X RepRat)
(define  (create-rational n d)
  ((get 'make 'rational) n d))


;;; generic complex number package in rectangular form (a+bi)
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
  ; procedures to coerce RepNums to RepComs
  (define (CCmethod->NCmethod method)
    (lambda (num com)
      (method
       (repnum->repcom num) ; repnum->repcom not implemented yet
       com)))
  (put 'make 'complex make-complex)
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)  
  (put 'div '(complex complex) div-complex)
  'done)

;;; Generic Complex Number Package User Interface

;;; A convenient procedure for building a generic complex
;;; number from generic numbers.

;;;    (GN, GN) --> ({complex} X RepRat)
(define (create-complex r i)
  ((get 'make 'complex) r i))