;;;; rsa-mission08.rkt

;;;; fast modular exponentiation. From the textbook, section 1.2.4

(define (enumerate-interval m n)
  (define (helper n l)
    (if (> m n)
        l
        (helper (- n 1) (cons n l))))
  (helper n '()))

(define (expmod b e m)
  (cond ((zero? e) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))

(define (square x) (* x x))


;;; An RSA key consists of a modulus and an exponent.

(define make-key cons)
(define key-modulus car)
(define key-exponent cdr)

;(RSA-T number (m,e)) = number^e (mod m)
(define (RSA-transform number key)
  (expmod number (key-exponent key) (key-modulus key)))

;;; The following routine compresses a list of numbers to a single
;;; number for use in creating digital signatures.

(define (compress intlist)
  (define (add-loop l)
    (if (null? l)
        0
        (+ (car l) (add-loop (cdr l)))))
  (modulo (add-loop intlist) (expt 2 28)))
;;;; generating RSA keys

;;; To choose a prime, we start searching at a random odd number in a
;;; specifed range

(define (choose-prime smallest range)
  (let ((start (+ smallest (choose-random range))))
    (search-for-prime (if (even? start) (+ start 1) start))))

(define (search-for-prime guess)
  (if (fast-prime? guess 2)
      guess
      (search-for-prime (+ guess 2))))

;;; The following procedure picks a random number in a given range,
;;; but makes sure that the specified range is not too big for
;;; Scheme's RANDOM primitive.

(define choose-random
  ;; restriction of Scheme RANDOM primitive
  (let ((max-random-number (- (expt 2 31) 1))) 
    (lambda (n)
      (random (floor (min n max-random-number))))))


;;; The Fermat test for primality, from the texbook section 1.2.6

(define (fermat-test n)
    (let ((a (choose-random n)))
      (= (expmod a n n) a)))

(define (fast-prime? n times)
    (cond ((zero? times) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
;;; RSA key pairs are pairs of keys

(define make-key-pair cons)
(define key-pair-public car)
(define key-pair-private cdr)

;;; generate an RSA key pair (k1, k2).  This has the property that
;;; transforming by k1 and transforming by k2 are inverse operations.
;;; Thus, we can use one key as the public key andone as the private key.

(define (generate-RSA-key-pair)
  (let ((size (expt 2 14)))
    ;; we choose p and q in the range from 2^14 to 2^15.  This insures
    ;; that the pq will be in the range 2^28 to 2^30, which is large
    ;; enough to encode four characters per number.
    (let ((p (choose-prime size size))
          (q (choose-prime size size)))
    (if (= p q)       ;check that we haven't chosen the same prime twice
        (generate-RSA-key-pair)     ;(VERY unlikely)
        (let ((n (* p q))
              (m (* (- p 1) (- q 1))))
          (let ((e (select-exponent m)))
            (let ((d (invert-modulo e m)))
              (make-key-pair (make-key n e) (make-key n d)))))))))


;;; The RSA exponent can be any random number relatively prime to m

(define (select-exponent m)
  (let ((try (choose-random m)))
    (if (= (gcd try m) 1)
        try
        (select-exponent m))))


;;; Invert e modulo m
;;; Note that solve-ax+by=1 is not defined in this file, and you are NOT required to implement it.
(define (invert-modulo e m)
  (if (= (gcd e m) 1)
      (let ((y (cdr (solve-ax+by=1 m e))))
        (modulo y m))                   ;just in case y was negative
      (error "gcd not 1" e m)))


;;; Actual RSA encryption and decryption

(define (RSA-encrypt string key1)
  (RSA-convert-list (string->intlist string) key1))

(define (RSA-convert-list intlist key)
  (let ((n (key-modulus key)))
    (define (convert lst sum)
      (if (null? lst)
          '()
          (let ((x (RSA-transform (modulo (- (car lst) sum) n)
                                  key)))
            (cons x (convert (cdr lst) x)))))
    (convert intlist 0)))

(define (RSA-decrypt intlist key2)
  (intlist->string (RSA-unconvert-list intlist key2)))

;;;(define (RSA-unconvert-list intlist key) ...)
;;;  You must complete this procedure 


;;;; searching for divisors.

;;; The following procedure is very much like the find-divisor
;;; procedure of section 1.2.6 of the test, except that it increments
;;; the test divisor by 2 each time (compare exercise 1.18 of the
;;; text).  You should be careful to call it only with odd numbers n.

(define (smallest-divisor n)
  (find-divisor n 3))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))


;;;; converting between strings and numbers

;;; The following procedures are used to convert between strings, and
;;; lists of integers in the range 0 through 2^28.  You are not
;;; responsible for studying this code -- just use it.

;;; Convert a string into a list of integers, where each integer
;;; encodes a block of characters.  Pad the string with spaces if the
;;; length of the string is not a multiple of the blocksize.

(define (string->intlist string)
  (let ((blocksize 4))
    (let ((padded-string (pad-string string blocksize)))
      (let ((length (string-length padded-string)))
        (block-convert padded-string 0 length blocksize)))))

(define (block-convert string start-index end-index blocksize)
  (if (= start-index end-index)
      '()
      (let ((block-end (+ start-index blocksize)))
        (cons (charlist->integer
	       (string->list (substring string start-index block-end)))
              (block-convert string block-end end-index blocksize)))))

(define (pad-string string blocksize)
  (let ((rem (remainder (string-length string) blocksize)))
    (if (= rem 0)
        string
        (string-append string (make-string (- blocksize rem) #\Space)))))

;;; Encode a list of characters as a single number
;;; Each character gets converted to an ascii code between 0 and 127.
;;; Then the resulting number is c[0]+c[1]*128+c[2]*128^2,...

(define (charlist->integer charlist)
  (let ((n (char->integer (car charlist))))
    (if (null? (cdr charlist))
        n
        (+ n (* 128 (charlist->integer (cdr charlist)))))))

;;; Convert a list of integers to a string. (Inverse of
;;; string->intlist, except for the padding.)

(define (intlist->string intlist)
  (list->string
   (apply
    append
    (map integer->charlist intlist))))
;;; Decode an integer into a list of characters.  (This is essentially
;;; writing the integer in base 128, and converting each "digit" to a
;;; character.)

(define (integer->charlist integer)
  (if (< integer 128)
      (list (integer->char integer))
      (cons (integer->char (remainder integer 128))
            (integer->charlist (quotient integer 128)))))

;;;; the following procedure is handy for timing things

(define (timed f . args)
    (let ((v (call-with-values
              (lambda () (time-apply f args))
              (lambda (a b c d) (list a b c d)))))
      (display 'time:) (display (cadr v)) (newline)
      (caar v)))

(define (measure-time f . args)
    (let ((v (call-with-values
              (lambda () (time-apply f args))
              (lambda (a b c d) (list a b c d)))))
      (cadr v)))

;;;; Some initial test data

(define test-key-pair1
  (make-key-pair
   (make-key 385517963 90747943)
   (make-key 385517963 137332327)))

(define test-key-pair2
  (make-key-pair
   (make-key 432208237 282377377)
   (make-key 432208237 401849313)))

(define test-public-key2 (key-pair-public test-key-pair2))
(define test-private-key2 (key-pair-private test-key-pair2))