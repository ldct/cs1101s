;
; CS1101S --- Programming Methodology (Scheme)
; 
; Mission 10
;
; Note that answers are commented out to allow the Tutors to 
; run your code easily while grading your problem set.

(load "rsa-mission10.rkt")

;;; You have defined the following in your previous missions. Just copy and paste your solution.
(define (RSA-unconvert-list intlist private-key)
  (define n (key-modulus private-key))
  (define (unconvert lst sum)
    (if (null? lst)
        '()
        (let ((x (modulo (+ sum
                            (RSA-transform (car lst) private-key))
                         n)))
          (cons x (unconvert (cdr lst) (car lst))))))
    (unconvert intlist 0))

(define make-msg-sig-pair cons)
(define msg-of car)
(define sig-of cdr)

(define (encrypt-and-sign msg sender-private-key recipient-public-key)
  (let ((ciphertext (RSA-encrypt msg recipient-public-key)))
    (make-msg-sig-pair ciphertext
                       (car (RSA-convert-list (list (compress ciphertext)) sender-private-key)))))

(define (authenticate-and-decrypt enc-pair sender-public-key recipient-private-key)
  (let* ((plaintext (RSA-unconvert-list (msg-of enc-pair) recipient-private-key))
         (real-sig (compress (msg-of enc-pair)))
         (claim-sig (RSA-unconvert-list (list (sig-of enc-pair)) sender-public-key)))
    (if (= real-sig (car claim-sig))
        (intlist->string plaintext)
        #f)))

(define make-msg-sig-pair cons)
(define msg-of car)
(define sig-of cdr)

;===========
'(--------------------- Task 1 ---------------------)
;===========

;;; Your solution

(define (solve-ax+by=1 a b)
  (if (= b 0)
      (cond ((= a 1) (cons 1 0))
            ((= a -1) (cons -1 0))
            (else 'no_solutions))
      (let* ((r (remainder a b))
             (q (quotient a b))
             (s (car (solve-ax+by=1 b r)))
             (t (cdr (solve-ax+by=1 b r))))
        (cons t (- s (* q t))))))

;;; Test your procedure

(solve-ax+by=1 233987973 41111687)
(solve-ax+by=1 3 0)
(solve-ax+by=1 -1 0)

;===========
'(--------------------- Task 2 ---------------------)
;===========

;;; Your solution

(define (crack-rsa public-key)
  (let* ((n (key-modulus public-key))
         (e (key-exponent public-key))
         (p (smallest-divisor n))
         (q (/ n p))
         (m (* (- p 1) (- q 1)))
         (d (car (solve-ax+by=1 e m))))
    (begin (display (list d e m))
           (make-key n (+ d m)))))

(define Darth-public-key (make-key 718392397 559318161))
(crack-rsa Darth-public-key)

(define received-mystery-message
  '(255535865 487823975 233970006 402199677 684685730 495370893 505793783 430488766 706214252 701906712 
              305848322 605390796 44672387 470688259 699083046 303611674 16613960 675393316 201697511 147339103 
              560643168 654148963 277682072 342352584 617660257 422975289 555587970 103807775 557422647 374444501 
              140739994 549426840 556644462 521432724 651694770 194873304 529339110 280220529 158479350 447705539 
              231698566 347088280 59785819 318454513 562783015 653512755 495999475 614259296 473944195 112519619 
              251469208 476180269 129061314 36061245 467666520 571601389 254080342 108854174 605316599 685557750 
              445382011 399868489 313285282 14035940 612810585 174051163 243706231 156811452 620088431 367796561 
              132457378 120747903 544949799 583103126 225638470 687196941 347447348))

(define Darth-private-key (crack-rsa Darth-public-key))
(RSA-decrypt received-mystery-message Darth-private-key)