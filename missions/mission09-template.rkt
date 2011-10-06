;
; CS1101S --- Programming Methodology (Scheme)
; 
; Mission 9
;
; Note that answers are commented out to allow the Tutors to 
; run your code easily while grading your problem set.

(load "rsa-mission09.rkt")

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


;===========
'(--------------------- Task 1 ---------------------)
;===========

;;; Your solution

(define make-msg-sig-pair cons)
(define msg-of car)
(define sig-of cdr)

(define (encrypt-and-sign msg sender-private-key recipient-public-key)
  (let ((ciphertext (RSA-encrypt msg recipient-public-key)))
    (make-msg-sig-pair ciphertext
                       (car (RSA-convert-list (list (compress ciphertext)) sender-private-key)))))

;;; Test your procedure

(define result2 
  (encrypt-and-sign "Test message from user 1 to user 2"
                    test-private-key1
                    test-public-key2))

;;; Result's message part should be
;;; (296342791 45501589 263575681 219298391 4609203 331301818 178930017 242685109 1345058)
;;; the signature is 254363563

(define (authenticate-and-decrypt enc-pair sender-public-key recipient-private-key)
  (let* ((plaintext (RSA-unconvert-list (msg-of enc-pair) recipient-private-key))
         (real-sig (compress (msg-of enc-pair)))
         (claim-sig (RSA-unconvert-list (list (sig-of enc-pair)) sender-public-key)))
    (if (= real-sig (car claim-sig))
        (intlist->string plaintext)
        #f)))
(authenticate-and-decrypt result2 test-public-key1 test-private-key2)
;; "Test message from user 1 to user 2  "

(define fake-result2
  (make-msg-sig-pair (msg-of result2)
                     (+ 1 (sig-of result2))))

(authenticate-and-decrypt fake-result2 test-public-key1 test-private-key2)
;; #f

;===========
'(--------------------- Task 2 ---------------------)
;===========

(define recieved-pair (make-msg-sig-pair received-mystery-message received-mystery-signature))

(define (show-sender senders msg private-key)
  (cond ((= (length senders) 0) #f)
        ((authenticate-and-decrypt msg (eval (car senders)) private-key)
         (car senders))
        (else (show-sender (cdr senders) msg private-key))))

(display "from: ")
(show-sender '(Ben-public-key Hung-public-key Nicolas-public-key DucHiep-public-key Ali-public-key)
             recieved-pair Darth-private-key)

(display "message: \n")
(RSA-decrypt received-mystery-message Darth-private-key)

;; from: DucHiep-public-key
;; message: 
;; "Greetings, Darth. The coordinates of our secret base are as follows: (104677, 104681, 104683, 104693, 104701, 104707, 104711, 104717, 104723, 104729). The JFDI is currently away from the planet training our new batch of Padawans, and it is imperative for you to seize the chance to strike now. Please hurry. "