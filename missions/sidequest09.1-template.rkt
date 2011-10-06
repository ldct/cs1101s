;
; CS1101S --- Programming Methodology (Scheme)
; 
; Side quest 09.1
;
; Note that answers are commented out to allow the Tutors to 
; run your code easily while grading your problem set.

; Task 1

(define (unique lst)
  (if (null? lst) lst
      (append (if (memq (car lst) (cdr lst)) '() (list (car lst)))
              (unique (cdr lst)))))

(define (alphabet s)
  (define (zip lst n)
    (if (null? lst)
        lst
        (append (list (list n (car lst)))
                (zip (cdr lst) (+ n 1)))))
  (zip (reverse (unique (reverse (append (string->list (string-upcase s))
                                         (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))) 0))

(define (number->char keyword n)
  (define (loop lst)
    (if (equal? (caar lst) n)
        (cadar lst)
        (loop (cdr lst))))
  (loop (alphabet keyword)))

(define (char->number keyword s)
  (define (loop lst)
    (if (equal? (cadar lst) s)
        (caar lst)
        (loop (cdr lst))))
  (loop (alphabet keyword)))

(define (caesar-encrypt-char n c)
  (if (or (not (char? c)) (not (memq (char-upcase c) (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
      c
      (number->char "" (modulo (+ (char->number "" (char-upcase c)) n)
                               26))))

(define (caesar-encrypt shift-length original-message)
  (list->string (map (lambda (c) (caesar-encrypt-char shift-length c))
                     (string->list original-message))))

(define (caesar-decrypt shift-length encrypted-message)
  (caesar-encrypt (- shift-length) encrypted-message))

(caesar-encrypt 3 "the quick brown fox jumps over the lazy dog")


(define (mixed-encrypt-char keyword c)
  (if (or (not (char? c)) (not (memq (char-upcase c) (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
      c
      (number->char keyword (char->number "" (char-upcase c)))))

(define (mixed-decrypt-char keyword c)
  (if (or (not (char? c)) (not (memq (char-upcase c) (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
      c
      (number->char "" (char->number keyword (char-upcase c)))))


(define (mixed-encrypt keyword original-message)
  (list->string (map (lambda (c) (mixed-encrypt-char keyword c))
                     (string->list original-message))))


(define (mixed-decrypt keyword encrypted-message)
  (list->string (map (lambda (c) (mixed-decrypt-char keyword c))
                     (string->list encrypted-message))))

(mixed-encrypt "zebras" "flee at once. we are discovered!")
(mixed-decrypt "zebras" "SIAA ZQ LKBA. VA ZOA RFPBLUAOAR!")




; Task 2

(define (zip lst1 lst2)
  (if (null? lst1)
      lst1
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (lookup key key-value)
  (cond ((null? key-value) #f)
        ((equal? key (caar key-value)) (cadar key-value))
        (else (lookup key (cdr key-value)))))

(define (invert key-value)
  (map (lambda (kvp) (list (cadr kvp) (car kvp))) key-value))

(define (create-substitution-cipher-encrypt substitution) ;substitution is a list       
  (define (encrypt-char c)
    (if (lookup c substitution)
        (lookup c substitution)
        c))
  (lambda (s)
    (list->string (map encrypt-char (string->list (string-upcase s))))))

(define (create-substitution-cipher-decrypt substitution)
  (create-substitution-cipher-encrypt (invert substitution)))

((create-substitution-cipher-encrypt '((#\a #\b))) "hallo")

(define (cyclize-1 lst)
  (append (cdr lst)
          (list (car lst))))

(define (cyclize-n n lst)
  (if (= n 0)
      lst
      (cyclize-n (- n 1) (cyclize-1 lst))))

(define ALPHA (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

; Alternate definitions for caesar-encrypt and caesar-decrypt
(define (caesar-encrypt shift-length)
   (create-substitution-cipher-encrypt (zip ALPHA 
                                            (cyclize-n shift-length ALPHA))))
(define (caesar-decrypt shift-length)
   (create-substitution-cipher-decrypt (zip ALPHA 
                                            (cyclize-n shift-length ALPHA))))
                                            
((caesar-encrypt 3) "the quick brown fox jumps over the lazy dog")
((caesar-decrypt 3) "WKH TXLFN EURZQ IRA MXPSV RYHU WKH ODCB GRJ")

; Alternate definitions for mixed-encrypt and mixed-decrypt
(define (mixed-encrypt keyword)
  (create-substitution-cipher-encrypt (zip ALPHA
                                           (reverse (unique (reverse (append (string->list (string-upcase keyword)) ALPHA)))))))
(define (mixed-decrypt keyword)
  (create-substitution-cipher-decrypt (zip ALPHA
                                           (reverse (unique (reverse (append (string->list (string-upcase keyword)) ALPHA)))))))

((mixed-encrypt "zebras") "flee at once. we are discovered!")
((mixed-decrypt "zebras") "SIAA ZQ LKBA. VA ZOA RFPBLUAOAR!")