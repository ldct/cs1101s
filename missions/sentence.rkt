(define (count-in-sentence s)
  (list (length s)
        (+ (length s) -1 (foldr + 0 (map length s)))))

(define s (list (list 's 'h 'e) (list 'l 'i 'k 'e 's) (list 'p 'i 'e 's)))

(define (unique lst)
  (if (null? lst) lst
      (append (if (memq (car lst) (cdr lst)) '() (list (car lst)))
              (unique (cdr lst)))))

(define (most-frequent-letters s)
  (let* ((frequencies (map (lambda (x) (list x (apply + (map (lambda (y) (if (eq? y x) 1 0))
                                                             (apply append s)))))
                           (apply append s))))
    (unique (map car (filter (lambda (x) (= (cadr x) (apply max (map cadr frequencies)))) frequencies)))))

(most-frequent-letters s)

(define (head n lst)
  (if (or (= 0 n) (null? lst))
      '()
      (append (list (car lst))
              (head (- n 1) (cdr lst)))))

(define (find-end-with sentence word)
  (filter (lambda (w) (equal? (reverse word)
                              (head (length word) (reverse w))))
          sentence))

(define (consonant-pair c w)
  (if (or (null? w) (memq (car w) '(a e i o u)))
      (cons c w)
      (consonant-pair (append c (list (car w))) (cdr w))))

(define (pig-latin-word w)
  (if (memq (car w) '(a e i o u))
      (append w '(w a y))
      (append (cdr (consonant-pair '() w))
              (car (consonant-pair '() w))
              '(a y))))

(define (pig-latin s) (map pig-latin-word s))
      


