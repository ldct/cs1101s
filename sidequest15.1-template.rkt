;
; CS1101S --- Programming Methodology
;
; Mission 15 - Side Quest 1
;
; Name: Li Xuanji
; Matric no: A0091997
; Discussion group: Raymond
; (Note that points will be taken off if the discussion group is not
; specified)
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(define grand-laser-angle (/ pi 12))
(define grand-laser-angle-cosine (cos (/ pi 12)))
(define grand-laser-reach 10000)

(define (square x) (* x x))

(define x-of car)
(define y-of cadr)
(define z-of caddr)

(define (mag vec)
  (sqrt (+ (square (x-of vec))
           (square (y-of vec))
           (square (z-of vec)))))

(define (unit vec)
  (map (lambda (x)
         (/ x (mag vec)))
       vec))

(define (cosine v1 v2)
  (let ((u1 (unit v1))
        (u2 (unit v2)))
    (+ (* (x-of u1) (x-of u2))
       (* (y-of u1) (y-of u2))
       (* (z-of u1) (z-of u2)))))

(define (killed? target bystander)
  (and (< (mag (cdr bystander)) 10000)
       (< grand-laser-angle-cosine (cosine (cdr target) (cdr bystander)))))

(define (kills target lst)
  (filter (lambda (x) (killed? x target))
          lst))

(define (pad lst)
  (apply append (map (lambda (x) (list x " "))
                     lst)))

(define (select-grand-laser-target hostile-list friendly-list)
  (begin
    (map (lambda (x) 
           (display "Target: ")
           (display x)
           (display "\n")
           (display "Hostiles in area of effect: ")
           (map display (pad (kills x hostile-list)))
           (display "\n")
           (display "Friends in area of effect: ")
           (map display (pad (kills x friendly-list)))
           (display "\n\n"))
         hostile-list)
    (let* ((targets (filter (lambda (x)
                              (null? (kills x friendly-list)))
                            hostile-list))
           (deaths (map (lambda (x) (cons x (length (kills x hostile-list))))
                        targets))
           (max-deaths (apply max (map cdr deaths))))
      (if (= 0 max-deaths)
          'none
          (caar (filter (lambda (x) (= max-deaths (cdr x)))
                        deaths))))))

(select-grand-laser-target
 '((TIE0001 890 700 906)
   (TIE0002 895 740 912)
   (TIE0003 -5634 -102 8589))
 '((XW0121 862 713 999)))

