;
; CS1101S --- Programming Methodology
;
; Sidequest 19.1
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.


;;;;;;;;;;;;;;;;;;;;;;;;;
; START SUPPORTING CODE ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;true if message matches is-x? for some x
(define (is-x? message)
  (let* ((str (symbol->string message))
         (len (string-length str))
         (left (if (> len 2) (substring str 0 3)))
         (right (if (> len 0) (substring str (- len 1) len))))
    (and (equal? left "is-")
         (equal? right "?"))))

;resolve m on x
;use this to dispatch unknown methods
;on classes which implement multiple inheritance
(define (mro x m full-message)
  (define (helper parents)
    (if (eq? parents '()) ;root class
        (apply x full-message)
        (let* ((immediate (map car parents))
               (immediate-methods (map (lambda (x) (list x (x 'methods))) immediate))
               (immediate-supports (map (lambda (x) (list (car x) (pair? (member m (cadr x))))) immediate-methods))
               (all-supports (filter cadr immediate-supports)))
          (if (pair? all-supports)
              (apply (caar all-supports) full-message)
              (helper (filter pair? (map cadr parents)))
              ))))
  (helper (filter pair? (x 'parents))))

(define (make-object . child)
  
  (define (self . message)
    (case (car message)
      ((is-object?) #t)
      ((parents) '())
      ((methods) '(is-object?))
      (else (if (is-x? (car message))
                #f
                (list 'no-method-found (car message))))))
  
  (define true-self (if (null? child) self (car child)))
  
  self)


(define (make-named-object name . child)
  
  (define (self . message)
    (case (car message)
      ((is-named-object?) #t)
      ((parents) (list parent (parent 'parents)))
      ((methods) '(name))
      ((name) (if (null? (cdr message))
                  name
                  (set! name (cadr message))))
      (else (apply parent message))))
  
  (define true-self (if (null? child) self (car child)))
  
  (define parent (make-object true-self))
  
  self)

(define (make-person name . child)
  (let ((partner #f))
    
    (define (self . message)
      (case (car message)
        ((is-person?) #t)
        ((parents) (list parent (parent 'parents)))
        ((methods) '(talk join-partner partner))
        ((talk) (if partner
                    (display (string-append 
                              (true-self 'name) 
                              " says: Hi, I am " (true-self 'name) " and I am partnered with " (partner 'name) "!\n"))
                    (display (string-append (true-self 'name) " says: Hi, I am " (true-self 'name) "!\n"))))
        
        ((join-partner) 
         (cond (partner (display (string-append (true-self 'name) " exclaims: I am already partnered!\n")))
               ((not ((cadr message) 'is-person? )) (display 
                                                     (string-append 
                                                      (true-self 'name) 
                                                      " exclaims: That is not a person! I can't partner with it!\n")))
               (else
                (let* ((potential-partner (cadr message))
                       (check ((cadr message) 'partner)))
                  (cond ((eq? check #t) (set! partner potential-partner)) 
                        ((not check) (begin (set! partner #t)
                                            (potential-partner 'join-partner true-self)
                                            (set! partner potential-partner)))
                        (else (display (string-append (true-self 'name) " exclaims: " 
                                                      (potential-partner 'name) " is already partnered!\n"))))))))        
        ((partner) partner)
        (else (apply parent message))))
    
    (define true-self (if (null? child) self (car child)))
    
    (define parent (make-named-object name true-self))
    
    self))


(define (make-JFDI-warrior name . child)
  (define (self . message)
    (case (car message)
      ((is-JFDI-warrior?) #t)
      ((parents) (list parent (parent 'parents)))
      ((methods) '(is-JFDI-warrior? swing))
      ((swing) (display (string-append (true-self 'name) " swings lightsaber!\n")))
      (else (apply parent message))))
  
  (define true-self (if (null? child) self (car child)))
  
  (define parent (make-person name true-self))
  
  self)

(define (make-SITH-lord name SITH-name . child)
  
  (define (self . message)
    (case (car message)
      ((is-SITH-lord?) #t)
      ((parents) (list parent (parent 'parents)))
      ((methods) '(is-SITH-lord? SITH-name talk))
      ((SITH-name) (if (null? (cdr message))
                       SITH-name
                       (set! SITH-name (cadr message))))
      ((talk) (begin
                (apply parent message)
                (display (string-append 
                          (true-self 'name) 
                          " says: Make that Lord " 
                          (true-self 'SITH-name) 
                          ", muahahaha...\n"))))
      (else (apply parent message))))
  
  (define true-self (if (null? child) self (car child)))
  
  (define parent (make-person name true-self))
  
  self)

(define (make-traitor JFDI-name SITH-name . child)
  
  (define (self . message)
    (case (car message)
      ((is-traitor?) #t)
      ((parents) (list (list parent1 (parent1 'parents)) (list parent2 (parent2 'parents))))
      ((methods) '(is-traitor))
      (else (if (is-x? (car message))
                (or (apply parent1 message) (apply parent2 message))
                (mro self (car message) message))))) ;use mro on a class that uses multiple inheritance
  
  (define true-self (if (null? child) self (car child)))
  
  (define parent1 (make-JFDI-warrior JFDI-name true-self))
  (define parent2 (make-SITH-lord JFDI-name SITH-name true-self))
  
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;
;  END SUPPORTING CODE  ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
; Count Dooku Example ;
;;;;;;;;;;;;;;;;;;;;;;;

(define dooku (make-traitor "Dooku" "Tyranus"))
(dooku 'is-JFDI-warrior?)
; #t

(dooku 'is-SITH-lord?)
; #t

(dooku 'is-traitor?)
; #t

(dooku 'swing)
; dooku swings lightsaber!

(dooku 'talk) ; error here - why didn't he mention that he is Lord Tyranus?
; dooku says: Hi, I am Dooku!
 
;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

;I will use a list to denote ties; for eg 1,(2,3),4 means that the order can be 1,2,3,4 or 1,3,2,4 
;Junwei's search order: G,E,F,(B,D),(C,A)
;Joe's search order: F,E,D,B,C,A
;pros/cons: Joe's method is easier to code, requiring each class to mantain only a list of parent:distance
;pairs while Junwei's method requires breadth-first search.
;the current method is basically dfs which sucks.

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

;I think Junwei's method is more intuitive because in his method creating indirection (inheritance)
;between the superclass implementing the function you want to use and the root class doesn't 
;affect the method resolution order since the distance from the superclass to the child class is
;the same while in Joe's method it does affect. 

;;;;;;;;;;
; Task 3 ;
;;;;;;;;;;

;;; Implement the method
