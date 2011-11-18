;;; CS1101S OOP System
;;; Simple object system with inheritance

(define (ask object message . args)
  (let ((method (get-method object message)))
    (if (method? method)
	(apply method (cons object args))
	(display (list "Unknown method" message)))))

(define (get-method object message)
  (object message))

(define (no-method name)
  (list 'no-method name))

(define (method? x)
  (not (no-method? x)))

(define (no-method? x)
  (if (pair? x)
      (eq? (car x) 'no-method)
      false))

(define (make-named-object name)
  (lambda (message) 
    (if (eq? message 'name) 
        (lambda (self) name)
        (no-method name))))