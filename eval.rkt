;r5rs

;Frame and environment data structures

(define apply-in-underlying-scheme apply)

(define make-frame cons)
(define frame-variables car)
(define frame-values cdr)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define f (make-frame '(a b c) '(1 2 3)))
(display "f") f 
(display "(frame-values f)") (frame-values f)

(define first-frame car)
(define enclosing-environment cdr)
(define the-empty-environment '())

(define (extend-environment var vals base-env)
  (cons (make-frame var vals)
        base-env))

(define e (extend-environment '(a b) '(1 2) the-empty-environment))
(set! e (extend-environment '(a b c) '(aa bb cc) e))
(display "e") e

(define (lookup-variable-value var env)
  (define (scan vars vals)
    (cond ((null? vars)
           (lookup-variable-value var (enclosing-environment env)))
          ((eq? var (car vars))
           (car vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan (frame-variables (first-frame env))
            (frame-values (first-frame env)))))

(display "(lookup-variable-value b e)") (lookup-variable-value 'b e)

(define (set-variable-value! var val env)
  (define (scan vars vals)
    (cond ((null? vars)
           (set-variable-value! var (enclosing-environment env)))
          ((eq? var (car vars))
           (set-car! vals val))
          (else
           (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (scan (frame-variables (first-frame env))
            (frame-values (first-frame env)))))

(set-variable-value! 'b 33 e)
(display "e") e

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define-variable! 'd 66 e)
(display "e") e

;Tagged list

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

;Primitive procedure

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define primitive-implementation cadr)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(display "(apply-primitive-procedure (list 'primitive +) '(1 2))") (apply-primitive-procedure (list 'primitive +) '(1 2))

;Procedure data structure

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (self-evaluating? exp) 
  (or (number? exp) (string? exp)))
(define variable? symbol?)

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define text-of-quotation cadr)

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define assignment-variable cadr)
(define assignment-value caddr)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       ( (assignment-value exp) env)
                       env)
  'ok)

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (lambda? exp) (tagged-list? exp 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (if? exp) (tagged-list? exp 'if))
(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (begin? exp) (tagged-list? exp 'begin))
(define begin-actions cdr)
(define (last-exp? seq) (null? (cdr seq)))
(define first-exp car)
(define rest-exps cdr)

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (application? exp) (pair? exp))
(define operator car)
(define operands cdr)
(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (eval-assignment exp env))
        ((definition? exp)
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         "error")
        ))

;Populate primiitive procedures

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        ;<more primitives>
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;Apply

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;REPL

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (display the-global-environment)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
(driver-loop)


(apply `(primitive ,+) '(1 2))