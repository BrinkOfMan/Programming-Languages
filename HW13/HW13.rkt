#lang racket
(require(lib "eopl.ss" "eopl"))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (saved-var var?)
   (saved-val scheme-value?)
   (saved-env environment?)))

(define var? symbol?)
(define scheme-value? (lambda (s) #t))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? search-var saved-var)
                      saved-val
                      (apply-env saved-env search-var))))))

(define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

(define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?)))
    
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

(define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

(define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  (define value-of
    (lambda (exp env)
      (cases expression exp
        (const-exp (num) (num-val num))
        (var-exp (var) (apply-env env var))
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        (add-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (+ num1 num2)))))
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env)))))))

(define-datatype program program?
 (a-program(expl1 expression?)))


(define-datatype expression expression?
  (const-exp(num number?))
  (var-exp(var symbol?))
  (diff-exp
       (exp1 expression?)
       (exp2 expression?))
  (add-exp
       (exp1 expression?)
       (exp2 expression?))
  (zero?-exp(exp1 expression?))
  (if-exp
      (exp1 expression?)
      (exp2 expression?)
      (exp3 expression?))
  (let-exp
      (var symbol?)
      (exp1 expression?)
      (body expression?)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
        ("+" "(" expression "," expression ")")
        add-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      ))

(define scan&parse
     (sllgen:make-string-parser the-lexical-spec the-grammar))


;Ethan Brinkman
;HW13: Due 4/22

;Create the trace for each of the following:

;+(x, 3)
(run "+(x, 3)")
(value-of-program(scan&parse "+(x, 3)"))
(value-of-program(a-program (add-exp (var-exp 'x) (const-exp 3))))
(value-of (add-exp (var-exp 'x) (const-exp 3)) (init-env))
;       val1 -> (vo (var-exp x) ie)
;		(apply-env ie x)
;		(num-val 10)
;	val2 -> (vo (const-exp 3) ie)
;		(num-val 3)
;	num1 -> (expval->num val1) =
;		  (expval->num (num-val 10)) =
;		   10
;	num2 -> (expval->num val2) =
;		  (expval->num (num-val 3)) =
;		   3
; (num-val (+ num1 num2)) =
; (num-val (+ 10 3)) =
;	(num-val 13)
  
;let x= 4 in +(x, 3)
(run "let x=4 in +(x, 3)")
(value-of-program(scan&parse "let x=4 in +(x, 3)"))
(value-of-program(a-program (let-exp 'x (const-exp 4) (add-exp (var-exp 'x) (const-exp 3)))))
(value-of (let-exp 'x (const-exp 4) (add-exp (var-exp 'x) (const-exp 3))) (init-env))
;       val1-> (vo (const-exp 4) ie)
;	           (num-val 4)
;	(vo body (extend-env var val1 ie)) =
;	(vo (add-exp (var-exp x) (const-exp 3)) (extend-env var val1 ie))	=
;	(vo (add-exp (var-exp x) (const-exp 3)) (extend-env x (num-val 4) ie))
;	Val11 -> (vo exp1 (extend-env x (num-val 4) ie)) =
;		(vo (var-exp x) (extend-env x (num-val 4) ie)) 
;		(apply-env (extend-env x (num-val 4) ie) x)
;		(num-val 4)
;	val12 -> (vo exp2 (extend-env x (num-val 4) ie)) =
;		(vo (const-exp 3) (extend-env x (num-val 4) ie)) 
;		(num-val 3)
;	num11 -> (expval->num val11) =
;		    (expval->num (num-val 4))
;		    4
;	num12 -> (expval->num val12) =
;		    (expval->num (num-val 3))
;		    3
;	(num-val (+ num1 num2)) =
;	(num-val   (+ 4 3)  ) =
;	(num-val 7)

;For graders:
;If you had to choose: vanilla, chocolate, caramel, rocky road, or mint-chocolate?



