#lang racket
(require(lib "eopl.ss" "eopl"))

;Interpreter Additions: 14.5/15
;for expval->null?, it should actually return false even if it is fed something other than a pair or an emptylist, rather than an error
;test cases: 8/8
;Total: 22.5/23
;Excellent

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

(define-datatype program program?
 (a-program(expl1 expression?)))

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
  
(define scan&parse
     (sllgen:make-string-parser the-lexical-spec the-grammar))

;=========================================================================================================================================

;Ethan Brinkman
;HW15: Due 5/1

;Exercise 3.9
;Add list processing operations to the language, including cons, car, cdr, null? and emptylist.
;A list should be able to contain any expressed value, including another ilst.
;Give the definitions of the expressed and enoted values of the language, as in section 3.2.2.

;Here is what you must include:
;(1) copy of your complete code for define-datatype for expression.
;(2) copy of your complete code for the-grammar spec.
;(3) copy of  your complete code for value-of-program and for value-of.
;(4) copy of your complete code for the define-datatype for expval.

;You must also show that your interpreter code runs by evaluating run on each of the following: 
;(1) cons ( 7,  emptylist) 
;(2) cons ( -(x, 1),  emptylist) 
;(3) cons (cons ( -(x, 1),  emptylist),emptylist) 
;(4) cons (x, cons (cons ( -(x, 1),  emptylist),  emptylist)) 
;(5) let x = 4 in  cons (x, cons (cons ( -(x, 1),  emptylist),  emptylist)))) 
;(6) car(let x = 4 in  cons (x, cons(cons(-(x, 1),emptylist),emptylist))) 
;(7) cdr(let x = 4 in  cons (x, cons (cons ( -(x, 1),  emptylist),  emptylist))) 
;(8) car(cdr(let x = 4 in cons(x, cons(cons (-(x,1),emptylist),emptylist))))


;============================================================
;Changes to expression datatype, value-of, and the-grammar: #
;============================================================

(define-datatype expression expression?
  (const-exp(num number?))
  (var-exp(var symbol?))
  (zero?-exp(exp1 expression?))
  (if-exp
      (exp1 expression?)
      (exp2 expression?)
      (exp3 expression?))
  (let-exp
      (var symbol?)
      (exp1 expression?)
      (body expression?))  
  (minus-exp
      (exp1 expression?)) 
  (diff-exp
       (exp1 expression?)
       (exp2 expression?))
  (add-exp
       (exp1 expression?)
       (exp2 expression?))
  (mul-exp
       (exp1 expression?)
       (exp2 expression?))
  (div-exp
       (exp1 expression?)
       (exp2 expression?))
  (equal?-exp
       (exp1 expression?)
       (exp2 expression?))
  (greater?-exp
       (exp1 expression?)
       (exp2 expression?))
  (less?-exp
       (exp1 expression?)
       (exp2 expression?))

  ;### ADDITIONS ###

  (cons-exp
       (exp1 expression?)
       (exp2 expression?))
  (car-exp
       (exp1 expression?))
  (cdr-exp
       (exp1 expression?))
  (null?-exp
       (exp1 expression?))
  (emptylist-exp)
  )
 

(define value-of
    (lambda (exp env)
      (cases expression exp
        (const-exp (num) (num-val num))
        (var-exp (var) (apply-env env var))
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
              (extend-env var val1 env))))       
        (minus-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (num-val (- num1)))))       
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
        (mul-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (* num1 num2)))))        
        (div-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (quotient num1 num2)))))
        (equal?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (= num1 num2)))))       
        (greater?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (> num1 num2)))))        
        (less?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (bool-val (< num1 num2)))))
        
        ;### ADDITIONS ###
        
        (cons-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
               (val2 (value-of exp2 env)))
          (cons-val val1 val2)))
        
        (car-exp (body)
          (expval->car (value-of body env)))

        (cdr-exp (body)
          (expval->cdr (value-of body env)))
     
        (null?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
          (let ((bool1 (expval->emptylist? val1)))
               (bool-val bool1))))
        
        (emptylist-exp ()
          (emptylist-val))
        )))


(define the-grammar
   '((program (expression) a-program)
     (expression (number) const-exp)      
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
     (expression
       ("minus" "(" expression ")") minus-exp)     
     (expression
       ("-" "(" expression "," expression ")") diff-exp)     
     (expression
       ("+" "(" expression "," expression ")") add-exp)    
     (expression
       ("*" "(" expression "," expression ")") mul-exp)     
     (expression
       ("/" "(" expression "," expression ")") div-exp)
     (expression
       ("equal?" "(" expression "," expression ")") equal?-exp)     
     (expression
       ("greater?" "(" expression "," expression ")") greater?-exp)     
     (expression
       ("less?" "(" expression "," expression ")") less?-exp)

     ;### ADDITIONS ###

     (expression
       ("cons" "(" expression "," expression ")") cons-exp)
     (expression
       ("car" "(" expression ")") car-exp)
     (expression
       ("cdr" "(" expression ")") cdr-exp)
     (expression
       ("null?" "(" expression ")") null?-exp)
     (expression
       ("emptylist") emptylist-exp)
      ))

;============================================================
;Changing define-dataype, adding car cdr and emptylist def: #
;============================================================

(define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (cons-val                             ;New!
      (first expval?) (rest expval?))
    (emptylist-val))                      ;Also new!

(define expval->car
  (lambda (val)
    (cases expval val
      (cons-val (first rest) first)
      (else (expval-extractor-error 'cons val)))))

(define expval->cdr
  (lambda (val)
    (cases expval val
      (cons-val (first rest) rest)
      (else (expval-extractor-error 'cons val)))))

(define expval->emptylist?
  (lambda (val)
    (cases expval val
      (emptylist-val () #t)
      (cons-val (first rest) #f)
      (else (expval-extractor-error 'cons-or-emptylist val)))))

;============================================================
;Test data that shows each of the added features working:   #
;============================================================


(run "cons ( 7,  emptylist)")
;(cons-val (num-val 7) (emptylist-val))

(run "cons ( -(x, 1),  emptylist)")
;(cons-val (num-val 9) (emptylist-val))

(run "cons (cons ( -(x, 1),  emptylist),emptylist)")
;(cons-val (cons-val (num-val 9) (emptylist-val)) (emptylist-val))

(run "cons (x, cons (cons ( -(x, 1),  emptylist),  emptylist))")
;(cons-val (num-val 10) (cons-val (cons-val (num-val 9) (emptylist-val)) (emptylist-val)))

(run "let x = 4 in  cons (x, cons (cons ( -(x, 1),  emptylist),  emptylist))")
;(cons-val (num-val 4) (cons-val (cons-val (num-val 3) (emptylist-val)) (emptylist-val)))

(run "car(let x = 4 in  cons (x, cons(cons(-(x, 1),emptylist),emptylist)))")
;(num-val 4)

(run "cdr(let x = 4 in  cons (x, cons (cons ( -(x, 1),  emptylist),  emptylist)))")
;(cons-val (cons-val (num-val 3) (emptylist-val)) (emptylist-val))

(run "car(cdr(let x = 4 in cons(x, cons(cons (-(x,1),emptylist),emptylist))))")
;(cons-val (num-val 3) (emptylist-val))


























