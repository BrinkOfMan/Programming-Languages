#lang racket
(require(lib "eopl.ss" "eopl"))

;============================================================
;All the definitions & stuff to make sure everything works: #
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


(define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (cons-val                         
      (first expval?) (rest expval?))
    (emptylist-val))                     

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
;HW16: Due 5/4

;Provide all the trace details for the following two BNF let language expressions:

;cdr(cons(6, cons(4, emptylist)))
(run "cdr(cons(6, cons(4, emptylist)))")
(value-of-program (scan&parse "cdr(cons(6, cons(4, emptylist)))"))
(value-of-program (a-program (cdr-exp (cons-exp (const-exp 6) (cons-exp (const-exp 4) (emptylist-exp))))))
(value-of (cdr-exp (cons-exp (const-exp 6) (cons-exp (const-exp 4) (emptylist-exp)))) init-env)
;    expval -> (cdr(vo body ie)) =
;              (cdr(vo(cons-exp(const-exp 6) (cons-exp (const-exp 4) (emptylist-exp))) ie))
;              val1 -> (vo exp1 ie) =
;                      (vo (const-exp 6) ie) =
;                      (num-val 6)
;              val2 -> (vo exp2 ie) =
;                      (vo (cons-exp (const-exp 4) (emptylist-exp)) ie)
;                      val21 -> (vo exp21 ie) =
;                               (vo (const-exp 4) ie) =
;                               (num-val 4)
;                      val22 -> (vo exp22 ie) =
;                               (vo (emptylist-exp) ie) =
;                               (emptylist-val)
;                        (cons-val (num-val 4) (emptylist-val))
;                (cons-val (num-val 6) (cons-val (num-val 4) (emptylist-val)))
;        (cdr(cons-val (num-val 6) (cons-val (num-val 4) (emptylist-val))))
;        (cons-val (num-val 4) (emptylist-val))



;cons (cons ( -(x, 1),  emptylist ),  emptylist )
(run "cons (cons ( -(x, 1),  emptylist ),  emptylist )")
(value-of-program (scan&parse "cons (cons ( -(x, 1),  emptylist ),  emptylist )"))
(value-of-program (a-program (cons-exp (cons-exp (diff-exp (var-exp 'x) (const-exp 1)) (emptylist-exp)) (emptylist-exp))))

;The value-of gives me this error:
;"cases: not a environment: #<procedure:init-env>"

;(value-of (cons-exp (cons-exp (diff-exp (var-exp 'x) (const-exp 1)) (emptylist-exp)) (emptylist-exp)) init-env) 
;     val1 -> (vo exp1 ie) =
;             (vo (cons-exp (diff-exp (var-exp 'x) (const-exp 1)) (emptylist-exp)) ie)
;             val11 -> (vo exp11 ie) =
;                      (vo (diff-exp (var-exp 'x) (const-exp 1)))
;                      val111 -> (vo exp111 ie) =
;				 (vo (var-exp ‘x) ie)
;			 	 (apply-env ie ‘x) =
;			         (num-val 10)
;		       val112 -> (vo exp112 ie) =
;			         (vo (const-exp 1) ie)
;			         (num-val 1)
;		       num111 -> (expval->num val111) =
;			         (expval->num (num-val 10) =
;				      10
;		       num112 -> (expval->num val112) =
;			         (expval->num (num-val 1) =
;			              1
;			 (num-val (- num11 num12) =
;			 (num-val (- 10 1)) =
;			 (num-val 9)
;             val12 -> (vo exp12 ie) =
;                      (vo (emptylist-exp) ie) =
;                      (emptylist-val)
;                 (cons-val (num-val 9) (emptylist-val))
;     val2 -> (vo exp2 ie) =
;             (vo emptylist-exp ie) =
;             (emptylist-val)
;        (cons-val (cons-val (num-val 9) (emptylist-val)) (emptylist-val))



;=========================================================================================================================================























