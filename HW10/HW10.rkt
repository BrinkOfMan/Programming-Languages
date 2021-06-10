#lang racket
(require(lib "eopl.ss" "eopl"))

(define-datatype program program?
 (a-program(expl1 expression?)))

(define-datatype expression expression?
 (const-exp(num number?))
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
 (var-exp(var symbol?))
 (let-exp
      (var symbol?)
      (exp1 expression?)
      (body expression?)))

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
;HW10: Due 4/17/2020

;"For hw10, you are to create 3 rather complicated let language expressions in BNF...
;Now for the homework, you need to [create]
;(1) an expression containing 4 different bnf expressions
;(2) an expression containing 5 different bnf expressions
;(3) an expression containing 7 different bnf expressions.
;Then use scare&parse to parse all of them."


;Exercise 1:
;An expression containing 4 different bnf expressions

;if zero? (x) then y else let x = -(y, x) in zero?(x)
;This contains a zero?-exp, if-exp, diff-exp, and let-exp
(scan&parse "if zero? (x) then y else let x = -(y, x) in zero?(x)")

;Exercise 2:
;An expression containing 5 different bnf expressions

;let x = 5 in if zero? (y) then y else -(x, +(x, 2))
;This ccontains a let-exp, if-exp, zero?-exp, diff-exp, and add-exp
(scan&parse "let x = 5 in if zero? (y) then y else -(x, +(x, 2))")

;Exercise 3:
;An expression containing 7 different bnf expressions

;if zero?(counter) then let x = 1 in +(total, x) else let x = 1 in zero? (-(timeleft, x))
;This contains an if-exp, zero?-exp, let-exp, add-exp, then another let-exp, zero?-exp, and a diff-exp
;(A really bad timer countdown program that checks if it is done now or will be done in the next tick)
(scan&parse "if zero?(counter) then let x = 1 in +(total, x) else let x = 1 in zero? (-(timeleft, x))")

