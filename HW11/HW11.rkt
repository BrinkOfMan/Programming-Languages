#lang racket
(require(lib "eopl.ss" "eopl")) ;Thanks, Jackson

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
;HW11: Due 4/20

;Draw the parse/syntax trees for the following 3 programs in the let language:

;+(x, 3)
(scan&parse "+(x, 3)")
;(a-program (add-exp (var-exp 'x) (const-exp 3)))

;              a-program
;                  |
;                expl1
;                  |
;               add-exp
;              /       \
;          var-exp  const-exp
;             |         |
;            var       num
;             |         |
;             x         3


;let y = 4 in -(x, y)
(scan&parse "let y = 4 in -(x, y)")
;(a-program (let-exp 'y (const-exp 4) (diff-exp (var-exp 'x) (var-exp 'y))))

;              a-program
;                  |
;                expl1
;                  |
;               let-exp
;             /    |    \
;           var   exp1  body
;           |      |       |
;           y  const-exp diff-exp
;                  |       |    \
;                 num     exp1   exp2
;                  |       |      |
;                  4    var-exp  var-exp
;                          |      |
;                         var    var
;                          |      |
;                          x      y

;if zero?(x) then 4 else y
(scan&parse "if zero?(x) then 4 else y")
;(a-program (if-exp (zero?-exp (var-exp 'x)) (const-exp 4) (var-exp 'y)))

;              a-program
;                  |
;                expl1
;                  |
;                if-exp
;              /   |   \
;           exp1  exp2  exp3
;          /       |       \
;    zero?-exp cosnt-exp var-exp
;         |        |        |
;        exp1     num      var
;         |        |        |
;      var-exp     4        y
;         |      
;        var
;         |
;         x 








