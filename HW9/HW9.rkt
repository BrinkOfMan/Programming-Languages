#lang racket
(require(lib "eopl.ss" "eopl")) ;Thanks, Jackson

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
    ((equal? (car datum) 'lambda)(lambda-exp
                                  (caadr datum)
                                  (parse-expression (caddr datum))))
    (#t (app-exp(parse-expression (car datum))
                (parse-expression (cadr datum)))))))

;Ethan Brinkman
;HW9: Due 4/13
;REMEMBER: Include 3 tests for 2.28!

;Exercise 2.27
;Draw the abstract syntax tree for the lambda calculus expressions

((lambda (a) (a b)) c)

;              app-exp
;              /     \
;      lambda-exp     var-exp
;       /      \         |
; bound-var    body      c
;     |         |
;     a      app-exp
;            /     \
;       var-exp   var-exp
;          |          |
;          a          b

(lambda (x)
  (lambda (y)
    ((lambda (x)
       (x y))
     x)))

;      lambda-exp
;      /       \
; bound-var    body
;     |         |
;     x     lambda-exp
;            /      \
;      bound-var    body
;          |         |
;          y      app-exp
;                 /     \
;        lambda-exp     var-exp
;         /      \         |
;   bound-var    body      x
;       |         |
;       x      app-exp
;              /     \
;         var-exp   var-exp
;            |          |
;            x          y

;Exercise 2.28
;Write an unparser that converts the abstract syntax of an lc-exp into a string that matches the second grammar in this section (page 52)

(define unparse
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'lambda (list bound-var)(unparse body)))
      (app-exp (rator rand)
               (list (unparse rator) (unparse rand))))))


;Testing with manual entry of an unparsable expression:
(unparse (var-exp 'x))
;'x
(unparse (app-exp (app-exp (var-exp 'b) (var-exp 'c)) (lambda-exp 'x (var-exp 'c))))
;'((b c) (lambda (x) c))
(unparse (lambda-exp 'x (lambda-exp 'y (app-exp (var-exp 'z) (lambda-exp 'a (var-exp 'c))))))
;'(lambda (x) (lambda (y) (z (lambda (a) c))))

;Testing with inputting a parse-expression:
(unparse (parse-expression 'x))
;'x
(unparse (parse-expression '((b c) (lambda (x) c))))
;'((b c) (lambda (x) c))
(unparse (parse-expression '(lambda (x) (lambda (y) (z (lambda (a) c))))))
;'(lambda (x) (lambda (y) (z (lambda (a) c))))
