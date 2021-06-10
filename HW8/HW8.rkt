#lang racket
(require(lib "eopl.ss" "eopl"))

;Ethan Brinkman
;HW8: Due 4/10

;Exercise 1:
;Draw the syntax trees for the following two lc-exp expressions:
;((b c) (lambda (x) c))
;(lambda (x) (lambda (y) (z (lambda (a) c))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;Exercise 2:
;Using the define-datatype interface evaluate and send the internal DrRacket list structures for the two expressions


(app-exp (app-exp (var-exp 'b) (var-exp 'c)) (lambda-exp 'x (var-exp 'c)))
(lambda-exp 'x (lambda-exp 'y (app-exp (var-exp 'z) (lambda-exp 'a (var-exp 'c)))))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
    ((equal? (car datum) 'lambda)(lambda-exp
                                  (caadr datum)
                                  (parse-expression (caddr datum))))
    (#t (app-exp(parse-expression (car datum))
                (parse-expression (cadr datum)))))))

;Exercise 3:
;using the parse-expression code evaluate and send the internal DrRacket list structures for the two expressions
;(ie, do the same as in 2 but with the parse-expression function)

(parse-expression '((b c) (lambda (x) c)))
(parse-expression '(lambda (x) (lambda (y) (z (lambda (a) c)))))

