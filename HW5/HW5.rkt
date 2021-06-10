#lang racket

;Ethan Brinkman
;HW5: Due 3/6

;Total: 19.5/25

;Exercise 2.5
;Implement environments using a representation in which the empty environment is represented as the empty list,
;and in which extend-env builds an environment that looks like (diagram)

;4/5
;-1: 0 test cases provided, 2 required
(define empty-env
  (lambda ()
    '()))

;4/5
;-1: 0 test cases provided, 2 required
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

;3.5/5
;-1: 0 test cases provided, 2 required
;-.5: you've created a function that could report a no-binding, but this needs to be incorporated into apply-env
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car(car env)) search-var) (cdr(car env)))
      (#t(apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var env)
    (error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (error 'apply-env "Bad environment ~s" env)))

;Exercise 2.8
;Add to the environment interface an observer called empty-env? and implement it using the a-list representation

;I have no idea what the a-list representation means
;It is simply the style of environment you created in 2.5

;4/5
;-1: 0 test cases provided, 2 required
(define empty-env? null?)

;Exercise 2.9
;Add to the environment interface an observer called has-binding? that takes an environment env and a variable s
;and tests to see if s has an associated value in env. Implement with the a-list representation.

;4/5
;-1: 0 test cases provided, 2 required
(define (has-binding? env s)
  (cond
    ((empty-env? env) #f)
    ((eqv? s (caar env)) #t)
    (#t (has-binding? (cdr env) s))))