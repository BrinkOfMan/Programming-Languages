#lang racket

;Ethan Brinkman
;HW6: Due 3/9

;Exercise 2.10
;Add to the environment interface a constructor extend-env*, and implement it using the a-list representation
;This constructor takes a list of variables, a list of values of the same length, and an environment,
;and is specifiec by (extend-env* (var1 ... vark) (val1 ... valk) [f]) = [g],
;where g(var) = vali if var=vari for some i such that 1 <= i <= k, f(var) otherwise

;You forgot to define extend-env itself. 
;6/10
(define extend-env*
  (lambda(vars vals env)
    (cond
      ((null? vars) env);Check to see if no variables (assuming vals list is the same length and will also be null) 
      (#t(extend-env* (cdr vars) (cdr vals) (extend-env (car vars) (car vals) env)))))) ;Otherwise, recur

;Exercise 2.11
;last exercise's extend-inv* is logarithmic (probably). It is possible to represent environments so that extend-env* requires only constant time:
;represent the empty environment by the empty list, and represent a non-empty environment by the data structure (diagram)
;such an environment might look like (diagram), this is caled the ribcage representation. The environment is represented as a list of pairs,
;called ribs; each left rib is a list of variables and each right rib is the corresponding list of values
;implement the environment interface, including extend-env* in this representation


;(Environment interface includes addition member, list-index, h-list-index, list-ref elements (+1 for each element)
;16/20

(define (empty-env) '())

;renaming extend-env* to extend-env** to avoid compiler errors
(define extend-env**
  (lambda (vars vals env)
  (cons (cons vars vals) env)))

(define extend-env
  (lambda (var val env)
  (extend-env** (list var) (list val) env)))

(define apply-env
  (lambda (env search-var)
  (cond
    ((null? env)(report-no-binding-found search-var)) ;Check for no environment
    (#t
     (let apply-ribs ((ribs (car env))) ;Create ribs from the environment
       (let ((vars (car ribs))(vals (cdr ribs))) ;Left rib is the car (vars), right rib is the cdr (vals)
         (cond
           ((null? vars)(apply-env (cdr env) search-var)) ;If no var, recur with the next element in env
           ((eqv? (car vars) search-var)(car vals)) ;If car of vars = search-var, return car vals
           (#t(apply-ribs (cons (cdr vars) (cdr vals))))))))))) ;Otherwise, recur with a list of the next var and val

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))