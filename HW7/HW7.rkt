#lang racket
(require(lib "eopl.ss" "eopl")) ;Thanks, Jackson

;Ethan Brinkman
;HW7: Due 3/16

;Total: 19.5/20

;Exercise 2.15
;Implement the lambda-calculus expression interface for the representation specified by the grammar

;.5/1 for constructors
;var-exp and app-exp should include in the return list the labels 'var-exp or 'app-exp
(define var-exp
  (lambda (var) var))

(var-exp 'a)
;> 'a
(var-exp 'b)
;> 'b

(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda (list bound-var) body)))

(lambda-exp 'a 5)
;> '(lambda (a) 5)
(lambda-exp 'b 2)
;> '(lambda (b) 2)

(define app-exp
  (lambda (exp1 exp2)
    (list exp1 exp2)))

(app-exp '(lambda (a) 5) '(lambda (b) 2))
;> '((lambda (a) 5) (lambda (b) 2))
(app-exp '(lambda (x) 3) '(lambda (y) 1))
;> '((lambda (x) 3) (lambda (y) 1))

;1/1 for predicates
;with the above change to the constructors, the predicates can simply check if the first thing in the list is the proper label
(define var-exp?
  (lambda (exp)
    (symbol? exp)))

(var-exp? 'a)
;> #t
(var-exp? 3)
;> #f

(define lambda-exp?
  (lambda (exp)
    (and (pair? exp) (eqv? 'lambda (car exp)))))

(lambda-exp? '(lambda (a) 5))
;> #t
(lambda-exp? '(Hello! I'm not a lambda expression. Remember to wash your hands.))
;> #f

(define app-exp?
  (lambda (exp)
    (and (pair? exp) (pair? (cdr exp) (null? (cddr exp))))))

;(app-exp? '((lambda (a) 5) (lambda (b) 2))) I must be testing this wrong or misunderstanding it, as the test doesn't work

;2/2 for extractors
(define var-exp->var
  (lambda (exp)
    exp))

(define lambda-exp->bound-var
  (lambda (exp)
    (caadr exp)))

(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

(define app-exp->rator
  (lambda (exp)
    (car exp)))

(define app-exp->rand
  (lambda (exp)
    (cadr exp)))

;Exercise 2.21
;Implement the data type of environments, as in section 2.2.2, using define-datatype.
;Then include has-binding? of exercise 2.9
;4/4
(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var var?)
   (saved-val scheme-value?)
   (saved-env env?)))

(define var? symbol?)
(define scheme-value? (lambda (s) #t))
;4/4
(define apply-env
  (lambda (environment search-var)
    (cases env environment
      (empty-env ()
                 (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? search-var saved-var)
                      saved-val
                      (apply-env saved-env search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
;4/4
(define has-binding?
  (lambda (environment search-var)
    (cases env environment
      (empty-env ()
                 #f)
      (extend-env (saved-var saved-val saved-env)
                  (or (eqv? search-var saved-var)
                      (has-binding? saved-env search-var))))))


;Exercise 2.24
;Implement a bintree-to-list procedure for binary trees, so that
;(bintree-to-list (interior-node 'a (leaf node 3) (leaf-node 4))) returns the list
;(interior-node
;  a
;   (leaf-node 3)
;   (leaf-node 4))

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
;4/4
(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num)
                 (list 'leaf-node
                       num))
      (interior-node (key left right)
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right))))))

(bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
;> '(interior-node a (leaf-node 3) (leaf-node 4))
;I think that's what we want

(bintree-to-list (interior-node 'a (leaf-node 2) (leaf-node 3)))
;> '(interior-node a (leaf-node 2) (leaf-node 3))