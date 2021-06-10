#lang racket
;Score 31/35
;Ethan Brinkman
;HW4: Due 3/2
;NOTE: You are not allowed to use the map funciton, despite it making things easier.

;Exercise 27
;(flatten slist) returns a list of the symbols contained in slist in the order in which they occur when slist is printed.
;Intuitively, flatten removes all the inner parentheses from its argument
;7/7
(define flatten
  (lambda (slist)
    (cond
      ((null? slist) '()) ;Nothing to add
      ((symbol? (car slist)) (cons (car slist) (flatten (cdr slist)))) ;If a symbol, add current car and flattened cdr together
      (#t (append (flatten (car slist)) (flatten (cdr slist))))))) ;Otherwise, add a flattened car and flattened cdr together
(flatten '(a b c))
;>'(a b c)
(flatten '((a) () (b ()) () (c)))
;>'(a b c)
(flatten '((a b) c (((d)) e)))
;>'(a b c d e)
(flatten '(a b (() (c))))
;>'(a b c)

;Exercise 29
;7/7 (add test more test runs next time)
;(sort loi) returns a list of elements of loi in ascending order

;I'm not sure why section A got assigned 27 and NOT 28, as we essentially need the funciton from 28 to make this possible
(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2) ;Only list two
      ((null? loi2) loi1) ;Only list one
      ((< (car loi1) (car loi2)) (cons (car loi1)(merge (cdr loi1) loi2))) ;If car1 < car2, cons car1 then car2
      (#t(cons (car loi2) (merge loi1 (cdr loi2))))))) ;Otherwise, cons car2 then car1

(define create-list ;Since we aren't allowed to use "map list loi", I am creating this
  (lambda (loi)
    (cond
      ((null? loi) '())
      (#t(cons(list(car loi))(create-list(cdr loi)))))))

(define sort
  (lambda (loi)
    (define merge-sort
      (lambda (lst)
        (cond
          ((null? lst) '()) ;Nothing to sort
          ((null? (cdr lst)) lst) ;Just one item
          (#t (merge-sort (cons (merge (car lst) (cadr lst))(merge-sort (cddr lst))))))));Create a list of sorted list of integers after sorting and merging them
      (car (merge-sort (create-list loi)))));Return just the car (which is a list) of the resulting merged-and-sorted list

(sort '(8 2 5 2 3))
;>(2 2 3 5 8)

;Exercise 30
;7/7
;(sort/predicate pred loi) returns a list of elements sorted by the predicate

(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond
      ((null? loi1) loi2) ;Only list two
      ((null? loi2) loi1) ;Only list one
      ((pred (car loi1) (car loi2)) (cons (car loi1)(merge/predicate pred (cdr loi1) loi2))) ;If car1 [PRED] car2, cons car1 then car2
      (#t(cons (car loi2) (merge/predicate pred loi1 (cdr loi2))))))) ;Otherwise, cons car2 then car1

(define sort/predicate
  (lambda (pred loi)
    (define merge-sort
      (lambda (lst)
        (cond
          ((null? lst) '()) ;Nothing to sort
          ((null? (cdr lst)) lst) ;Just one item
          (#t (merge-sort (cons (merge/predicate pred (car lst) (cadr lst))(merge-sort (cddr lst))))))));Create a list of sorted list of integers after sorting and merging them
      (car (merge-sort (create-list loi)))));Return just the car (which is a list) of the resulting merged-and-sorted list


(sort/predicate < '(8 2 5 2 3))
;>(2 2 3 5 8)
(sort/predicate > '(8 2 5 2 3))
;>(8 5 3 2 2)

;Exercise 31
;5/7 (test runs)
;Write the following procedures for calculating on a bintree (by def 1.17):
;leaf and interior-node, which build bintrees
;leaf? which tests whether a bintree is a leaf
;lson, rson, and contents-of, which extract the components of a node.
;contents-of should work on both leaves and interior nodes

(define (leaf n) n)

(define (interior-node name lson rson)
  (list name lson rson))

(define (leaf? tree)
  (number? tree))

(define lson cadr)

(define rson caddr)

(define (contents-of tree)
  (cond ((leaf? tree) tree)
        (else (car tree))))

;Exercise 32
;5/7 (test runs)
;Write a procedure double-tree that takes a bintree (by def 1.17),
;and produces another bintree like the original, but with all the integers in the leaves doubled

(define (double-tree tree)
  (cond
    ((leaf? tree)(leaf (* 2 (contents-of tree))))
    (#t(interior-node (contents-of tree)
                         (double-tree (lson tree))
                         (double-tree (rson tree))))))