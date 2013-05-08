;==============================================================================;
;        Computer Science 1901 - Spring 2010 - University of Minnesota         ;
;                             Final Course Project                             ;
;==============================================================================;
;  Utility Functions                                                           ;
; -------------------                                                          ;
;  Notes:                                                                      ;
;   + This file is loaded by gomoku++.scm.                                       ;
;   + Your player-procedure has access to these procedures through the top     ;
;      level environment.  There is no need to copy them in to your file.      ;
;   + Do not submit this file.                                                 ;
;==============================================================================;


;;;==============================;;;
;;;  General Utility Procedures  ;;;
;;;==============================;;;

;; Most Scheme implementations define these.
(define (1+ x)  (+ x 1))
(define (-1+ x) (- x 1))

;; Evaluates thunk n times.
(define (dotimes thunk n)
  (for-each (lambda (i) (thunk)) (iota n)))

;; Is n in the range a..b?  (default: inclusive/inclusive)
;; Arguments:  a (start of range), b (end of range), n (test value)
;; May specify if range is inclusive or exclusive:
;;   #t -- both inclusive
;;   #f -- both inclusive
;;   #f #t -- a exclusive, b inclusive
;;   #t #f -- a inclusive, b exclusive
(define (in-range? a b n . inclusive)
  (let ((inclusive
   (cond ((null? inclusive) (list #t #t))
	       (else (list (first inclusive) (last inclusive))))))
    (or (< a n b)
	(and (car inclusive) (= a n))
	(and (cadr inclusive) (= b n)))))


;;;======================;;;
;;;  Display Procedures  ;;;
;;;======================;;;

;; Display any number of items.  No spaces or line breaks inserted.
(define (display+ . args) 
  (for-each display args))


;;;==============================;;;
;;;  List Processing Procedures  ;;;
;;;==============================;;;

;; Counts instance of x in lst with eq?
(define (count x lst)
  (length (filter (lambda (el) (eq? el x)) lst)))

;; First n elements of lst
(define (take lst n)
  (if (= n 0) '() (cons (car lst) (take (cdr lst) (- n 1)))))

;; All elements after n of lst
(define (leave lst n)
  (if (<= n 0) lst (leave (cdr lst) (- n 1))))

;; Do any elements of list satisify predicate?
(define (any? predicate? lst)
  (and (not (null? lst)) 
       (or (predicate? (car lst)) 
	   (any? predicate? (cdr lst)))))

;; Do all elements of list satisify predicate?
(define (all? predicate? lst)
  (or (null? lst) 
      (and  (predicate? (car lst))
	    (all? predicate? (cdr lst)))))

;; Returns a random element of the given list
(define (random-element lst)
  (list-ref lst (random (length lst))))

;; Standard filter procedure, as defined in SRFI1
(define (filter predicate? lst)
  (apply append (map (lambda (x) (if (predicate? x) (list x) '())) lst)))

;; Build a list containing n instances of x
(define (repeat x n)
  (if (= n 0) '() (cons (tree-copy x) (repeat x (- n 1)))))

;; Common Lisp style iota
;;  examples: (iota n)   => (0 1 2 ... n)
;;            (iota n m) => (n n+1 n+2 ... m)
(define (iota . args)
  (define (iter lower upper)
    (cond ((= lower upper) '())
	  (else (cons lower (iter (1+ lower) upper)))))
  (if (null? (cdr args))
      (iter 0 (car args))
      (iter (car args) (cadr args))))

;; Haskell style concat-map, f should take an element and return a list, 
;;   these lists are then merged into a single list.
(define (concat-map f lst)
  (apply append (map f lst)))

;; Haskell style zip-with
;;  examples: (zip-with + (1 2 3) (1 5 9)) => (2 7 12)
;;            (zip-with  (1 0) (1 1 0 1) (0 0 1)) => (2 1)
(define (zip-with proc . lsts)
  (let ((min-len (apply min (map length lsts))))
    (apply map (cons proc (map (lambda (lst) (take lst min-len)) lsts)))))

;; Flattens an arbitarily nested list
(define (enumerate-tree tree)
  (if (list? tree) 
      (apply append (map enumerate-tree tree)) 
      (list tree)))

;; Allocates a new copy of a tree so that the original cannot be modified.
;; Not defined in Dr Scheme
(define (tree-copy tree)
    (if (not (pair? tree))
        tree
        (cons (tree-copy (car tree))
              (tree-copy (cdr tree)))))

;;;======================;;;
;;;  Tagging Procedures  ;;;
;;;======================;;;
;;; From Page 176 of Text

;; Returns a pair containing type-tag and contents
(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; Get the tag.
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad typed datum -- TYPE")))

;; Get the data contents.
(define (contents datum)
  (if (pair? datum)
      (cdr datum)        
      (error "Bad typed datum -- CONTENTS")))
