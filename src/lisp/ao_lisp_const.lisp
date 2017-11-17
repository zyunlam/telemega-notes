;
; Copyright © 2016 Keith Packard <keithp@keithp.com>
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; Lisp code placed in ROM

					; return a list containing all of the arguments

(set (quote list) (lexpr (l) l))

					;
					; Define a variable without returning the value
					; Useful when defining functions to avoid
					; having lots of output generated
					;

(set (quote define) (macro (name val rest)
			(list
			 'progn
			 (list
			  'set
			  (list 'quote name)
			  val)
			 (list 'quote name)
			 )
			)
     )

					;
					; A slightly more convenient form
					; for defining lambdas.
					;
					; (defun <name> (<params>) s-exprs)
					;

(define defun (macro (name args exprs)
		  (list
		   define
		   name
		   (cons 'lambda (cons args exprs))
		   )
		  )
     )

					; basic list accessors


(defun cadr (l) (car (cdr l)))

(defun caddr (l) (car (cdr (cdr l))))

(defun nth (list n)
  (cond ((= n 0) (car list))
	((nth (cdr list) (1- n)))
	)
  )

					; simple math operators

(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))

(define if (macro (test args)
	       (cond ((null? (cdr args))
		      (list
		       cond
		       (list test (car args)))
		      )
		     (else
		      (list
		       cond
		       (list test (car args))
		       (list 'else (cadr args))
		       )
		      )
		     )
	       )
     )

(if (> 3 2) 'yes)
(if (> 3 2) 'yes 'no)
(if (> 2 3) 'no 'yes)
(if (> 2 3) 'no)

					; define a set of local
					; variables and then evaluate
					; a list of sexprs
					;
					; (let (var-defines) sexprs)
					;
					; where var-defines are either
					;
					; (name value)
					;
					; or
					;
					; (name)
					;
					; e.g.
					;
					; (let ((x 1) (y)) (set! y (+ x 1)) y)

(define let (macro (vars exprs)
		((lambda (make-names make-exprs make-nils)

					;
					; make the list of names in the let
					;

		   (set! make-names (lambda (vars)
				      (cond ((not (null? vars))
					     (cons (car (car vars))
						   (make-names (cdr vars))))
					    )
				      )
			 )

					; the set of expressions is
					; the list of set expressions
					; pre-pended to the
					; expressions to evaluate

		   (set! make-exprs (lambda (vars exprs)
				      (cond ((not (null? vars)) (cons
						   (list set
							 (list quote
							       (car (car vars))
							       )
							 (cadr (car vars))
							 )
						   (make-exprs (cdr vars) exprs)
						   )
						  )
					    (exprs)
					    )
				      )
			 )

					; the parameters to the lambda is a list
					; of nils of the right length

		   (set! make-nils (lambda (vars)
				     (cond ((not (null? vars)) (cons () (make-nils (cdr vars))))
					   )
				     )
			 )
					; prepend the set operations
					; to the expressions

		   (set! exprs (make-exprs vars exprs))

					; build the lambda.

		   (cons (cons 'lambda (cons (make-names vars) exprs))
			 (make-nils vars)
			 )
		   )
		 ()
		 ()
		 ()
		 )
		)
     )

(let ((x 1)) x)

					; boolean operators

(define or (lexpr (l)
	       (let ((ret #f))
		 (while (not (null? l))
		   (cond ((car l) (set! ret #t) (set! l ()))
			 ((set! l (cdr l)))))
		 ret
		 )
	       )
     )

					; execute to resolve macros

(or #f #t)

(define and (lexpr (l)
	       (let ((ret #t))
		 (while (not (null? l))
		   (cond ((car l)
			  (set! l (cdr l)))
			 (#t
			  (set! ret #f)
			  (set! l ()))
			 )
		   )
		 ret
		 )
	       )
     )

					; execute to resolve macros

(and #t #f)

(defun equal? (a b)
  (cond ((eq? a b) #t)
	((and (pair? a) (pair? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b)))
	 )
	(else #f)
	)
  )

(equal? '(a b c) '(a b c))
(equal? '(a b c) '(a b b))
