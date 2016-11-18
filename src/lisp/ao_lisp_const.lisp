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

(setq def (macro (name val rest)
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

(def defun (macro (name args exprs)
		  (list
		   def
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
					; (let ((x 1) (y)) (setq y (+ x 1)) y)

(def let (macro (vars exprs)
		((lambda (make-names make-exprs make-nils)

					;
					; make the list of names in the let
					;

		   (setq make-names (lambda (vars)
				      (cond (vars
					     (cons (car (car vars))
						   (make-names (cdr vars))))
					    )
				      )
			 )

					; the set of expressions is
					; the list of set expressions
					; pre-pended to the
					; expressions to evaluate

		   (setq make-exprs (lambda (vars exprs)
				      (cond (vars (cons
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

		   (setq make-nils (lambda (vars)
				     (cond (vars (cons nil (make-nils (cdr vars))))
					   )
				     )
			 )
					; prepend the set operations
					; to the expressions

		   (setq exprs (make-exprs vars exprs))

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

					; boolean operators

(def or (lexpr (l)
	       (let ((ret nil))
		 (while l
		   (cond ((setq ret (car l))
			  (setq l nil))
			 ((setq l (cdr l)))))
		 ret
		 )
	       )
     )

					; execute to resolve macros

(or nil t)

(def and (lexpr (l)
	       (let ((ret t))
		 (while l
		   (cond ((setq ret (car l))
			  (setq l (cdr l)))
			 ((setq ret (setq l nil)))
			 )
		   )
		 ret
		 )
	       )
     )

					; execute to resolve macros

(and t nil)
