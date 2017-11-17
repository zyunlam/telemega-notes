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


(defun caar (l) (car (car l)))

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

(define zero? (macro (value rest)
		     (list
		      eq?
		      value
		      0)
		     )
  )

(zero? 1)
(zero? 0)
(zero? "hello")

(define positive? (macro (value rest)
			 (list
			  >
			  value
			  0)
			 )
  )

(positive? 12)
(positive? -12)

(define negative? (macro (value rest)
			 (list
			  <
			  value
			  0)
			 )
  )

(negative? 12)
(negative? -12)

(defun abs (x) (cond ((>= x 0) x)
		     (else (- x)))
       )

(abs 12)
(abs -12)

(define max (lexpr (first rest)
		   (while (not (null? rest))
		     (cond ((< first (car rest))
			    (set! first (car rest)))
			   )
		     (set! rest (cdr rest))
		     )
		   first)
  )

(max 1 2 3)
(max 3 2 1)

(define min (lexpr (first rest)
		   (while (not (null? rest))
		     (cond ((> first (car rest))
			    (set! first (car rest)))
			   )
		     (set! rest (cdr rest))
		     )
		   first)
  )

(min 1 2 3)
(min 3 2 1)

(defun even? (x) (zero? (% x 2)))

(even? 2)
(even? -2)
(even? 3)
(even? -1)

(defun odd? (x) (not (even? x)))

(odd? 2)
(odd? -2)
(odd? 3)
(odd? -1)

(define exact? number?)
(defun inexact? (x) #f)

					; (if <condition> <if-true>)
					; (if <condition> <if-true> <if-false)

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


(define append (lexpr (args)
		      (let ((append-list (lambda (a b)
					   (cond ((null? a) b)
						 (else (cons (car a) (append-list (cdr a) b)))
						 )
					   )
					 )
			    (append-lists (lambda (lists)
					    (cond ((null? lists) lists)
						  ((null? (cdr lists)) (car lists))
						  (else (append-list (car lists) (append-lists (cdr lists))))
						  )
					    )
					  )
			    )
			(append-lists args)
			)
		      )
  )

(append '(a b c) '(d e f) '(g h i))

(defun reverse (list)
  (let ((result ()))
    (while (not (null? list))
      (set! result (cons (car list) result))
      (set! list (cdr list))
      )
    result)
  )

(reverse '(1 2 3))

(define list-tail
  (lambda (x k)
    (if (zero? k)
	x
      (list-tail (cdr x) (- k 1)))))

(list-tail '(1 2 3) 2)

(defun list-ref (x k) (car (list-tail x k)))

(list-ref '(1 2 3) 2)

    
					; recursive equality

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

(defun _member (obj list test?)
  (if (null? list)
      #f
    (if (test? obj (car list))
	list
      (memq obj (cdr list)))))

(defun memq (obj list) (_member obj list eq?))

(memq 2 '(1 2 3))

(memq 4 '(1 2 3))

(defun memv (obj list) (_member obj list eqv?))

(memv 2 '(1 2 3))

(memv 4 '(1 2 3))

(defun member (obj list) (_member obj list equal?))

(member '(2) '((1) (2) (3)))

(member '(4) '((1) (2) (3)))

(defun _assoc (obj list test?)
  (if (null? list)
      #f
    (if (test? obj (caar list))
	(car list)
      (_assoc obj (cdr list) test?)
      )
    )
  )

(defun assq (obj list) (_assoc obj list eq?))
(defun assv (obj list) (_assoc obj list eqv?))
(defun assoc (obj list) (_assoc obj list equal?))

(assq 'a '((a 1) (b 2) (c 3)))
(assv 'b '((a 1) (b 2) (c 3)))
(assoc '(c) '((a 1) (b 2) ((c) 3)))

(define char? integer?)

(char? #\q)
(char? "h")

(defun char-upper-case? (c) (<= #\A c #\Z))

(char-upper-case? #\a)
(char-upper-case? #\B)
(char-upper-case? #\0)
(char-upper-case? #\space)

(defun char-lower-case? (c) (<= #\a c #\a))

(char-lower-case? #\a)
(char-lower-case? #\B)
(char-lower-case? #\0)
(char-lower-case? #\space)

(defun char-alphabetic? (c) (or (char-upper-case? c) (char-lower-case? c)))

(char-alphabetic? #\a)
(char-alphabetic? #\B)
(char-alphabetic? #\0)
(char-alphabetic? #\space)

(defun char-numeric? (c) (<= #\0 c #\9))

(char-numeric? #\a)
(char-numeric? #\B)
(char-numeric? #\0)
(char-numeric? #\space)

(defun char-whitespace? (c) (or (<= #\tab c #\return) (= #\space c)))

(char-whitespace? #\a)
(char-whitespace? #\B)
(char-whitespace? #\0)
(char-whitespace? #\space)

(defun char->integer (c) c)
(defun integer->char (c) char-integer)

(defun char-upcase (c) (if (char-lower-case? c) (+ c (- #\A #\a)) c))

(char-upcase #\a)
(char-upcase #\B)
(char-upcase #\0)
(char-upcase #\space)

(defun char-downcase (c) (if (char-upper-case? c) (+ c (- #\a #\A)) c))

(char-downcase #\a)
(char-downcase #\B)
(char-downcase #\0)
(char-downcase #\space)

(define string (lexpr (chars) (list->string chars)))

;(define number->string (lexpr (arg opt)
;			      (let ((base (if (null? opt) 10 (car opt)))
					;
;
				
