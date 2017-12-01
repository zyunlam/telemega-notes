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

(set (quote set!)
     (macro (name value rest)
	    (list
	     set
	     (list
	      quote
	      name)
	     value)
	    )
     )

(set! append
     (lexpr (args)
	    ((lambda (append-list append-lists)
	       (set! append-list
		    (lambda (a b)
		      (cond ((null? a) b)
			    (else (cons (car a) (append-list (cdr a) b)))
			    )
		      )
		    )
	       (set! append-lists
		    (lambda (lists)
		      (cond ((null? lists) lists)
			    ((null? (cdr lists)) (car lists))
			    (else (append-list (car lists) (append-lists (cdr lists))))
			    )
		      )
		    )
	       (append-lists args)
	       ) () ())
	    )
     )

(append '(a b c) '(d e f) '(g h i))

					; boolean operators

(set! or
     (macro (l)
	    ((lambda (_or)
	       (set! _or
		    (lambda (l)
		      (cond ((null? l) #f)
			    ((null? (cdr l))
			     (car l))
			    (else
			     (list
			      cond
			      (list
			       (car l))
			      (list
			       'else
			       (_or (cdr l))
			       )
			      )
			     )
			    )
		      )
		    )
	       (_or l)) ())))

					; execute to resolve macros

(or #f #t)


(set! and
     (macro (l)
	    ((lambda (_and)
	       (set! _and
		    (lambda (l)
		      (cond ((null? l) #t)
			    ((null? (cdr l))
			     (car l))
			    (else
			     (list
			      cond
			      (list
			       (car l)
			       (_and (cdr l))
			       )
			      )
			     )
			    )
		      )
		    )
	       (_and l)) ())
	    )
     )


					; execute to resolve macros

(and #t #f)

(set! quasiquote
  (macro (x rest)
	 ((lambda (constant? combine-skeletons expand-quasiquote)
	    (set! constant?
					; A constant value is either a pair starting with quote,
					; or anything which is neither a pair nor a symbol

		 (lambda (exp)
		   (cond ((pair? exp)
			  (eq? (car exp) 'quote)
			  )
			 (else
			  (not (symbol? exp))
			  )
			 )
		   )
		 )
	    (set! combine-skeletons
		 (lambda (left right exp)
		   (cond
		    ((and (constant? left) (constant? right)) 
		     (cond ((and (eqv? (eval left) (car exp))
				 (eqv? (eval right) (cdr exp)))
			    (list 'quote exp)
			    )
			   (else
			    (list 'quote (cons (eval left) (eval right)))
			    )
			   )
		     )
		    ((null? right)
		     (list 'list left)
		     )
		    ((and (pair? right) (eq? (car right) 'list))
		     (cons 'list (cons left (cdr right)))
		     )
		    (else
		     (list 'cons left right)
		     )
		    )
		   )
		 )

	    (set! expand-quasiquote
		 (lambda (exp nesting)
		   (cond

					; non cons -- constants
					; themselves, others are
					; quoted

		    ((not (pair? exp)) 
		     (cond ((constant? exp)
			    exp
			    )
			   (else
			    (list 'quote exp)
			    )
			   )
		     )

					; check for an unquote exp and
					; add the param unquoted

		    ((and (eq? (car exp) 'unquote) (= (length exp) 2))
		     (cond ((= nesting 0)
			    (car (cdr exp))
			    )
			   (else
			    (combine-skeletons ''unquote 
					       (expand-quasiquote (cdr exp) (- nesting 1))
					       exp))
			   )
		     )

					; nested quasi-quote --
					; construct the right
					; expression

		    ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
		     (combine-skeletons ''quasiquote 
					(expand-quasiquote (cdr exp) (+ nesting 1))
					exp))

					; check for an
					; unquote-splicing member,
					; compute the expansion of the
					; value and append the rest of
					; the quasiquote result to it

		    ((and (pair? (car exp))
			  (eq? (car (car exp)) 'unquote-splicing)
			  (= (length (car exp)) 2))
		     (cond ((= nesting 0)
			    (list 'append (car (cdr (car exp)))
				  (expand-quasiquote (cdr exp) nesting))
			    )
			   (else
			    (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
					       (expand-quasiquote (cdr exp) nesting)
					       exp))
			   )
		     )

					; for other lists, just glue
					; the expansion of the first
					; element to the expansion of
					; the rest of the list

		    (else (combine-skeletons (expand-quasiquote (car exp) nesting)
					     (expand-quasiquote (cdr exp) nesting)
					     exp)
			  )
		    )
		   )
		 )
	    (expand-quasiquote x 0)
	    ) () () ())
	 )
  )
					;
					; Define a variable without returning the value
					; Useful when defining functions to avoid
					; having lots of output generated.
					;
					; Also accepts the alternate
					; form for defining lambdas of
					; (define (name x y z) sexprs ...) 
					;

(set! define
      (macro (first rest)

					; check for alternate lambda definition form

	     (cond ((list? first)
		    (set! rest
			  (append
			   (list
			    'lambda
			    (cdr first))
			   rest))
		    (set! first (car first))
		    )
		   (else
		    (set! rest (car rest))
		    )
		   )
	     `(begin
	       (set! ,first ,rest)
	       (quote ,first))
	     )
      )

					; basic list accessors


(define (caar l) (car (car l)))

(define (cadr l) (car (cdr l)))

(define (cdar l) (cdr (car l)))

(define (caddr l) (car (cdr (cdr l))))

(define (list-tail x k)
  (if (zero? k)
      x
    (list-tail (cdr x (- k 1)))
    )
  )

(define (list-ref x k)
  (car (list-tail x k))
  )

					; (if <condition> <if-true>)
					; (if <condition> <if-true> <if-false)

(define if
  (macro (test args)
	 (cond ((null? (cdr args))
		`(cond (,test ,(car args)))
		)
	       (else
		`(cond (,test ,(car args))
		       (else ,(cadr args)))
		)
	       )
	 )
  )

(if (> 3 2) 'yes)
(if (> 3 2) 'yes 'no)
(if (> 2 3) 'no 'yes)
(if (> 2 3) 'no)

					; simple math operators

(define zero? (macro (value rest) `(eq? ,value 0)))

(zero? 1)
(zero? 0)
(zero? "hello")

(define positive? (macro (value rest) `(> ,value 0)))

(positive? 12)
(positive? -12)

(define negative? (macro (value rest) `(< ,value 0)))

(negative? 12)
(negative? -12)

(define (abs x) (if (>= x 0) x (- x)))

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

(define (even? x) (zero? (% x 2)))

(even? 2)
(even? -2)
(even? 3)
(even? -1)

(define (odd? x) (not (even? x)))

(odd? 2)
(odd? -2)
(odd? 3)
(odd? -1)


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
					    (else ())
					    )
				      )
			 )

					; the set of expressions is
					; the list of set expressions
					; pre-pended to the
					; expressions to evaluate

		   (set! make-exprs (lambda (vars exprs)
				      (cond ((not (null? vars))
					     (cons
					      (list set
						    (list quote
							  (car (car vars))
							  )
						    (cond ((null? (cdr (car vars))) ())
							  (else (cadr (car vars))))
						    )
					      (make-exprs (cdr vars) exprs)
					      )
					     )
					    (else exprs)
					    )
				      )
			 )

					; the parameters to the lambda is a list
					; of nils of the right length

		   (set! make-nils (lambda (vars)
				     (cond ((not (null? vars)) (cons () (make-nils (cdr vars))))
					   (else ())
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

(define let* let)

(define when (macro (test l)
		    (list
		     cond
		     (cons test l))))

(when #t (display 'when))

(define unless (macro (test l)
		      (list
		       cond
		       (cons (list not test) l))))

(unless #f (display 'unless))

(define (reverse list)
  (let ((result ()))
    (while (not (null? list))
      (set! result (cons (car list) result))
      (set! list (cdr list))
      )
    result)
  )

(reverse '(1 2 3))

(define (list-tail x k)
  (if (zero? k)
      x
    (list-tail (cdr x) (- k 1)))))

(list-tail '(1 2 3) 2)

(define (list-ref x k) (car (list-tail x k)))

(list-ref '(1 2 3) 2)
    
					; recursive equality

(define (equal? a b)
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

(define (_member obj list test?)
  (if (null? list)
      #f
    (if (test? obj (car list))
	list
      (memq obj (cdr list)))))

(define (memq obj list) (_member obj list eq?))

(memq 2 '(1 2 3))

(memq 4 '(1 2 3))

(define (memv obj list) (_member obj list eqv?))

(memv 2 '(1 2 3))

(memv 4 '(1 2 3))

(define (member obj list) (_member obj list equal?))

(member '(2) '((1) (2) (3)))

(member '(4) '((1) (2) (3)))

(define (_assoc obj list test?)
  (if (null? list)
      #f
    (if (test? obj (caar list))
	(car list)
      (_assoc obj (cdr list) test?)
      )
    )
  )

(define (assq obj list) (_assoc obj list eq?))
(define (assv obj list) (_assoc obj list eqv?))
(define (assoc obj list) (_assoc obj list equal?))

(assq 'a '((a 1) (b 2) (c 3)))
(assv 'b '((a 1) (b 2) (c 3)))
(assoc '(c) '((a 1) (b 2) ((c) 3)))

(define char? integer?)

(char? #\q)
(char? "h")

(define (char-upper-case? c) (<= #\A c #\Z))

(char-upper-case? #\a)
(char-upper-case? #\B)
(char-upper-case? #\0)
(char-upper-case? #\space)

(define (char-lower-case? c) (<= #\a c #\a))

(char-lower-case? #\a)
(char-lower-case? #\B)
(char-lower-case? #\0)
(char-lower-case? #\space)

(define (char-alphabetic? c) (or (char-upper-case? c) (char-lower-case? c)))

(char-alphabetic? #\a)
(char-alphabetic? #\B)
(char-alphabetic? #\0)
(char-alphabetic? #\space)

(define (char-numeric? c) (<= #\0 c #\9))

(char-numeric? #\a)
(char-numeric? #\B)
(char-numeric? #\0)
(char-numeric? #\space)

(define (char-whitespace? c) (or (<= #\tab c #\return) (= #\space c)))

(char-whitespace? #\a)
(char-whitespace? #\B)
(char-whitespace? #\0)
(char-whitespace? #\space)

(define (char->integer c) c)
(define (integer->char c) char-integer)

(define (char-upcase c) (if (char-lower-case? c) (+ c (- #\A #\a)) c))

(char-upcase #\a)
(char-upcase #\B)
(char-upcase #\0)
(char-upcase #\space)

(define (char-downcase c) (if (char-upper-case? c) (+ c (- #\a #\A)) c))

(char-downcase #\a)
(char-downcase #\B)
(char-downcase #\0)
(char-downcase #\space)

(define string (lexpr (chars) (list->string chars)))

(display "apply\n")
(apply cons '(a b))

(define map (lexpr (proc lists)
		   (let ((args (lambda (lists)
				 (if (null? lists) ()
				   (cons (caar lists) (args (cdr lists))))))
			 (next (lambda (lists)
				 (if (null? lists) ()
				   (cons (cdr (car lists)) (next (cdr lists))))))
			 (domap (lambda (lists)
				  (if (null? (car lists)) ()
				    (cons (apply proc (args lists)) (domap (next lists)))
					)))
			 )
		     (domap lists))))

(map cadr '((a b) (d e) (g h)))

(define for-each (lexpr (proc lists)
			(apply map proc lists)
			#t))

(for-each display '("hello" " " "world" "\n"))

(define _string-ml (lambda (strings)
			     (if (null? strings) ()
			       (cons (string->list (car strings)) (_string-ml (cdr strings))))))

(define string-map (lexpr (proc strings)
			  (list->string (apply map proc (_string-ml strings))))))

(string-map (lambda (x) (+ 1 x)) "HAL")

(define string-for-each (lexpr (proc strings)
			       (apply for-each proc (_string-ml strings))))

(string-for-each write-char "IBM\n")

(define newline (lambda () (write-char #\newline)))

(newline)

(call-with-current-continuation
 (lambda (exit)
   (for-each (lambda (x)
	       (write "test" x)
	       (if (negative? x)
		   (exit x)))
	     '(54 0 37 -3 245 19))
   #t))


					; `q -> (quote q)
					; `(q) -> (append (quote (q)))
					; `(a ,(+ 1 2)) -> (append (quote (a)) (list (+ 1 2)))
					; `(a ,@(list 1 2 3) -> (append (quote (a)) (list 1 2 3))



`(hello ,(+ 1 2) ,@(list 1 2 3) `foo)

(define repeat (macro (count rest)
		       `(let ((__count__ ,count))
			  (while (<= 0 (set! __count__ (- __count__ 1))) ,@rest))))

(repeat 2 (write 'hello))
(repeat 3 (write 'goodbye))

(define case (macro (test l)
		    (let ((_unarrow
					; construct the body of the
					; case, dealing with the
					; lambda version ( => lambda)
			   
			   (lambda (l)
			     (cond ((null? l) l)
				   ((eq? (car l) '=>) `(( ,(cadr l) __key__)))
				   (else l))))
			  (_case (lambda (l)

					; Build the case elements, which is
					; simply a list of cond clauses

				   (cond ((null? l) ())

					; else case

					 ((eq? (caar l) 'else)
					  `((else ,@(_unarrow (cdr (car l))))))

					; regular case
					  
					 (else
					  (cons
					   `((eqv? ,(caar l) __key__)
					     ,@(_unarrow (cdr (car l))))
					   (_case (cdr l)))
					  )
					 ))))

					; now construct the overall
					; expression, using a lambda
					; to hold the computed value
					; of the test expression

		      `((lambda (__key__)
			  (cond ,@(_case l))) ,test))))

(case 12 (1 "one") (2 "two") (3 => (lambda (x) (write "the value is" x))) (12 "twelve") (else "else"))

;(define number->string (lexpr (arg opt)
;			      (let ((base (if (null? opt) 10 (car opt)))
					;
;
				
