					; basic list accessors


(setq cadr (lambda (l) (car (cdr l))))
(setq caddr (lambda (l) (car (cdr (cdr l)))))
(setq list (lexpr (l) l))

					; evaluate a list of sexprs

(setq progn (lexpr (l) (last l)))

					; simple math operators

(setq 1+ (lambda (x) (+ x 1)))
(setq 1- (lambda (x) (- x 1)))

					; define a variable without returning the value

(set 'def (macro (def-param)
		 (list
		  'progn
		  (list
		   'set
		   (list
		    'quote
		    (car def-param))
		   (cadr def-param)
		   )
		  (list
		   'quote
		   (car def-param)
		   )
		  )
		 )
     )

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

(def let (macro (let-param)
		((lambda (vars exprs make-names make-exprs make-nils)
		   (progn

					;
					; make the list of names in the let
					;

		     (set 'make-names (lambda (vars)
				       (cond (vars
					      (cons (car (car vars))
						    (make-names (cdr vars))))
					     )
				       )
			  )
					;
					; the set of expressions is
					; the list of set expressions
					; pre-pended to the
					; expressions to evaluate
					;
		     (set 'make-exprs (lambda (vars exprs)
				       (progn
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
			  )
		     (set 'exprs (make-exprs vars exprs))

					;
					; the parameters to the lambda is a list
					; of nils of the right length
					;
		     (set 'make-nils (lambda (vars)
				      (cond (vars (cons nil (make-nils (cdr vars))))
					    )
				      )
			  )
					;
					; build the lambda.
					;
		     (set 'last-let-value 
		     (cons
		      (list
		       'lambda
		       (make-names vars)
		       (cond ((cdr exprs) (cons 'progn exprs))
			     ((car exprs))
			     )
		       )
		      (make-nils vars)
		      )
		     )
		     )
		     
		   )
		 (car let-param)
		 (cdr let-param)
		 ()
		 ()
		 ()
		 )
		)
     )
