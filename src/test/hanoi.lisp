(defun move-to (col row)
  (patom "\033[" row ";" col "H" nil)
  )

(defun clear ()
  (patom "\033[2J" nil)
  )

(defun test ()
  (clear)
  (move-to 30 12)
  (patom "hello, world")
  (move-to 0 19)
  )

(setq stack '("*" "**" "***" "****" "*****" "******" "*******"))

(setq stacks nil)

(defun display-string (x y str)
  (move-to x y)
  (move-to x y)
  (patom str)
  )

(defun display-stack (x y stack)
  (cond (stack (progn
		 (display-string x y (car stack))
		 (display-stack x (1+ y) (cdr stack)))))
  )

(defun clear-stack (x y)
  (cond ((> y 0) (progn
		   (move-to x y)
		   (patom "            ")
		   (clear-stack x (1- y))
		   )
	 )
	)
  )

(defun length (list)
  (cond (list (1+ (length (cdr list))))
	(0)
	)
  )

(defun stack-pos (y stack)
  (- y (length stack))
  )

(defun display-stacks (x y stacks)
  (cond (stacks (progn
		  (clear-stack x 20)
		  (display-stack x (stack-pos y (car stacks)) (car stacks))
		  (display-stacks (+ x 20) y (cdr stacks)))
		)
	)
  )

(defun display ()
  (display-stacks 0 20 stacks)
  (move-to 1 21)
  (flush)
  )

(defun length (l)
  (cond (l (1+ (length (cdr l)))) (0))
  )

(defun reset-stacks ()
  (setq stacks (list stack nil nil))
  (length stack)
  )

(defun min (a b)
  (cond ((< a b) a)
	(b)
	)
  )

(defun nth (list n)
  (cond ((= n 0) (car list))
	((nth (cdr list) (1- n)))
	)
  )

(defun replace (list pos member)
  (cond ((= pos 0) (cons member (cdr list)))
	((cons (car list) (replace (cdr list) (1- pos) member)))
	)
  )

(defun move-piece (from to)
  (let ((from-stack (nth stacks from))
	(to-stack (nth stacks to))
	(piece (car from-stack)))
    (setq from-stack (cdr from-stack))
    (setq to-stack (cons piece to-stack))
    (setq stacks (replace stacks from from-stack))
    (setq stacks (replace stacks to to-stack))
    (display)
    (delay 100)
    )
  )

(defun _hanoi (n from to use)
  (cond ((= 1 n)
	 (progn
	  (move-piece from to)
	  nil)
	 )
	(t
	 (progn
	  (_hanoi (1- n) from use to)
	  (_hanoi 1 from to use)
	  (_hanoi (1- n) use to from)
	  )
	 )
	)
  )

(defun hanoi ()
  (setq len (reset-stacks))
  (clear)
  (_hanoi len 0 1 2)
  )
