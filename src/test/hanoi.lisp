;
; Towers of Hanoi
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

					; ANSI control sequences

(defun move-to (col row)
  (patom "\033[" row ";" col "H")
  )

(defun clear ()
  (patom "\033[2J")
  )

(defun display-string (x y str)
  (move-to x y)
  (patom str)
  )

					; Here's the pieces to display

(setq stack '("     *     " "    ***    " "   *****   " "  *******  " " ********* " "***********"))

					; Here's all of the stacks of pieces
					; This is generated when the program is run

(setq stacks nil)

					; Display one stack, clearing any
					; space above it

(defun display-stack (x y clear stack)
  (cond ((= 0 clear)
	 (cond (stack 
		(display-string x y (car stack))
		(display-stack x (1+ y) 0 (cdr stack))
		)
	       )
	 )
	(t 
	 (display-string x y "                   ")
	 (display-stack x (1+ y) (1- clear) stack)
	 )
	)
  )

					; Position of the top of the stack on the screen
					; Shorter stacks start further down the screen

(defun stack-pos (y stack)
  (- y (length stack))
  )

					; Display all of the stacks, spaced 20 columns apart

(defun display-stacks (x y stacks)
  (cond (stacks
	 (display-stack x 0 (stack-pos y (car stacks)) (car stacks))
	 (display-stacks (+ x 20) y (cdr stacks)))
	)
  )

					; Display all of the stacks, then move the cursor
					; out of the way and flush the output

(defun display ()
  (display-stacks 0 top stacks)
  (move-to 1 21)
  (flush)
  )

					; Reset stacks to the starting state, with
					; all of the pieces in the first stack and the
					; other two empty

(defun reset-stacks ()
  (setq stacks (list stack nil nil))
  (setq top (+ (length stack) 3))
  (length stack)
  )

					; more functions which could usefully
					; be in the rom image

(defun min (a b)
  (cond ((< a b) a)
	(b)
	)
  )

					; Replace a stack in the list of stacks
					; with a new value

(defun replace (list pos member)
  (cond ((= pos 0) (cons member (cdr list)))
	((cons (car list) (replace (cdr list) (1- pos) member)))
	)
  )

					; Move a piece from the top of one stack
					; to the top of another

(setq move-delay 100)

(defun move-piece (from to)
  (let ((from-stack (nth stacks from))
	(to-stack (nth stacks to))
	(piece (car from-stack)))
    (setq from-stack (cdr from-stack))
    (setq to-stack (cons piece to-stack))
    (setq stacks (replace stacks from from-stack))
    (setq stacks (replace stacks to to-stack))
    (display)
    (delay move-delay)
    )
  )

; The implementation of the game

(defun _hanoi (n from to use)
  (cond ((= 1 n)
	 (move-piece from to)
	 )
	(t
	 (_hanoi (1- n) from use to)
	 (_hanoi 1 from to use)
	 (_hanoi (1- n) use to from)
	 )
	)
  )

					; A pretty interface which
					; resets the state of the game,
					; clears the screen and runs
					; the program

(defun hanoi ()
  (setq len (reset-stacks))
  (clear)
  (_hanoi len 0 1 2)
  (move-to 0 23)
  t
  )
