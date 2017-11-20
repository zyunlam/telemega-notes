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

(define move-to (lambda (col row)
		  (for-each display (list "\033[" row ";" col "H"))
		  )
  )

(define clear (lambda ()
		(display "\033[2J")
		)
  )

(define display-string (lambda (x y str)
			 (move-to x y)
			 (display str)
			 )
  )

					; Here's the pieces to display

(define tower '("     *     " "    ***    " "   *****   " "  *******  " " ********* " "***********"))

					; Here's all of the towers of pieces
					; This is generated when the program is run

(define towers ())

(define one- (lambda (x) (- x 1)))
(define one+ (lambda (x) (+ x 1)))
					; Display one tower, clearing any
					; space above it

(define display-tower (lambda (x y clear tower)
			(cond ((= 0 clear)
			       (cond ((not (null? tower))
				      (display-string x y (car tower))
				      (display-tower x (one+ y) 0 (cdr tower))
				      )
				     )
			       )
			      (else 
			       (display-string x y "                   ")
			       (display-tower x (one+ y) (one- clear) tower)
			       )
			      )
			)
  )

					; Position of the top of the tower on the screen
					; Shorter towers start further down the screen

(define tower-pos (lambda (y tower)
		    (- y (length tower))
		    )
  )

					; Display all of the towers, spaced 20 columns apart

(define display-towers (lambda (x y towers)
			 (cond ((not (null? towers))
				(display-tower x 0 (tower-pos y (car towers)) (car towers))
				(display-towers (+ x 20) y (cdr towers)))
			       )
			 )
  )

(define top 0)
					; Display all of the towers, then move the cursor
					; out of the way and flush the output

(define display-hanoi (lambda ()
			(display-towers 0 top towers)
			(move-to 1 21)
			(flush-output)
			)
  )

					; Reset towers to the starting state, with
					; all of the pieces in the first tower and the
					; other two empty

(define reset-towers (lambda ()
		       (set! towers (list tower () ()))
		       (set! top (+ (length tower) 3))
		       (length tower)
		       )
  )

					; Replace a tower in the list of towers
					; with a new value

(define replace (lambda (list pos member)
		  (cond ((= pos 0) (cons member (cdr list)))
			((cons (car list) (replace (cdr list) (one- pos) member)))
			)
		  )
  )

					; Move a piece from the top of one tower
					; to the top of another

(define move-delay 10)

(define move-piece (lambda (from to)
		     (let* ((from-tower (list-ref towers from))
			   (to-tower (list-ref towers to))
			   (piece (car from-tower)))
		       (set! from-tower (cdr from-tower))
		       (set! to-tower (cons piece to-tower))
		       (set! towers (replace towers from from-tower))
		       (set! towers (replace towers to to-tower))
		       (display-hanoi)
		       (delay move-delay)
		       )
		     )
  )

; The implementation of the game

(define _hanoi (lambda (n from to use)
		 (cond ((= 1 n)
			(move-piece from to)
			)
		       (else
			(_hanoi (one- n) from use to)
			(_hanoi 1 from to use)
			(_hanoi (one- n) use to from)
			)
		       )
		 )
  )

					; A pretty interface which
					; resets the state of the game,
					; clears the screen and runs
					; the program

(define hanoi (lambda ()
		(let ((len))
		  (set! len (reset-towers))
		  (clear)
		  (_hanoi len 0 1 2)
		  (move-to 0 23)
		  #t
		  )
		)
  )
