/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 */

#ifndef _AO_LISP_READ_H_
#define _AO_LISP_READ_H_

# define END	0
# define NAME	1
# define OPEN  	2
# define CLOSE	3
# define QUOTE	4
# define STRING	5
# define NUM	6

/*
 * character classes
 */

# define PRINTABLE	0x00000001	/* \t \n ' ' - '~' */
# define QUOTED		0x00000002	/* \ anything */
# define BRA		0x00000004	/* ( [ { */
# define KET		0x00000008	/* ) ] } */
# define WHITE		0x00000010	/* ' ' \t \n */
# define DIGIT		0x00000020	/* [0-9] */
# define SIGN		0x00000040	/* +- */
# define ENDOFFILE	0x00000080	/* end of file */
# define COMMENT	0x00000100	/* ; # */
# define IGNORE		0x00000200	/* \0 - ' ' */
# define QUOTEC		0x00000400	/* ' */
# define BACKSLASH	0x00000800	/* \ */
# define VBAR		0x00001000	/* | */
# define TWIDDLE	0x00002000	/* ~ */
# define STRINGC	0x00004000	/* " */

# define NOTNAME	(STRINGC|TWIDDLE|VBAR|QUOTEC|COMMENT|ENDOFFILE|WHITE|KET|BRA)
# define NUMBER		(DIGIT|SIGN)

#endif /* _AO_LISP_READ_H_ */
