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

/*
 * token classes
 */

# define END	0
# define NAME	1
# define OPEN  	2
# define CLOSE	3
# define QUOTE	4
# define STRING	5
# define NUM	6
# define FLOAT	7
# define DOT	8
# define BOOL	9

/*
 * character classes
 */

# define PRINTABLE	0x0001	/* \t \n ' ' - '~' */
# define SPECIAL	0x0002	/* ( [ { ) ] } ' */
# define DOTC		0x0004	/* . */
# define WHITE		0x0008	/* ' ' \t \n */
# define DIGIT		0x0010	/* [0-9] */
# define SIGN		0x0020	/* +- */
# define FLOATC		0x0040	/* . e E */
# define ENDOFFILE	0x0080	/* end of file */
# define COMMENT	0x0100	/* ; */
# define IGNORE		0x0200	/* \0 - ' ' */
# define BACKSLASH	0x0400	/* \ */
# define STRINGC	0x0800	/* " */
# define POUND		0x1000	/* # */

# define NOTNAME	(STRINGC|COMMENT|ENDOFFILE|WHITE|SPECIAL)
# define INTEGER	(DIGIT|SIGN)
# define NUMBER		(INTEGER|FLOATC)

#endif /* _AO_LISP_READ_H_ */
