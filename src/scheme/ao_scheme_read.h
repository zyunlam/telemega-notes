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

#ifndef _AO_SCHEME_READ_H_
#define _AO_SCHEME_READ_H_

/*
 * token classes
 */

# define END			0
# define NAME			1
# define OPEN  			2
# define CLOSE			3
# define QUOTE			4
#ifdef AO_SCHEME_FEATURE_QUASI
# define QUASIQUOTE		5
# define UNQUOTE		6
# define UNQUOTE_SPLICING	7
#endif
# define STRING			8
# define NUM			9
#ifdef AO_SCHEME_FEATURE_FLOAT
# define FLOAT			10
#endif
# define DOT			11
# define BOOL			12
#ifdef AO_SCHEME_FEATURE_VECTOR
# define OPEN_VECTOR		13
#endif

/*
 * character classes
 */

# define PRINTABLE	0x0001	/* \t \n ' ' - ~ */
# define SPECIAL	0x0002	/* ( [ { ) ] } ' ` , */
#ifdef AO_SCHEME_FEATURE_QUASI
# define SPECIAL_QUASI	SPECIAL
#else
# define SPECIAL_QUASI	0
#endif
# define DOTC		0x0004	/* . */
# define WHITE		0x0008	/* ' ' \t \n */
# define DIGIT		0x0010	/* [0-9] */
# define SIGN		0x0020	/* +- */
#ifdef AO_SCHEME_FEATURE_FLOAT
# define FLOATC		0x0040	/* . e E */
#else
# define FLOATC		0
#endif
# define ENDOFFILE	0x0080	/* end of file */
# define COMMENT	0x0100	/* ; */
# define IGNORE		0x0200	/* \0 - ' ' */
# define BACKSLASH	0x0400	/* \ */
# define STRINGC	0x0800	/* " */
# define HEX_LETTER	0x1000	/* a-f A-F */

# define NOTNAME	(STRINGC|COMMENT|ENDOFFILE|WHITE|SPECIAL)
# define INTEGER	(DIGIT|SIGN)
# define NUMBER		(INTEGER|FLOATC)
# define HEX_DIGIT	(DIGIT|HEX_LETTER)

#endif /* _AO_SCHEME_READ_H_ */
