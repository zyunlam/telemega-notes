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

#include "ao_lisp.h"

const uint32_t	classTable[256] = {
	IGNORE,		/* ^@ */
	IGNORE,		/* ^A */
	IGNORE,		/* ^B */
	IGNORE,		/* ^C */
	IGNORE,		/* ^D */
	IGNORE,		/* ^E */
	IGNORE,		/* ^F */
	IGNORE,		/* ^G */
	IGNORE,		/* ^H */
	WHITE,		/* ^I */
	WHITE,		/* ^J */
	WHITE,		/* ^K */
	WHITE,		/* ^L */
	WHITE,		/* ^M */
	IGNORE,		/* ^N */
	IGNORE,		/* ^O */
	IGNORE,		/* ^P */
	IGNORE,		/* ^Q */
	IGNORE,		/* ^R */
	IGNORE,		/* ^S */
	IGNORE,		/* ^T */
	IGNORE,		/* ^U */
	IGNORE,		/* ^V */
	IGNORE,		/* ^W */
	IGNORE,		/* ^X */
	IGNORE,		/* ^Y */
	IGNORE,		/* ^Z */
	IGNORE,		/* ^[ */
	IGNORE,		/* ^\ */
	IGNORE,		/* ^] */
	IGNORE,		/* ^^ */
	IGNORE,		/* ^_ */
	PRINTABLE|WHITE,	/*    */
 	PRINTABLE,		/* ! */
 	PRINTABLE|STRINGC,	/* " */
 	PRINTABLE|COMMENT,	/* # */
 	PRINTABLE,		/* $ */
 	PRINTABLE,		/* % */
 	PRINTABLE,		/* & */
 	PRINTABLE|QUOTEC,	/* ' */
 	PRINTABLE|BRA,		/* ( */
 	PRINTABLE|KET,		/* ) */
 	PRINTABLE,		/* * */
 	PRINTABLE|SIGN,		/* + */
 	PRINTABLE,		/* , */
 	PRINTABLE|SIGN,		/* - */
 	PRINTABLE|DOT,		/* . */
 	PRINTABLE,		/* / */
 	PRINTABLE|DIGIT,	/* 0 */
 	PRINTABLE|DIGIT,	/* 1 */
 	PRINTABLE|DIGIT,	/* 2 */
 	PRINTABLE|DIGIT,	/* 3 */
 	PRINTABLE|DIGIT,	/* 4 */
 	PRINTABLE|DIGIT,	/* 5 */
 	PRINTABLE|DIGIT,	/* 6 */
 	PRINTABLE|DIGIT,	/* 7 */
 	PRINTABLE|DIGIT,	/* 8 */
 	PRINTABLE|DIGIT,	/* 9 */
 	PRINTABLE,		/* : */
 	PRINTABLE|COMMENT,	/* ; */
 	PRINTABLE,		/* < */
 	PRINTABLE,		/* = */
 	PRINTABLE,		/* > */
 	PRINTABLE,		/* ? */
  	PRINTABLE,		/*  @ */
	PRINTABLE,		/*  A */
	PRINTABLE,		/*  B */
	PRINTABLE,		/*  C */
	PRINTABLE,		/*  D */
	PRINTABLE|EXP,		/*  E */
	PRINTABLE,		/*  F */
	PRINTABLE,		/*  G */
	PRINTABLE,		/*  H */
	PRINTABLE,		/*  I */
	PRINTABLE,		/*  J */
	PRINTABLE,		/*  K */
	PRINTABLE,		/*  L */
	PRINTABLE,		/*  M */
	PRINTABLE,		/*  N */
	PRINTABLE,		/*  O */
	PRINTABLE,		/*  P */
	PRINTABLE,		/*  Q */
	PRINTABLE,		/*  R */
	PRINTABLE,		/*  S */
	PRINTABLE,		/*  T */
	PRINTABLE,		/*  U */
	PRINTABLE,		/*  V */
	PRINTABLE,		/*  W */
	PRINTABLE,		/*  X */
	PRINTABLE,		/*  Y */
	PRINTABLE,		/*  Z */
	PRINTABLE|BRA,		/*  [ */
	PRINTABLE|BACKSLASH,	/*  \ */
	PRINTABLE|KET,		/*  ] */
	PRINTABLE,		/*  ^ */
	PRINTABLE,		/*  _ */
  	PRINTABLE,		/*  ` */
	PRINTABLE,		/*  a */
	PRINTABLE,		/*  b */
	PRINTABLE,		/*  c */
	PRINTABLE,		/*  d */
	PRINTABLE|EXP,		/*  e */
	PRINTABLE,		/*  f */
	PRINTABLE,		/*  g */
	PRINTABLE,		/*  h */
	PRINTABLE,		/*  i */
	PRINTABLE,		/*  j */
	PRINTABLE,		/*  k */
	PRINTABLE,		/*  l */
	PRINTABLE,		/*  m */
	PRINTABLE,		/*  n */
	PRINTABLE,		/*  o */
	PRINTABLE,		/*  p */
	PRINTABLE,		/*  q */
	PRINTABLE,		/*  r */
	PRINTABLE,		/*  s */
	PRINTABLE,		/*  t */
	PRINTABLE,		/*  u */
	PRINTABLE,		/*  v */
	PRINTABLE,		/*  w */
	PRINTABLE,		/*  x */
	PRINTABLE,		/*  y */
	PRINTABLE,		/*  z */
	PRINTABLE|BRA,		/*  { */
	PRINTABLE|VBAR,		/*  | */
	PRINTABLE|KET,		/*  } */
	PRINTABLE|TWIDDLE,	/*  ~ */
	IGNORE,			/*  ^? */
};
