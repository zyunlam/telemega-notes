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
#include "ao_lisp_read.h"

static const uint16_t	lex_classes[128] = {
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
 	PRINTABLE|POUND,	/* # */
 	PRINTABLE,		/* $ */
 	PRINTABLE,		/* % */
 	PRINTABLE,		/* & */
 	PRINTABLE|SPECIAL,	/* ' */
 	PRINTABLE|SPECIAL,	/* ( */
 	PRINTABLE|SPECIAL,	/* ) */
 	PRINTABLE,		/* * */
 	PRINTABLE|SIGN,		/* + */
 	PRINTABLE,		/* , */
 	PRINTABLE|SIGN,		/* - */
 	PRINTABLE|SPECIAL,	/* . */
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
	PRINTABLE,		/*  E */
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
	PRINTABLE,		/*  [ */
	PRINTABLE|BACKSLASH,	/*  \ */
	PRINTABLE,		/*  ] */
	PRINTABLE,		/*  ^ */
	PRINTABLE,		/*  _ */
  	PRINTABLE,		/*  ` */
	PRINTABLE,		/*  a */
	PRINTABLE,		/*  b */
	PRINTABLE,		/*  c */
	PRINTABLE,		/*  d */
	PRINTABLE,		/*  e */
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
	PRINTABLE,		/*  { */
	PRINTABLE|VBAR,		/*  | */
	PRINTABLE,		/*  } */
	PRINTABLE,		/*  ~ */
	IGNORE,			/*  ^? */
};

static int lex_unget_c;

static inline int
lex_get()
{
	int	c;
	if (lex_unget_c) {
		c = lex_unget_c;
		lex_unget_c = 0;
	} else {
		c = ao_lisp_getc();
	}
	return c;
}

static inline void
lex_unget(int c)
{
	if (c != EOF)
		lex_unget_c = c;
}

static uint16_t	lex_class;

static int
lexc(void)
{
	int	c;
	do {
		c = lex_get();
		if (c == EOF) {
			c = 0;
			lex_class = ENDOFFILE;
		} else {
			c &= 0x7f;
			lex_class = lex_classes[c];
		}
	} while (lex_class & IGNORE);
	return c;
}

static int
lex_quoted(void)
{
	int	c;
	int	v;
	int	count;

	c = lex_get();
	if (c == EOF) {
		lex_class = ENDOFFILE;
		return 0;
	}
	lex_class = 0;
	c &= 0x7f;
 	switch (c) {
	case 'n':
		return '\n';
	case 'f':
		return '\f';
	case 'b':
		return '\b';
	case 'r':
		return '\r';
	case 'v':
		return '\v';
	case 't':
		return '\t';
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		v = c - '0';
		count = 1;
		while (count <= 3) {
			c = lex_get();
			if (c == EOF)
				return EOF;
			c &= 0x7f;
			if (c < '0' || '7' < c) {
				lex_unget(c);
				break;
			}
			v = (v << 3) + c - '0';
			++count;
		}
		return v;
	default:
		return c;
	}
}

#define AO_LISP_TOKEN_MAX	32

static char	token_string[AO_LISP_TOKEN_MAX];
static int	token_int;
static int	token_len;

static inline void add_token(int c) {
	if (c && token_len < AO_LISP_TOKEN_MAX - 1)
		token_string[token_len++] = c;
}

static inline void end_token(void) {
	token_string[token_len] = '\0';
}

static int
_lex(void)
{
	int	c;

	token_len = 0;
	for (;;) {
		c = lexc();
		if (lex_class & ENDOFFILE)
			return END;

		if (lex_class & WHITE)
			continue;

		if (lex_class & COMMENT) {
			while ((c = lexc()) != '\n') {
				if (lex_class & ENDOFFILE)
					return END;
			}
			continue;
		}

		if (lex_class & SPECIAL) {
			add_token(c);
			end_token();
			switch (c) {
			case '(':
			case '[':
				return OPEN;
			case ')':
			case ']':
				return CLOSE;
			case '\'':
				return QUOTE;
			case '.':
				return DOT;
			}
		}
		if (lex_class & POUND) {
			c = lexc();
			switch (c) {
			case 't':
				add_token(c);
				end_token();
				return BOOL;
			case 'f':
				add_token(c);
				end_token();
				return BOOL;
			case '\\':
				for (;;) {
					int alphabetic;
					c = lexc();
					alphabetic = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'));
					if (token_len == 0) {
						add_token(c);
						if (!alphabetic)
							break;
					} else {
						if (alphabetic)
							add_token(c);
						else {
							lex_unget(c);
							break;
						}
					}
				}
				end_token();
				if (token_len == 1)
					token_int = token_string[0];
				else if (!strcmp(token_string, "space"))
					token_int = ' ';
				else if (!strcmp(token_string, "newline"))
					token_int = '\n';
				else if (!strcmp(token_string, "tab"))
					token_int = '\t';
				else if (!strcmp(token_string, "return"))
					token_int = '\r';
				else if (!strcmp(token_string, "formfeed"))
					token_int = '\f';
				else {
					ao_lisp_error(AO_LISP_INVALID, "invalid character token #\\%s", token_string);
					continue;
				}
				return NUM;
			}
		}
		if (lex_class & STRINGC) {
			for (;;) {
				c = lexc();
				if (lex_class & BACKSLASH)
					c = lex_quoted();
				if (lex_class & (STRINGC|ENDOFFILE)) {
					end_token();
					return STRING;
				}
				add_token(c);
			}
		}
		if (lex_class & PRINTABLE) {
			int	isnum;
			int	hasdigit;
			int	isneg;

			isnum = 1;
			hasdigit = 0;
			token_int = 0;
			isneg = 0;
			for (;;) {
				if (!(lex_class & NUMBER)) {
					isnum = 0;
				} else {
 					if (token_len != 0 &&
					    (lex_class & SIGN))
					{
						isnum = 0;
					}
					if (c == '-')
						isneg = 1;
					if (lex_class & DIGIT) {
						hasdigit = 1;
						if (isnum)
							token_int = token_int * 10 + c - '0';
					}
				}
				add_token (c);
				c = lexc ();
				if (lex_class & (NOTNAME)) {
//					if (lex_class & ENDOFFILE)
//						clearerr (f);
					lex_unget(c);
					end_token ();
					if (isnum && hasdigit) {
						if (isneg)
							token_int = -token_int;
						return NUM;
					}
					return NAME;
				}
			}

		}
	}
}

static inline int lex(void)
{
	int	parse_token = _lex();
	DBGI("token %d (%s)\n", parse_token, token_string);
	return parse_token;
}

static int parse_token;

struct ao_lisp_cons	*ao_lisp_read_cons;
struct ao_lisp_cons	*ao_lisp_read_cons_tail;
struct ao_lisp_cons	*ao_lisp_read_stack;

#define READ_IN_QUOTE	0x01
#define READ_SAW_DOT	0x02
#define READ_DONE_DOT	0x04

static int
push_read_stack(int cons, int read_state)
{
	DBGI("push read stack %p 0x%x\n", ao_lisp_read_cons, read_state);
	DBG_IN();
	if (cons) {
		ao_lisp_read_stack = ao_lisp_cons_cons(ao_lisp_cons_poly(ao_lisp_read_cons),
						       ao_lisp__cons(ao_lisp_int_poly(read_state),
								     ao_lisp_cons_poly(ao_lisp_read_stack)));
		if (!ao_lisp_read_stack)
			return 0;
	}
	ao_lisp_read_cons = NULL;
	ao_lisp_read_cons_tail = NULL;
	return 1;
}

static int
pop_read_stack(int cons)
{
	int	read_state = 0;
	if (cons) {
		ao_lisp_read_cons = ao_lisp_poly_cons(ao_lisp_read_stack->car);
		ao_lisp_read_stack = ao_lisp_poly_cons(ao_lisp_read_stack->cdr);
		read_state = ao_lisp_poly_int(ao_lisp_read_stack->car);
		ao_lisp_read_stack = ao_lisp_poly_cons(ao_lisp_read_stack->cdr);
		for (ao_lisp_read_cons_tail = ao_lisp_read_cons;
		     ao_lisp_read_cons_tail && ao_lisp_read_cons_tail->cdr;
		     ao_lisp_read_cons_tail = ao_lisp_poly_cons(ao_lisp_read_cons_tail->cdr))
			;
	} else {
		ao_lisp_read_cons = 0;
		ao_lisp_read_cons_tail = 0;
		ao_lisp_read_stack = 0;
	}
	DBG_OUT();
	DBGI("pop read stack %p %d\n", ao_lisp_read_cons, read_state);
	return read_state;
}

ao_poly
ao_lisp_read(void)
{
	struct ao_lisp_atom	*atom;
	char			*string;
	int			cons;
	int			read_state;
	ao_poly			v;


	cons = 0;
	read_state = 0;
	ao_lisp_read_cons = ao_lisp_read_cons_tail = ao_lisp_read_stack = 0;
	for (;;) {
		parse_token = lex();
		while (parse_token == OPEN) {
			if (!push_read_stack(cons, read_state))
				return AO_LISP_NIL;
			cons++;
			read_state = 0;
			parse_token = lex();
		}

		switch (parse_token) {
		case END:
		default:
			if (cons)
				ao_lisp_error(AO_LISP_EOF, "unexpected end of file");
			return _ao_lisp_atom_eof;
			break;
		case NAME:
			atom = ao_lisp_atom_intern(token_string);
			if (atom)
				v = ao_lisp_atom_poly(atom);
			else
				v = AO_LISP_NIL;
			break;
		case NUM:
			v = ao_lisp_int_poly(token_int);
			break;
		case BOOL:
			if (token_string[0] == 't')
				v = _ao_lisp_bool_true;
			else
				v = _ao_lisp_bool_false;
			break;
		case STRING:
			string = ao_lisp_string_copy(token_string);
			if (string)
				v = ao_lisp_string_poly(string);
			else
				v = AO_LISP_NIL;
			break;
		case QUOTE:
			if (!push_read_stack(cons, read_state))
				return AO_LISP_NIL;
			cons++;
			read_state = READ_IN_QUOTE;
			v = _ao_lisp_atom_quote;
			break;
		case CLOSE:
			if (!cons) {
				v = AO_LISP_NIL;
				break;
			}
			v = ao_lisp_cons_poly(ao_lisp_read_cons);
			--cons;
			read_state = pop_read_stack(cons);
			break;
		case DOT:
			if (!cons) {
				ao_lisp_error(AO_LISP_INVALID, ". outside of cons");
				return AO_LISP_NIL;
			}
			if (!ao_lisp_read_cons) {
				ao_lisp_error(AO_LISP_INVALID, ". first in cons");
				return AO_LISP_NIL;
			}
			read_state |= READ_SAW_DOT;
			continue;
		}

		/* loop over QUOTE ends */
		for (;;) {
			if (!cons)
				return v;

			if (read_state & READ_DONE_DOT) {
				ao_lisp_error(AO_LISP_INVALID, ". not last in cons");
				return AO_LISP_NIL;
			}

			if (read_state & READ_SAW_DOT) {
				read_state |= READ_DONE_DOT;
				ao_lisp_read_cons_tail->cdr = v;
			} else {
				struct ao_lisp_cons	*read = ao_lisp_cons_cons(v, AO_LISP_NIL);
				if (!read)
					return AO_LISP_NIL;

				if (ao_lisp_read_cons_tail)
					ao_lisp_read_cons_tail->cdr = ao_lisp_cons_poly(read);
				else
					ao_lisp_read_cons = read;
				ao_lisp_read_cons_tail = read;
			}

			if (!(read_state & READ_IN_QUOTE) || !ao_lisp_read_cons->cdr)
				break;

			v = ao_lisp_cons_poly(ao_lisp_read_cons);
			--cons;
			read_state = pop_read_stack(cons);
		}
	}
	return v;
}
