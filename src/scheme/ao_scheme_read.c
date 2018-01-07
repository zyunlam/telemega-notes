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

#include "ao_scheme.h"
#include "ao_scheme_read.h"
#include <math.h>
#include <stdlib.h>

static const uint16_t	lex_classes[128] = {
	IGNORE,				/* ^@ */
	IGNORE,				/* ^A */
	IGNORE,				/* ^B */
	IGNORE,				/* ^C */
	IGNORE,				/* ^D */
	IGNORE,				/* ^E */
	IGNORE,				/* ^F */
	IGNORE,				/* ^G */
	IGNORE,				/* ^H */
	WHITE,				/* ^I */
	WHITE,				/* ^J */
	WHITE,				/* ^K */
	WHITE,				/* ^L */
	WHITE,				/* ^M */
	IGNORE,				/* ^N */
	IGNORE,				/* ^O */
	IGNORE,				/* ^P */
	IGNORE,				/* ^Q */
	IGNORE,				/* ^R */
	IGNORE,				/* ^S */
	IGNORE,				/* ^T */
	IGNORE,				/* ^U */
	IGNORE,				/* ^V */
	IGNORE,				/* ^W */
	IGNORE,				/* ^X */
	IGNORE,				/* ^Y */
	IGNORE,				/* ^Z */
	IGNORE,				/* ^[ */
	IGNORE,				/* ^\ */
	IGNORE,				/* ^] */
	IGNORE,				/* ^^ */
	IGNORE,				/* ^_ */
	PRINTABLE|WHITE,		/*    */
 	PRINTABLE,			/* ! */
 	PRINTABLE|STRINGC,		/* " */
 	PRINTABLE,			/* # */
 	PRINTABLE,			/* $ */
 	PRINTABLE,			/* % */
 	PRINTABLE,			/* & */
 	PRINTABLE|SPECIAL,		/* ' */
 	PRINTABLE|SPECIAL,		/* ( */
 	PRINTABLE|SPECIAL,		/* ) */
 	PRINTABLE,			/* * */
 	PRINTABLE|SIGN,			/* + */
 	PRINTABLE|SPECIAL_QUASI,	/* , */
 	PRINTABLE|SIGN,			/* - */
 	PRINTABLE|SPECIAL|FLOATC,	/* . */
 	PRINTABLE,			/* / */
 	PRINTABLE|DIGIT,		/* 0 */
 	PRINTABLE|DIGIT,		/* 1 */
 	PRINTABLE|DIGIT,		/* 2 */
 	PRINTABLE|DIGIT,		/* 3 */
 	PRINTABLE|DIGIT,		/* 4 */
 	PRINTABLE|DIGIT,		/* 5 */
 	PRINTABLE|DIGIT,		/* 6 */
 	PRINTABLE|DIGIT,		/* 7 */
 	PRINTABLE|DIGIT,		/* 8 */
 	PRINTABLE|DIGIT,		/* 9 */
 	PRINTABLE,			/* : */
 	PRINTABLE|COMMENT,		/* ; */
 	PRINTABLE,			/* < */
 	PRINTABLE,			/* = */
 	PRINTABLE,			/* > */
 	PRINTABLE,			/* ? */
  	PRINTABLE,			/*  @ */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  A */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  B */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  C */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  D */
	PRINTABLE|ALPHA|FLOATC|HEX_LETTER,/*  E */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  F */
	PRINTABLE|ALPHA,		/*  G */
	PRINTABLE|ALPHA,		/*  H */
	PRINTABLE|ALPHA,		/*  I */
	PRINTABLE|ALPHA,		/*  J */
	PRINTABLE|ALPHA,		/*  K */
	PRINTABLE|ALPHA,		/*  L */
	PRINTABLE|ALPHA,		/*  M */
	PRINTABLE|ALPHA,		/*  N */
	PRINTABLE|ALPHA,		/*  O */
	PRINTABLE|ALPHA,		/*  P */
	PRINTABLE|ALPHA,		/*  Q */
	PRINTABLE|ALPHA,		/*  R */
	PRINTABLE|ALPHA,		/*  S */
	PRINTABLE|ALPHA,		/*  T */
	PRINTABLE|ALPHA,		/*  U */
	PRINTABLE|ALPHA,		/*  V */
	PRINTABLE|ALPHA,		/*  W */
	PRINTABLE|ALPHA,		/*  X */
	PRINTABLE|ALPHA,		/*  Y */
	PRINTABLE|ALPHA,		/*  Z */
	PRINTABLE,			/*  [ */
	PRINTABLE,			/*  \ */
	PRINTABLE,			/*  ] */
	PRINTABLE,			/*  ^ */
	PRINTABLE,			/*  _ */
  	PRINTABLE|SPECIAL_QUASI,	/*  ` */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  a */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  b */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  c */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  d */
	PRINTABLE|ALPHA|FLOATC|HEX_LETTER,/*  e */
	PRINTABLE|ALPHA|HEX_LETTER,	/*  f */
	PRINTABLE|ALPHA,		/*  g */
	PRINTABLE|ALPHA,		/*  h */
	PRINTABLE|ALPHA,		/*  i */
	PRINTABLE|ALPHA,		/*  j */
	PRINTABLE|ALPHA,		/*  k */
	PRINTABLE|ALPHA,		/*  l */
	PRINTABLE|ALPHA,		/*  m */
	PRINTABLE|ALPHA,		/*  n */
	PRINTABLE|ALPHA,		/*  o */
	PRINTABLE|ALPHA,		/*  p */
	PRINTABLE|ALPHA,		/*  q */
	PRINTABLE|ALPHA,		/*  r */
	PRINTABLE|ALPHA,		/*  s */
	PRINTABLE|ALPHA,		/*  t */
	PRINTABLE|ALPHA,		/*  u */
	PRINTABLE|ALPHA,		/*  v */
	PRINTABLE|ALPHA,		/*  w */
	PRINTABLE|ALPHA,		/*  x */
	PRINTABLE|ALPHA,		/*  y */
	PRINTABLE|ALPHA,		/*  z */
	PRINTABLE,			/*  { */
	PRINTABLE,			/*  | */
	PRINTABLE,			/*  } */
	PRINTABLE,			/*  ~ */
	IGNORE,				/*  ^? */
};

static int lex_unget_c;

static inline int
lex_get(FILE *in)
{
	int	c;
	if (lex_unget_c) {
		c = lex_unget_c;
		lex_unget_c = 0;
	} else {
		c = getc(in);
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
lexc(FILE *in)
{
	int	c;
	do {
		c = lex_get(in);
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
lex_quoted(FILE *in)
{
	int	c;
	int	v;
	int	count;

	c = lex_get(in);
	if (c == EOF) {
	eof:
		lex_class = ENDOFFILE;
		return 0;
	}
	lex_class = 0;
	c &= 0x7f;
 	switch (c) {
	case 'a':
		return '\a';
	case 'b':
		return '\b';
	case 't':
		return '\t';
	case 'n':
		return '\n';
	case 'r':
		return '\r';
	case 'f':
		return '\f';
	case 'v':
		return '\v';
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
			c = lex_get(in);
			if (c == EOF)
				goto eof;
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

#ifndef AO_SCHEME_TOKEN_MAX
#define AO_SCHEME_TOKEN_MAX	128
#endif

static char	token_string[AO_SCHEME_TOKEN_MAX];
static int32_t	token_int;
static int	token_len;

static void start_token(void) {
	token_len = 0;
}

static void add_token(int c) {
	if (c && token_len < AO_SCHEME_TOKEN_MAX - 1)
		token_string[token_len++] = c;
}

static void end_token(void) {
	token_string[token_len] = '\0';
}

#ifdef AO_SCHEME_FEATURE_FLOAT
static float	token_float;

struct namedfloat {
	const char	*name;
	float		value;
};

static const struct namedfloat namedfloats[] = {
	{ .name = "+inf.0", .value = INFINITY },
	{ .name = "-inf.0", .value = -INFINITY },
	{ .name = "+nan.0", .value = NAN },
	{ .name = "-nan.0", .value = NAN },
};

#define NUM_NAMED_FLOATS	(sizeof namedfloats / sizeof namedfloats[0])
#endif

static int
parse_int(FILE *in, int base)
{
	int	cval;
	int	c;

	token_int = 0;
	for (;;) {
		c = lexc(in);
		if ((lex_class & HEX_DIGIT) == 0) {
			lex_unget(c);
			return NUM;
		}
		if ('0' <= c && c <= '9')
			cval = c - '0';
		else
			cval = (c | ('a' - 'A')) - 'a' + 10;
		token_int = token_int * base + cval;
	}
	return NUM;
}

static int
_lex(FILE *in)
{
	int	c;

	start_token();
	for (;;) {
		c = lexc(in);
		if (lex_class & ENDOFFILE)
			return END;

		if (lex_class & WHITE)
			continue;

		if (lex_class & COMMENT) {
			while ((c = lexc(in)) != '\n') {
				if (lex_class & ENDOFFILE)
					return END;
			}
			continue;
		}

		if (lex_class & SPECIAL) {
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
#ifdef AO_SCHEME_FEATURE_QUASI
			case '`':
				return QUASIQUOTE;
			case ',':
				c = lexc(in);
				if (c == '@') {
					return UNQUOTE_SPLICING;
				} else {
					lex_unget(c);
					return UNQUOTE;
				}
#endif
			}
		}
		if (c == '#') {
			c = lexc(in);
			switch (c) {
			case 't':
				return TRUE_TOKEN;
			case 'f':
				return FALSE_TOKEN;
#ifdef AO_SCHEME_FEATURE_VECTOR
			case '(':
				return OPEN_VECTOR;
#endif
			case '\\':
				for (;;) {
					c = lexc(in);
					if (token_len == 0) {
						add_token(c);
						if (!(lex_class & ALPHA))
							break;
					} else {
						if (lex_class & ALPHA)
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
					ao_scheme_error(AO_SCHEME_INVALID, "invalid character token #\\%s", token_string);
					continue;
				}
				return NUM;
			case 'x':
				return parse_int(in, 16);
			case 'o':
				return parse_int(in, 8);
			case 'b':
				return parse_int(in, 2);
			}
		}
		if (lex_class & STRINGC) {
			for (;;) {
				c = lexc(in);
				if (c == '\\')
					c = lex_quoted(in);
				if (lex_class & (STRINGC|ENDOFFILE)) {
					end_token();
					return STRING;
				}
				add_token(c);
			}
		}
		if (lex_class & PRINTABLE) {
#ifdef AO_SCHEME_FEATURE_FLOAT
			int	isfloat = 1;
			int	epos = 0;
#endif
			int	hasdigit = 0;
			int	isneg = 0;
			int	isint = 1;

			token_int = 0;
			for (;;) {
				if (!(lex_class & NUMBER)) {
					isint = 0;
#ifdef AO_SCHEME_FEATURE_FLOAT
					isfloat = 0;
#endif
				} else {
#ifdef AO_SCHEME_FEATURE_FLOAT
					if (!(lex_class & INTEGER))
						isint = 0;
 					if (token_len != epos &&
					    (lex_class & SIGN))
					{
						isint = 0;
						isfloat = 0;
					}
#endif
					if (c == '-')
						isneg = 1;
#ifdef AO_SCHEME_FEATURE_FLOAT
					if (c == '.' && epos != 0)
						isfloat = 0;
					if (c == 'e' || c == 'E') {
						if (token_len == 0)
							isfloat = 0;
						else
							epos = token_len + 1;
					}
#endif
					if (lex_class & DIGIT) {
						hasdigit = 1;
						if (isint)
							token_int = token_int * 10 + c - '0';
					}
				}
				add_token (c);
				c = lexc (in);
				if ((lex_class & (NOTNAME))
#ifdef AO_SCHEME_FEATURE_FLOAT
				    && (c != '.' || !isfloat)
#endif
					) {
#ifdef AO_SCHEME_FEATURE_FLOAT
					unsigned int u;
#endif
					lex_unget(c);
					end_token ();
					if (isint && hasdigit) {
						if (isneg)
							token_int = -token_int;
						return NUM;
					}
#ifdef AO_SCHEME_FEATURE_FLOAT
					if (isfloat && hasdigit) {
						token_float = strtof(token_string, NULL);
						return FLOAT;
					}
					for (u = 0; u < NUM_NAMED_FLOATS; u++)
						if (!strcmp(namedfloats[u].name, token_string)) {
							token_float = namedfloats[u].value;
							return FLOAT;
						}
#endif
					return NAME;
				}
			}
		}
	}
}

static inline int lex(FILE *in)
{
	int	parse_token = _lex(in);
	RDBGI("token %d \"%s\"\n", parse_token, token_string);
	return parse_token;
}

static int parse_token;

int			ao_scheme_read_list;
struct ao_scheme_cons	*ao_scheme_read_cons;
struct ao_scheme_cons	*ao_scheme_read_cons_tail;
struct ao_scheme_cons	*ao_scheme_read_stack;
static int		ao_scheme_read_state;

#define READ_IN_QUOTE	0x01
#define READ_SAW_DOT	0x02
#define READ_DONE_DOT	0x04
#define READ_SAW_VECTOR	0x08

static int
push_read_stack(int read_state)
{
	RDBGI("push read stack %p 0x%x\n", ao_scheme_read_cons, read_state);
	RDBG_IN();
	if (ao_scheme_read_list) {
		ao_scheme_read_stack = ao_scheme_cons_cons(ao_scheme_cons_poly(ao_scheme_read_cons),
						       ao_scheme_cons(ao_scheme_int_poly(read_state),
								     ao_scheme_cons_poly(ao_scheme_read_stack)));
		if (!ao_scheme_read_stack)
			return 0;
	} else
		ao_scheme_read_state = read_state;
	ao_scheme_read_cons = NULL;
	ao_scheme_read_cons_tail = NULL;
	return 1;
}

static int
pop_read_stack(void)
{
	int	read_state = 0;
	if (ao_scheme_read_list) {
		ao_scheme_read_cons = ao_scheme_poly_cons(ao_scheme_read_stack->car);
		ao_scheme_read_stack = ao_scheme_poly_cons(ao_scheme_read_stack->cdr);
		read_state = ao_scheme_poly_int(ao_scheme_read_stack->car);
		ao_scheme_read_stack = ao_scheme_poly_cons(ao_scheme_read_stack->cdr);
		for (ao_scheme_read_cons_tail = ao_scheme_read_cons;
		     ao_scheme_read_cons_tail && ao_scheme_read_cons_tail->cdr;
		     ao_scheme_read_cons_tail = ao_scheme_poly_cons(ao_scheme_read_cons_tail->cdr))
			;
	} else {
		read_state = ao_scheme_read_state;
		ao_scheme_read_cons = NULL;
		ao_scheme_read_cons_tail = NULL;
		ao_scheme_read_stack = NULL;
		ao_scheme_read_state = 0;
	}
	RDBG_OUT();
	RDBGI("pop read stack %p %d\n", ao_scheme_read_cons, read_state);
	return read_state;
}

#ifdef AO_SCHEME_FEATURE_VECTOR
#define is_open(t) ((t) == OPEN || (t) == OPEN_VECTOR)
#else
#define is_open(t) ((t) == OPEN)
#endif

ao_poly
ao_scheme_read(FILE *in)
{
	struct ao_scheme_atom	*atom;
	struct ao_scheme_string	*string;
	int			read_state;
	ao_poly			v = AO_SCHEME_NIL;

	ao_scheme_read_list = 0;
	read_state = 0;
	ao_scheme_read_cons = ao_scheme_read_cons_tail = ao_scheme_read_stack = NULL;
	for (;;) {
		parse_token = lex(in);
		while (is_open(parse_token)) {
#ifdef AO_SCHEME_FEATURE_VECTOR
			if (parse_token == OPEN_VECTOR)
				read_state |= READ_SAW_VECTOR;
#endif
			if (!push_read_stack(read_state))
				return AO_SCHEME_NIL;
			ao_scheme_read_list++;
			read_state = 0;
			parse_token = lex(in);
		}

		switch (parse_token) {
		case END:
		default:
			if (ao_scheme_read_list)
				ao_scheme_error(AO_SCHEME_EOF, "unexpected end of file");
			return _ao_scheme_atom_eof;
			break;
		case NAME:
			atom = ao_scheme_atom_intern(token_string);
			if (atom)
				v = ao_scheme_atom_poly(atom);
			else
				v = AO_SCHEME_NIL;
			break;
		case NUM:
			v = ao_scheme_integer_poly(token_int);
			break;
#ifdef AO_SCHEME_FEATURE_FLOAT
		case FLOAT:
			v = ao_scheme_float_get(token_float);
			break;
#endif
		case TRUE_TOKEN:
			v = _ao_scheme_bool_true;
			break;
		case FALSE_TOKEN:
			v = _ao_scheme_bool_false;
			break;
		case STRING:
			string = ao_scheme_string_new(token_string);
			if (string)
				v = ao_scheme_string_poly(string);
			else
				v = AO_SCHEME_NIL;
			break;
		case QUOTE:
#ifdef AO_SCHEME_FEATURE_QUASI
		case QUASIQUOTE:
		case UNQUOTE:
		case UNQUOTE_SPLICING:
#endif
			if (!push_read_stack(read_state))
				return AO_SCHEME_NIL;
			ao_scheme_read_list++;
			read_state = READ_IN_QUOTE;
			switch (parse_token) {
			case QUOTE:
				v = _ao_scheme_atom_quote;
				break;
#ifdef AO_SCHEME_FEATURE_QUASI
			case QUASIQUOTE:
				v = _ao_scheme_atom_quasiquote;
				break;
			case UNQUOTE:
				v = _ao_scheme_atom_unquote;
				break;
			case UNQUOTE_SPLICING:
				v = _ao_scheme_atom_unquote2dsplicing;
				break;
#endif
			}
			break;
		case CLOSE:
			if (!ao_scheme_read_list) {
				v = AO_SCHEME_NIL;
				break;
			}
			v = ao_scheme_cons_poly(ao_scheme_read_cons);
			--ao_scheme_read_list;
			read_state = pop_read_stack();
#ifdef AO_SCHEME_FEATURE_VECTOR
			if (read_state & READ_SAW_VECTOR) {
				v = ao_scheme_vector_poly(ao_scheme_list_to_vector(ao_scheme_poly_cons(v)));
				read_state &= ~READ_SAW_VECTOR;
			}
#endif
			break;
		case DOT:
			if (!ao_scheme_read_list) {
				ao_scheme_error(AO_SCHEME_INVALID, ". outside of cons");
				return AO_SCHEME_NIL;
			}
			if (!ao_scheme_read_cons) {
				ao_scheme_error(AO_SCHEME_INVALID, ". first in cons");
				return AO_SCHEME_NIL;
			}
			read_state |= READ_SAW_DOT;
			continue;
		}

		/* loop over QUOTE ends */
		for (;;) {
			if (!ao_scheme_read_list)
				return v;

			if (read_state & READ_DONE_DOT) {
				ao_scheme_error(AO_SCHEME_INVALID, ". not last in cons");
				return AO_SCHEME_NIL;
			}

			if (read_state & READ_SAW_DOT) {
				read_state |= READ_DONE_DOT;
				ao_scheme_read_cons_tail->cdr = v;
			} else {
				struct ao_scheme_cons	*read = ao_scheme_cons_cons(v, AO_SCHEME_NIL);
				if (!read)
					return AO_SCHEME_NIL;

				if (ao_scheme_read_cons_tail)
					ao_scheme_read_cons_tail->cdr = ao_scheme_cons_poly(read);
				else
					ao_scheme_read_cons = read;
				ao_scheme_read_cons_tail = read;
			}

			if (!(read_state & READ_IN_QUOTE) || !ao_scheme_read_cons->cdr)
				break;

			v = ao_scheme_cons_poly(ao_scheme_read_cons);
			--ao_scheme_read_list;
			read_state = pop_read_stack();
		}
	}
	return v;
}
