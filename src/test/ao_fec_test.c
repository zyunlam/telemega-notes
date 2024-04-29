/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include <ao_fec.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#ifndef RANDOM_MAX
#define RANDOM_MAX 0x7fffffff
#endif

static double
rand_real(void) {
	return (double) random() / (double) RANDOM_MAX;
}

static double
gaussian_random(double mean, double dev)
{
	static int	save_x_valid = 0;
	static double	save_x;
	double		x;

	if (save_x_valid)
	{
		x = save_x;
		save_x_valid = 0;
	}
	else
	{
		double    w;
		double    normal_x1, normal_x2;

		do {
			normal_x1 = 2 * rand_real () - 1;
			normal_x2 = 2 * rand_real () - 1;
			w = normal_x1*normal_x1 + normal_x2*normal_x2;
		} while (w >= 1 || w < 1E-30);

		w = sqrt(log(w)*(-2./w));

		/*
		 * normal_x1 and normal_x2 are independent normally
		 * distributed variates
		 */

		x = normal_x1 * w;
		/* save normal_x2 for next call */
		save_x = normal_x2 * w;
		save_x_valid = 1;
	}
	return x * dev + mean;
}

#define PREPARE_LEN(input_len)		((input_len) + AO_FEC_PREPARE_EXTRA)
#define ENCODE_LEN(input_len)		(PREPARE_LEN(input_len) * 2)
#define DECODE_LEN(input_len)		((input_len) + AO_FEC_PREPARE_EXTRA)
#define EXPAND_LEN(input_len)		(ENCODE_LEN(input_len) * 8)

static uint8_t ao_bit(uint8_t b) {
	if (b)
		return 0x00;
	return 0xff;
}

static int
ao_expand(uint8_t *bits, int bits_len, uint8_t *bytes)
{
	int	i, bit;
	uint8_t	b;

	for (i = 0; i < bits_len; i++) {
		b = bits[i];
		for (bit = 7; bit >= 0; bit--)
			*bytes++ = ao_bit ((b >> bit) & 1);
	}

	return bits_len * 8;
}

static int
ao_fuzz (uint8_t *in, int in_len, uint8_t *out, double dev)
{
	int	i;
	int	errors = 0;
	int	s;
	
	for (i = 0; i < in_len; i++) {
		double	error = gaussian_random(0, dev);
		uint8_t	byte = in[i];

		if (error > 0) {
			if (error > 0xff)
				error = 0xff;
			if (error >= 0x80)
				errors++;
			if (byte < 0x80)
				byte += error;
			else
				byte -= error;
		}

		/* abcd efgh	8
		 * abcd efga	7
		 * abcd efab	6
		 * abcd eabc	5
		 * abcd abcd	4
		 * abca bcab	3
		 * abab abab	2
		 * aaaa aaaa	1
		 */

#define SAVE		8
#define SAVE_MASK	(((1 << SAVE) - 1) << (8 - SAVE))

		byte &= SAVE_MASK;
		for (s = SAVE; s < 8; s += SAVE)
			byte |= byte >> s;

		out[i] = byte;
	}
	return errors;
}

static uint8_t
ao_random_data(uint8_t	*out, uint8_t out_len)
{
	uint8_t	len = random() % (out_len + 1);
	uint8_t	i;
	
	for (i = 0; i < len; i++)
		out[i] = random();
	return len;
}	


static uint8_t real_packet[] = {
 0x40, 0x38, 0xcd, 0x38, 0x3d, 0x34, 0xca, 0x31, 0xc3, 0xc1, 0xc6, 0x35, 0xcc, 0x3a, 0x3c,
 0x3c, 0x3d, 0x3c, 0x37, 0xc5, 0xc1, 0xc0, 0xc1, 0xc1, 0xc3, 0xc0, 0xc1, 0xc6, 0x38, 0x3b, 0xc6,
 0xc0, 0xc6, 0x32, 0xc9, 0xc9, 0x34, 0xcf, 0x35, 0xcf, 0x3a, 0x3b, 0xc6, 0xc7, 0x35, 0xcf, 0x36,
 0xce, 0x37, 0xc8, 0xc8, 0x3a, 0x3c, 0xc9, 0xc8, 0x3a, 0x3c, 0xcc, 0x32, 0xcd, 0x32, 0xce, 0x32,
 0xc9, 0xc6, 0x37, 0x3e, 0x3d, 0xc8, 0xc3, 0xc6, 0x38, 0x3d, 0xcf, 0x34, 0x3d, 0x3b, 0xcb, 0x31,
 0xca, 0x35, 0x38, 0xcb, 0x31, 0xcc, 0x31, 0x3b, 0xc7, 0xbf, 0xbe, 0xbe, 0xc3, 0x35, 0x38, 0xcb,
 0x2f, 0xc9, 0xc2, 0x34, 0x3c, 0xcc, 0x31, 0xca, 0xc1, 0xc0, 0xc3, 0x33, 0x3f, 0x3f, 0x3e, 0x3c,
 0xcc, 0x31, 0xcc, 0xc3, 0xc4, 0x32, 0xd1, 0x34, 0x3f, 0x3c, 0xcc, 0x31, 0xcc, 0xc3, 0x33, 0xd0,
 0x31, 0xd1, 0x35, 0x3f, 0x3e, 0x3c, 0xca, 0xc5, 0x34, 0xcc, 0xc2, 0xc3, 0xc3, 0xc5, 0x36, 0x40,
 0x3e, 0x41, 0x3e, 0x41, 0x3e, 0x3f, 0x41, 0x40, 0xce, 0xc5, 0x33, 0xcf, 0xc7, 0x36, 0xd2, 0x32,
 0xd1, 0xc5, 0xc6, 0x35, 0xd4, 0x32, 0xd1, 0xc6, 0x34, 0x41, 0xd2, 0x30, 0xd0, 0xc2, 0xc2, 0xc5,
 0x31, 0xd3, 0x31, 0xd1, 0xc7, 0x33, 0xd4, 0xc6, 0x34, 0x42, 0x40, 0x40, 0x40, 0x41, 0x41, 0xd2,
 0xc4, 0xc4, 0x39, 0x3e, 0xce, 0x35, 0x3c, 0xcd, 0x31, 0x3d, 0xc8, 0xc3, 0x32, 0x3d, 0xcc, 0x2f,
 0xce, 0x30, 0xd0, 0x2e, 0xcf, 0x30, 0x3d, 0x3c, 0x3c, 0x3a, 0xcb, 0xbf, 0xbf, 0xc1, 0x31, 0x3d,
 0x3d, 0x3c, 0x3c, 0x3c, 0x3d, 0x3b, 0xcf, 0x2e, 0xd2, 0x2e, 0xd0, 0x2c, 0x3b, 0xd1, 0x2a, 0xcf,
 0xbe, 0xc0, 0xc0, 0xbf, 0xc1, 0xc1, 0x2c, 0x3f, 0xd0, 0xc0, 0x2e, 0xd0, 0x31, 0xd2, 0xc2, 0x2f,
 0x3e, 0x3e, 0x3d, 0xcf, 0x31, 0xcb, 0xc3, 0x32, 0xd1, 0xc4, 0x37, 0x3f, 0xce, 0xc4, 0x35, 0xd0,
 0x33, 0x3f, 0xce, 0x31, 0x3c, 0x3b, 0xcc, 0x31, 0xcd, 0x2f, 0xd1, 0x32, 0x3a, 0x3b, 0xcb, 0xc1,
 0xc0, 0x32, 0x3e, 0x3e, 0x3c, 0x3d, 0x3d, 0xce, 0x2d, 0xcd, 0xc3, 0xc2, 0x30, 0x3d, 0xcf, 0xc1,
 0xc2, 0x30, 0xd0, 0xc4, 0x31, 0xd4, 0x30, 0x40, 0x3e, 0xd0, 0x2f, 0xd0, 0x2f, 0xd1, 0xc2, 0x2e,
 0xd4, 0x2e, 0x3f, 0xce, 0xc2, 0x34, 0x3e, 0x3f, 0xd0, 0x30, 0xcf, 0x31, 0x3d, 0x3d, 0xcc, 0x2d,
 0xcf, 0x2f, 0xcf, 0x2e, 0x3b, 0xcf, 0x2c, 0x3b, 0x3b, 0x3a, 0x3d, 0x38, 0xcc, 0x2d, 0x3b, 0xcc,
 0xbe, 0x2d, 0xd1, 0x2c, 0x3c, 0x3d, 0xce, 0xc0, 0x2d, 0xd1, 0x2f, 0x3f, 0x3f, 0x3c, 0x3f, 0x3e,
 0x3e, 0x3c, 0xd3, 0x2a, 0xd1, 0xc2, 0x2e, 0x3c, 0xd1, 0x2f, 0x3d, 0xd1, 0xbe, 0xc1, 0x2d, 0xd2,
 0x2d, 0x3d, 0x3d, 0x3b, 0xcd, 0x31, 0xcc, 0x31, 0xce, 0x2e, 0x3d, 0x3e, 0x3a, 0xcd, 0x2d, 0xcc,
 0xc1, 0xc1, 0xc3, 0x2e, 0x3f, 0x3f, 0x3c, 0xcf, 0xc0, 0x31, 0xd1, 0xc2, 0xc3, 0x33, 0xd2, 0xc7,
 0x32, 0x40, 0xd3, 0xc4, 0xc4, 0x33, 0x40, 0x40, 0xd2, 0x2f, 0xd0, 0x2f, 0x3c, 0xd1, 0x2c, 0x3a,
 0x3d, 0xd0, 0x2a, 0xd0, 0x28, 0xcf, 0xc3, 0x2c, 0xd2, 0x2d, 0xd3, 0xc3, 0xc3, 0x2e, 0x3e, 0x41,
 0xd2, 0xc3, 0xc2, 0xc2, 0xc2, 0xc2, 0xc4, 0x32, 0xd2, 0x34, 0x3e, 0x3e, 0x3c, 0xd1, 0x30, 0x3d,
 0x3c, 0xce, 0x2e, 0x3b, 0x38, 0xcb, 0xbe, 0xc1, 0x2e, 0x3e, 0x3c, 0x3d, 0xd1, 0x2c, 0x3b, 0x3b,
 0xcd, 0xbf, 0x2d, 0xd0, 0x2f, 0xd0, 0x2d, 0xd1, 0x2d, 0x3d, 0xd0, 0x2b, 0x3b, 0xcf, 0x2b, 0x3a,
 0xcc, 0xbc, 0xc1, 0x2a, 0x3c, 0xce, 0x28, 0xd1, 0x2a, 0x3c, 0x3a, 0xcf, 0xbf, 0x2b, 0x3e, 0x3c,
 0xd2, 0x2b, 0x3d, 0x3a, 0x3a, 0xcf, 0x2c, 0xcc, 0xbe, 0xc1, 0xc0, 0xbf, 0xc1, 0x31, 0x3c, 0xce,
 0xc0, 0xc1, 0xc0, 0xc1, 0x31, 0x3c, 0xd1, 0xc2, 0x2e, 0xd1, 0xc3, 0xc4, 0x30, 0x3f, 0xd3, 0x2c,
 0xd3, 0xc2, 0x30, 0xd5, 0xc3, 0xc5, 0x30, 0xd5, 0xc4, 0xc5, 0xc5, 0x31, 0x41, 0xd4, 0xc4, 0x31,
 0x40, 0xd4, 0x2d, 0xd5, 0xc0, 0xc3, 0x2c, 0x3f, 0x3e, 0x3f, 0x3f, 0x3f, 0xd6, 0x2c, 0x3e, 0xd2,
 0x27, 0x37, 0xde, 0xc9, 0xe9, 0x2e, 0xc7, 0x3c, 0xd9, 0x07, 0xf3, 0x28, 0x8d, 0xa5, 0xc9, 0xca,
 0xe7, 0xcb, 0xfc, 0xb3, 0x3c, 0xd4, 0xd3, 0x9a, 0xe7, 0x2c, 0xc8, 0xf8, 0x44, 0xb3, 0xb0, 0xfa,
 0x1c, 0xbf, 0xc9, 0xff, 0xbb, 0x1d, 0x02, 0xdb, 0x45, 0xc1, 0x40, 0x1c, 0xec, 0x48, 0xda, 0x21,
 0x4c, 0xe3, 0x38, 0xdf, 0x34, 0xd8, 0x35, 0xd8, 0x31, 0xd7, 0x30, 0xd4, 0x2f, 0xd6, 0x2d, 0xd5,
 0x2b, 0xd5, 0x33, 0xce, 0x33, 0xce, 0x33, 0xd0, 0x34, 0xcf, 0x31, 0xcf, 0x30, 0xce, 0x30, 0xcf,
 0x30, 0x3d, 0xce, 0x2f, 0xcf, 0xc2, 0x30, 0x3f, 0x3d, 0xcf, 0xc1, 0x31, 0xd0, 0xc5, 0xc4, 0x33,
 0x41, 0x3e, 0xd1, 0x30, 0x3c, 0x3d, 0xce, 0x2f, 0xce, 0xc1, 0xc3, 0x2e, 0xd3, 0x30, 0x3e, 0x3e,
 0x3c, 0x3f, 0x3c, 0xd1, 0xc0, 0xc0, 0xc0, 0xc1, 0xc2, 0xc0, 0xc1, 0xc3, 0x2c, 0x3f, 0xd2, 0xbe,
 0xc1, 0x2e, 0xcf, 0xc4, 0x33, 0xd3, 0x34, 0xd0, 0x33, 0x3d, 0xcf, 0xc3, 0x32, 0xce, 0x33, 0xd2,
 0x32, 0xce, 0xc5, 0x34, 0x40, 0xce, 0xc5, 0x32, 0x3e, 0xd0, 0x31, 0xd0, 0x2f, 0xd2, 0x30, 0xce,
 0xc2, 0x33, 0x40, 0x3e, 0xd0, 0xc3, 0xc2, 0x2e, 0xd3, 0x31, 0xd0, 0xc7, 0x32, 0x40, 0x3e, 0xd3,
 0x2f, 0xd3, 0x2e, 0xd0, 0x2d, 0xd3, 0x2d, 0xd3, 0x2f, 0x3c, 0x3d, 0xd1, 0x2a, 0x3b, 0xd1, 0x28,
 0x3b, 0x3b, 0xc8, 0x31, 0x39, 0x3b, 0x38, 0xc6, 0xbf, 0x31, 0x3d, 0x3a, 0xca, 0xc0, 0x33, 0x3c,
 0xca, 0xc0, 0xc1, 0xc1, 0xc2, 0x35, 0x3e, 0x3c, 0x3d, 0x3f, 0xcc, 0xc0, 0xc3, 0x31, 0xce, 0xc5,
 0x33, 0xd0, 0xc4, 0x35, 0xd3, 0x33, 0xd3, 0x32, 0xd1, 0xc4, 0xc3, 0x35, 0xd3, 0xc6, 0xc4, 0x35,
 0xd2, 0xc6, 0x35, 0x41, 0x41, 0xd4, 0x33, 0xd1, 0xc4, 0x30, 0x41, 0xd2, 0x30, 0x3e, 0xce, 0xc1,
 0xc3, 0xc0, 0x31, 0xce, 0xc5, 0x34, 0x40, 0x3d, 0xd1, 0xc4, 0x32, 0x3e, 0xcf, 0xc2, 0xc3, 0x30,
 0xd4, 0x32, 0x3e, 0x3f, 0x3e, 0x3f, 0x3e, 0xd0, 0xc2, 0x31, 0x3e, 0x3f, 0x3f, 0xd1, 0xc2, 0x2f,
 0xd5, 0x2e, 0xd3, 0xc3, 0x2f, 0xd7, 0x30, 0x3e, 0xd2, 0x2c, 0x3e, 0x3c, 0xd0, 0xc1, 0xc1, 0xc3,
 0xc0, 0xc3, 0x2c, 0xd4, 0xc2, 0x2e, 0x40, 0xd2, 0xc3, 0x2d, 0xd5, 0xc3, 0x2f, 0x42, 0x40, 0xd6,
 0x2d, 0xd4, 0xc2, 0xc2, 0xc2, 0x32, 0xd4, 0xc3, 0xc5, 0xc4, 0x32, 0x40, 0xd3, 0x30, 0xd0, 0xc3,
 0xc4, 0x30, 0xd3, 0xc5, 0xc5, 0xc4, 0xc4, 0x33, 0x40, 0x40, 0xd4, 0x2f, 0xd2, 0x2d, 0x3d, 0xd1,
 0xc1, 0xc2, 0x2c, 0xd4, 0x2e, 0xd4, 0x2d, 0x3d, 0xd3, 0xc1, 0xc1, 0xc1, 0x2d, 0xd5, 0xc2, 0xc2,
 0xc2, 0x2d, 0xd9, 0xc4, 0xc5, 0x2f, 0x43, 0x40, 0xd6, 0xd7, 0xd7, 0x2b, 0x3e, 0xd5, 0x29, 0x3d,
 0xd4, 0x24, 0x3b, 0x3a, 0x3b, 0xce, 0x2a, 0x3a, 0xcc, 0xbe, 0x2d, 0x3b, 0x3b, 0x3d, 0x3b, 0x3b,
 0xce, 0x2b, 0x3a, 0xcc, 0x2b, 0xd0, 0x2b, 0x3b, 0x3b, 0x39, 0xcf, 0xbf, 0x2a, 0xd1, 0xc0, 0x2f,
 0x3e, 0xd0, 0x2d, 0x3a, 0xd1, 0xbe, 0x2b, 0xd2, 0xc3, 0xc2, 0xc0, 0x2d, 0xd7, 0x2c, 0xd7, 0xc2,
 0xc3, 0x2f, 0x43, 0x41, 0x40, 0xd4, 0xc3, 0xc3, 0xc3, 0xc2, 0x2c, 0x40, 0xd7, 0x2a, 0x3d, 0xd3,
 0x26, 0xd5, 0x2f, 0x3a, 0x3c, 0xca, 0xc3, 0x2c, 0xd3, 0x2e, 0x3c, 0x3d, 0x3d, 0x3d, 0xd0, 0xc2,
 0x2e, 0xd3, 0x2e, 0xd1, 0xc5, 0x2e, 0x3f, 0xd3, 0x2e, 0x3c, 0xd1, 0xc0, 0xc2, 0x2a, 0xd2, 0xc2,
 0xc4, 0x2e, 0xd7, 0x2e, 0x3e, 0xd3, 0xc1, 0xc3, 0xc1, 0x2d, 0x3f, 0xd3, 0xc2, 0x2c, 0x3f, 0xd3,
 0x2b, 0x3b, 0xd1, 0xbf, 0xc1, 0xbe, 0x29, 0x3f, 0xd4, 0x29, 0xd5, 0xbf, 0x29, 0xd7, 0xc0, 0x2d,
 0xd8, 0xc2, 0x33, 0x41, 0xd3, 0x30, 0x3d, 0xd0, 0x2c, 0xcf, 0xc1, 0xc1, 0xc2, 0x2e, 0x3f, 0xd3,
 0x2b, 0xcf, 0xc3, 0x2f, 0x40, 0x3f, 0x3e, 0xd3, 0x2a, 0x3d, 0x3d, 0xce, 0xc1, 0x2b, 0x3d, 0xd0,
 0x2b, 0xd3, 0x2b, 0x3b, 0x3d, 0xce, 0x29, 0x3b, 0x3b, 0x3b, 0xd1, 0xbe, 0x2b, 0xd2, 0x29, 0x3d,
 0x3a, 0xd3, 0x29, 0x3a, 0x3b, 0x3b, 0xd2, 0x26, 0x3b, 0xd0, 0xbf, 0xbe, 0xbc, 0xbf, 0xbe, 0xbc,
 0x27, 0x3d, 0x3a, 0x3c, 0xce, 0xc1, 0x2c, 0xd2, 0xc2, 0xc2, 0x2f, 0x40, 0xd2, 0xc2, 0xc2, 0x2e,
 0x40, 0xd4, 0x2a, 0x3b, 0xcf, 0x2b, 0xd2, 0x2a, 0x3c, 0xd0, 0xc1, 0x2a, 0x3d, 0xd0, 0xc1, 0xc0,
 0xbe, 0x2b, 0x3f, 0x3e, 0xd2, 0xc1, 0xc0, 0xc1, 0xc0, 0xc2, 0x2a, 0xd6, 0xc2, 0xc3, 0xc3, 0xc3,
 0x2c, 0x3e, 0x41, 0xd6, 0xc0, 0xc3, 0xc2, 0xc2, 0x2a, 0xd9, 0x28, 0x3f, 0xd5, 0xc1, 0xc2, 0xc0,
 0xc3, 0x28, 0xd5, 0xc2, 0xc5, 0x30, 0xd6, 0xc4, 0xc5, 0xc5, 0x31, 0x43, 0xd4, 0xc3, 0xc5, 0xc2,
 0xc2, 0x2f, 0xd6, 0xc5, 0xc2, 0x2d, 0x41, 0x41, 0x43, 0x40, 0x41, 0xd8, 0x2a, 0x3f, 0x3f, 0x3e,
 0x2c, 0x0a, 0x0d, 0x2a, 0xbb, 0x91, 0x5e, 0x2a, 0xca, 0x23, 0xf0, 0x0f, 0xbe, 0xd6, 0xfb, 0xc0,
 0x2c, 0x34, 0x7f, 0xd1, 0xc9, 0xc1, 0x1c, 0x06, 0x97, 0x1f, 0x21, 0xa8, 0x04, 0x5d, 0x07, 0xb0,
 0x49, 0xaf, 0x10, 0x60, 0xd1, 0xf1, 0xf3, 0xcb, 0x72, 0x16, 0x24, 0xe9, 0xd4, 0x28, 0xb2, 0x8c,
 0xf3, 0x3b, 0xe8, 0x06, 0xeb, 0x09, 0xec, 0x0a, 0xec, 0x0b, 0xee, 0x0b, 0xee, 0x0b, 0xf0, 0x0b,
 0xf0, 0x0a, 0xf3, 0x0b, 0xf2, 0x0a, 0xf5, 0x08, 0xf5, 0x0a, 0xf4, 0x08, 0xf5, 0x08, 0xf5, 0x09,
 0xf4, 0x07, 0x3f, 0xf5, 0x05, 0xf6, 0xbf, 0x06, 0x3f, 0x3f, 0xf8, 0xbe, 0x07, 0xf8, 0xc3, 0xc3,
 0x06, 0x41, 0x40, 0xf0, 0x0f, 0x3c, 0x3d, 0xef, 0x0e, 0xee, 0xbe, 0xbf, 0x11, 0xf2, 0x0e, 0x3e,
 0x3c, 0x3f, 0x3e, 0x3e, 0xf0, 0xc1, 0xc1, 0xc1, 0xc1, 0xc3, 0xc0, 0xc1, 0xc1, 0x0b, 0x3f, 0xf5,
 0xc0, 0xc0, 0x08, 0xf7, 0xc2, 0x0c, 0xf6, 0x0d, 0xf9, 0x0a, 0x3f, 0xf9, 0xc0, 0x0b, 0xf6, 0x06,
 0xf8, 0x06, 0xf9, 0xc3, 0x08, 0x40, 0xfa, 0xc2, 0x09, 0x3e, 0xf7, 0x05, 0xf6, 0x04, 0xf7, 0x05,
 0xf8, 0xc1, 0xc3, 0xc1, 0x0a, 0x3e, 0x41, 0xf6, 0x07, 0xf7, 0x08, 0xf6, 0x07, 0x3e, 0x3c, 0x3f,
 0xf4, 0xbf, 0xbe, 0x06, 0xf8, 0x07, 0xf8, 0x05, 0xfb, 0xc0, 0xc1, 0x02, 0x40, 0x40, 0x3e, 0xfb,
 0x02, 0x3e, 0x3e, 0xf6, 0x01, 0xf9, 0x00, 0x3f, 0xf8, 0xbd, 0x02, 0xfc, 0xc1, 0xc0, 0x03, 0xfe,
 0xc5, 0xc4, 0xc2, 0xc5, 0xc2, 0x02, 0x41, 0x40, 0x40, 0x41, 0x40, 0xff, 0xff, 0x40, 0xfd, 0xfd,
 0xfc, 0xfd, 0x3e, 0xfc, 0xbe, 0xbf, 0xfe, 0xfc, 0xc1, 0xc0, 0xc1, 0xc1, 0x00, 0x01, 0x01, 0x02,
 0xc0, 0x00, 0x02, 0xc3, 0xc5, 0x02, 0x07, 0xc4, 0xc6, 0xc4, 0x02, 0x06, 0xc3, 0x01, 0x43, 0x43,
 0x42, 0x05, 0xfc, 0x04, 0xfd, 0x41, 0x3e, 0x05, 0xbf, 0xfb, 0x41, 0x3e, 0x3e, 0x3f, 0x3f, 0x40,
 0x02, 0xf9, 0x3e, 0x03, 0xbf, 0xbe, 0xf6, 0x3f, 0x05, 0xf6, 0x3c, 0x05, 0xbd, 0xbf, 0xbe, 0xf2,
 0x3f, 0x3c, 0x3d, 0x3e, 0x08, 0xee, 0x3d, 0x3b, 0x07, 0xbd, 0xf3, 0x0e, 0xc0, 0xc0, 0xf2, 0x0f,
 0xf2, 0x40, 0x0d, 0xef, 0x3f, 0x3e, 0x0a, 0xbf, 0xf0, 0x0e, 0xf1, 0x40, 0x0a, 0xef, 0x0e, 0xc1,
 0xc0, 0xc1, 0xc2, 0xc0, 0xee, 0x13, 0xc1, 0xee, 0x43, 0x10, 0xea, 0x15, 0xc0, 0xc0, 0xc0, 0xc3,
 0xc0, 0xec, 0x15, 0xc2, 0xf0, 0x16, 0xc2, 0xef, 0x1b, 0xc2, 0xf1, 0x44, 0x42, 0x19, 0xc2, 0xc3,
 0xed, 0x17, 0xe7, 0x21, 0xe4, 0x40, 0x3f, 0x20, 0xc0, 0xe2, 0x40, 0x3e, 0x1d, 0xc1, 0xc2, 0xc0,
 0xe2, 0x20, 0xe2, 0x3f, 0x3f, 0x3e, 0x1c, 0xdf, 0x3c, 0x3f, 0x1e, 0xbf, 0xc1, 0xbe, 0xdf, 0x3e,
 0x3d, 0x1f, 0xdc, 0x3c, 0x3b, 0x3d, 0x3a, 0x3d, 0x1f, 0xdb, 0x1e, 0xda, 0x20, 0xda, 0x21, 0xc1,
 0xc0, 0xc1, 0xdc, 0x3e, 0x23, 0xda, 0x3e, 0x20, 0xbf, 0xbd, 0xdb, 0x23, 0xbf, 0xdf, 0x26, 0xdc,
 0x26, 0xdc, 0x2e, 0xc2, 0xc3, 0xc3, 0xd4, 0x40, 0x2f, 0xc2, 0xd2, 0x3f, 0x3f, 0x3f, 0x2e, 0xd1,
 0x2d, 0xd3, 0x2c, 0xc3, 0xc3, 0xc3, 0xc2, 0xc3, 0xc2, 0xd3, 0x3e, 0x31, 0xd0, 0x2d, 0xc3, 0xc0,
 0xc1, 0xd1, 0x2f, 0xd3, 0x3f, 0x3f, 0x31, 0xc1, 0xcf, 0x31, 0xd2, 0x3e, 0x3f, 0x3c, 0x41, 0x3e,
 0x3e, 0x3f, 0x31, 0xd1, 0x30, 0xc0, 0xcf, 0x33, 0xd1, 0x3d, 0x41, 0x32, 0xc0, 0xc3, 0xce, 0x3e,
 0x32, 0xc3, 0xc3, 0xc2, 0xc0, 0xc3, 0xca, 0x3c, 0x37, 0xc5, 0xc3, 0xca, 0x34, 0xcf, 0x35, 0xc6,
 0xca, 0x3c, 0x39, 0xce, 0x3c, 0x37, 0xc2, 0xc8, 0x3c, 0x36, 0xcc, 0x30, 0xcb, 0x33, 0xc6, 0xc3,
 0xc8, 0x34, 0xcc, 0x3d, 0x37, 0xc5, 0xc9, 0x38, 0xc6, 0xca, 0x3b, 0x40, 0x40, 0x40, 0x3a, 0xc5,
 0xc8, 0x3f, 0x3e, 0x3b, 0xce, 0x3a, 0x3e, 0x38, 0xca, 0x39, 0x3c, 0x3c, 0x36, 0xcb, 0x36, 0x3b,
 0x37, 0xc8, 0x35, 0x3a, 0x36, 0xc8, 0x2f, 0xc4, 0xc5, 0x36, 0x38, 0xc7, 0xc0, 0xc7, 0x33, 0xc8,
 0xc3, 0xc5, 0x39, 0x3b, 0xc9, 0xc7, 0x34, 0xd1, 0x34, 0xcf, 0x35, 0xca, 0xc9, 0x36, 0xd0, 0x36,
 0xcc, 0xc7, 0x39, 0x41, 0x42, 0x42, 0x3e, 0xca, 0xc1, 0xc3, 0xc3, 0xc2, 0xc3, 0xc2, 0xc4, 0xc6,
 0x34, 0xd1, 0x35, 0xcf, 0x37, 0x3c, 0xce, 0x35, 0x3b, 0xcc, 0x30, 0xca, 0x33, 0x39, 0xc9, 0xbf,
 0xbf, 0xbf, 0xc3, 0x34, 0x3c, 0x3f, 0x3a, 0xcc, 0x2f, 0xcd, 0x32, 0x3c, 0x3a, 0xc8, 0xc2, 0x34,
 0x3a, 0xcb, 0xc0, 0xc0, 0x35, 0x3c, 0xce, 0x32, 0x3c, 0x3c, 0x3d, 0x3a, 0xcd, 0x2e, 0x3b, 0x39,
 0xc9, 0xbc, 0xbf, 0x30, 0xcc, 0xc0, 0xc3, 0x33, 0xce, 0xc5, 0xc2, 0xc4, 0x36, 0x3e, 0xd1, 0xc3,
 0xc2, 0xc3, 0xc5, 0x33, 0xd0, 0xc5, 0xc4, 0x35, 0x40, 0x40, 0x3e, 0x41, 0x3e, 0xd2, 0x30, 0x3f,
 0x3e, 0x3d, 0x21, 0xc8, 0x0d, 0x23, 0xec, 0xb1, 0x1a, 0xf0, 0xf3, 0x10, 0xc6, 0xbd, 0x5b, 0x0d,
 0xff, 0xe2, 0xe6, 0xd4, 0x72, 0xe2, 0xda, 0x0c, 0xf8, 0x1b, 0x04, 0x0b, 0xf7, 0xb4, 0xf0, 0x44,
 0xcd, 0xaf, 0x12, 0x1f, 0x11, 0xa1, 0x29, 0xb8, 0x1a, 0xdb, 0x09, 0x69, 0xf7, 0x20, 0xc4, 0x1c,
 0xc0, 0xc8, 0x1d, 0x45, 0xd9, 0x30, 0xd8, 0x2e, 0xd8, 0x2e, 0xd6, 0x2c, 0xd5, 0x2d, 0xd4, 0x2b,
 0xd5, 0x2b, 0xd1, 0x31, 0xd0, 0x32, 0xd2, 0x30, 0xd0, 0x31, 0xd0, 0x31, 0xce, 0x2f, 0xd3, 0x2e,
 0xce, 0x2f, 0xd1, 0x31, 0x3c, 0xd1, 0x2d, 0xd0, 0xc0, 0x2e, 0x3c, 0x3d, 0xd1, 0xc3, 0x2c, 0xd2,
 0xc3, 0xc5, 0x31, 0x40, 0x40, 0xd5, 0x2c, 0x3a, 0x3d, 0xd0, 0x2b, 0xd3, 0xc0, 0xc0, 0x2d, 0xd4,
 0x2c, 0x3f, 0x3e, 0x3c, 0x3f, 0x3c, 0xd2, 0xc1, 0xc2, 0xc0, 0xc1, 0xc1, 0xc1, 0xc1, 0xc1, 0x2a,
};


int
ao_real_packet(void)
{
	uint8_t	decode[64];
	int	ok;

	ok = ao_fec_decode(real_packet, 576, decode, 34, NULL);

	if (ok && decode[33] == AO_FEC_DECODE_CRC_OK) {
		printf ("match\n");

		ao_fec_dump_bytes(decode, 34, "Decode");
	} else {
		printf ("actual packet crc error\n");
		ok = 0;
	}
	return ok;
}

int
ao_hello_packet(void)
{
	uint8_t message[5] = "hello";
	uint8_t	encode[ENCODE_LEN(sizeof(message))];
	int encode_len;
	uint8_t transmit[EXPAND_LEN(sizeof(message))];
	uint8_t decode[DECODE_LEN(sizeof(message))];
	int transmit_len;
	int decode_ok;

	printf("Hello packet test:\n");
	ao_fec_dump_bytes(message, sizeof(message), "Message");
	encode_len = ao_fec_encode(message, sizeof(message), encode);
	ao_fec_dump_bytes(encode, encode_len, "Encode");
	transmit_len = ao_expand(encode, encode_len, transmit);
	ao_fec_dump_bytes(transmit, transmit_len, "Transmit");
	decode_ok = ao_fec_decode(transmit, transmit_len, decode, sizeof(message) + 2, NULL);
	ao_fec_dump_bytes(decode, sizeof(message) + 2, "Receive");
	printf("Hello result: %s\n", decode_ok ? "success" : "fail");
	return decode_ok;
}

#define EXPECT_DECODE_FAIL	0
#define EXPECT_CRC_MISMATCH	6304
#define EXPECT_DATA_MISMATCH	0
#define NOISE_AMOUNT		0x50

int
main(int argc, char **argv)
{
	int		trial;

	uint8_t		original[120];
	uint8_t		original_len;

	uint8_t		encode[ENCODE_LEN(sizeof(original))];
	int		encode_len;

	uint8_t		transmit[EXPAND_LEN(sizeof(original))];
	int		transmit_len;

	uint8_t		receive[EXPAND_LEN(sizeof(original))];
	int		receive_len;

	uint8_t		decode[DECODE_LEN(sizeof(original))];
	int		decode_ok;

	int		errors = 0;
	int		decode_fail = 0;
	int		crc_mismatch = 0;
	int		data_mismatch = 0;

	if (!ao_real_packet())
		errors++;

	if (!ao_hello_packet())
		errors++;

	srandom(0);
	for (trial = 0; trial < 100000; trial++) {

		/* Compute some random data */
		original_len = ao_random_data(original, sizeof(original));

		/* Encode it */
		encode_len = ao_fec_encode(original, original_len, encode);

		/* Expand from 1-bit-per-symbol to 1-byte-per-symbol */
		transmit_len = ao_expand(encode, encode_len, transmit);

		/* Add gaussian noise to the signal */
		(void) ao_fuzz(transmit, transmit_len, receive, NOISE_AMOUNT);
		receive_len = transmit_len;
		
		/* Decode it */
		decode_ok = ao_fec_decode(receive, receive_len, decode, original_len + 2, NULL);

		/* Check to see if we received the right data */

		if (!decode_ok)
			decode_fail++;
		else if (decode[original_len +1] != AO_FEC_DECODE_CRC_OK)
			crc_mismatch++;
		else if (memcmp(original, decode, original_len) != 0)
			data_mismatch++;
	}


	printf ("%d packets coded\n", trial);
	printf ("decode_fail %d crc_mismatch %d data_mismatch %d\n",
		decode_fail, crc_mismatch, data_mismatch);
	if (decode_fail != EXPECT_DECODE_FAIL) {
		printf ("expected %d decode failures\n", EXPECT_DECODE_FAIL);
		errors++;
	}
	if (crc_mismatch != EXPECT_CRC_MISMATCH) {
		printf ("expected %d crc mismatch\n", EXPECT_CRC_MISMATCH);
		errors++;
	}
	if (data_mismatch != EXPECT_DATA_MISMATCH) {
		printf ("expected %d data mismatch\n", EXPECT_DATA_MISMATCH);
		errors++;
	}
	return errors;
}


