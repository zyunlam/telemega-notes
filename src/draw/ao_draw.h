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

#ifndef _AO_DRAW_H_
#define _AO_DRAW_H_

void
ao_blt(uint32_t		*src_line,
       int16_t		src_stride,
       int16_t		src_x,
       uint32_t		*dst_line,
       int16_t		dst_stride,
       int16_t		dst_x,
       int16_t		width,
       int16_t		height,
       uint8_t		rop,
       uint8_t		reverse,
       uint8_t		upsidedown);

void
ao_solid(uint32_t	and,
	 uint32_t	xor,
	 uint32_t	*dst,
	 int16_t	dst_stride,
	 int16_t	dst_x,
	 int16_t	width,
	 int16_t	height);

void
ao_text(char		*string,
	uint32_t	*dst_line,
	int16_t		dst_stride,
	int16_t		dst_x);

#define AO_ROP_CLEAR	0x0
#define AO_ROP_COPY	0x3
#define AO_ROP_SET	0xf

#define AO_SHIFT	5
#define AO_UNIT		(1 << AO_SHIFT)
#define AO_MASK		(AO_UNIT - 1)
#define AO_ALLONES	((uint32_t) -1)

static inline uint32_t
ao_left(uint32_t bits, int16_t shift) {
	return bits >> shift;
}

static inline uint32_t
ao_right(uint32_t bits, int16_t shift) {
	return bits << shift;
}

static inline uint32_t
ao_right_mask(int16_t x) {
	if ((AO_UNIT - x) & AO_MASK)
		return ao_left(AO_ALLONES,(AO_UNIT - x) & AO_MASK);
	else
		return 0;
}

static inline uint32_t
ao_left_mask(int16_t x) {
	if (x & AO_MASK)
		return ao_right(AO_ALLONES, x & AO_MASK);
	else
		return 0;
}

static inline uint32_t
ao_bits_mask(int16_t x, int16_t w) {
	return ao_right(AO_ALLONES, x & AO_MASK) &
		ao_left(AO_ALLONES,(AO_UNIT - (x + w)) & AO_MASK);
}

#define ao_mask_bits(x,w,l,n,r) { \
    n = (w); \
    r = ao_right_mask((x)+n); \
    l = ao_left_mask(x); \
    if (l) { \
	n -= AO_UNIT - ((x) & AO_MASK); \
	if (n < 0) { \
	    n = 0; \
	    l &= r; \
	    r = 0; \
	} \
    } \
    n >>= AO_SHIFT; \
}

static inline uint32_t
ao_do_mask_rrop(uint32_t dst, uint32_t and, uint32_t xor, uint32_t mask) {
	return (dst & (and | ~mask)) ^ (xor & mask);
}

static inline uint32_t
ao_do_rrop(uint32_t dst, uint32_t and, uint32_t xor) {
	return (dst & and) ^ xor;
}

#define AO_CLEAR         0x0	/* 0 */
#define AO_AND           0x1	/* src AND dst */
#define AO_AND_REVERSE   0x2	/* src AND NOT dst */
#define AO_COPY          0x3	/* src */
#define AO_AND_INVERTED  0x4	/* NOT src AND dst */
#define AO_NOOP          0x5	/* dst */
#define AO_XOR           0x6	/* src XOR dst */
#define AO_OR            0x7	/* src OR dst */
#define AO_NOR           0x8	/* NOT src AND NOT dst */
#define AO_EQUIV         0x9	/* NOT src XOR dst */
#define AO_INVERT        0xa	/* NOT dst */
#define AO_OR_REVERSE    0xb	/* src OR NOT dst */
#define AO_COPY_INVERTED 0xc	/* NOT src */
#define AO_OR_INVERTED   0xd	/* NOT src OR dst */
#define AO_NAND          0xe	/* NOT src OR NOT dst */
#define AO_SET           0xf	/* 1 */

#endif /* _AO_DRAW_H_ */
