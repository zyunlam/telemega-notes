/*
 * Copyright © 2012 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
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

#ifndef _AO_FEC_H_
#define _AO_FEC_H_

#include <stdint.h>

#define AO_FEC_CRC_INIT			0xffff
#define AO_FEC_TRELLIS_TERMINATOR	0x0b
#define AO_FEC_PREPARE_EXTRA		4

extern const uint8_t ao_fec_whiten_table[];

void
ao_fec_dump_bytes(uint8_t *bytes, uint16_t len, char *name);

uint16_t
ao_fec_crc(uint8_t *bytes, uint8_t len);

/*
 * 'len' is the length of the original data; 'bytes'
 * must  be four bytes longer than that, and the first
 * two after 'len' must be the received crc
 */
uint8_t
ao_fec_check_crc(uint8_t *bytes, uint8_t len);

/*
 * Append CRC and terminator bytes, returns resulting length.
 * 'out' must be at least len + AO_FEC_PREPARE_EXTRA bytes long
 */
uint8_t
ao_fec_prepare(uint8_t *in, uint8_t len, uint8_t *out);

/*
 * Whiten data using the cc1111 PN9 sequence. 'out'
 * must be 'len' bytes long. 'out' and 'in' can be
 * the same array
 */
void
ao_fec_whiten(uint8_t *in, uint8_t len, uint8_t *out);

/*
 * Encode and interleave data. 'out' must be len*2 bytes long
 */
uint8_t
ao_fec_encode(uint8_t *in, uint8_t len, uint8_t *out);

/*
 * Decode data. 'in' is one byte per bit, soft decision
 * 'out' must be len/8 bytes long
 */

#define AO_FEC_DECODE_BLOCK	(8 * 32)	/* callback must return multiples of this many bits */

uint8_t
ao_fec_decode(uint8_t *in, uint16_t in_len, uint8_t *out, uint8_t out_len, uint16_t (*callback)());

/*
 * Interleave data packed in bytes. 'out' must be 'len' bytes long.
 */
uint16_t
ao_fec_interleave_bytes(uint8_t *in, uint16_t len, uint8_t *out);

#endif /* _AO_FEC_H_ */
