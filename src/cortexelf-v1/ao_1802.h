/*
 * Copyright © 2017 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_1802_H_
#define _AO_1802_H_

/* Signals muxed between 1802 and STM */
extern uint8_t		MRD, TPB, TPA, MWR;

/* Decoded address driven by TPA/TPB signals */
extern uint16_t		ADDRESS;

/* Decoded data, driven by TPB signal */
extern uint8_t		DATA;

extern uint8_t		SC;

#define SC_FETCH	0
#define SC_EXECUTE	1
#define SC_DMA		2
#define SC_INTERRUPT	3

#endif /* _AO_1802_H_ */
