/*
 * Copyright © 2011 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_BRINGUP_
#define _AO_BRINGUP_

#include <stdio.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#define TEENSY 1
#if TEENSY
#define F_CPU 16000000UL	// 16 MHz
#else
#define F_CPU  8000000UL	// 8 MHz
#endif
#include <util/delay.h>

#define LEDOUT		PORTB7
#define LEDPORT		PORTB
#define LEDDDR		DDRB
#define LEDDDRPIN	DD7

void	ao_bringup_init(void);

#endif
