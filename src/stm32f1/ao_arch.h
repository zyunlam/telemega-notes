/*
 * Copyright © 2023 Keith Packard <keithp@keithp.com>
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
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#ifndef _AO_ARCH_H_
#define _AO_ARCH_H_

#include <stm32f1.h>

#ifndef AO_STACK_SIZE
#define AO_STACK_SIZE	512
#endif

#define AO_PORT_TYPE	uint16_t

#define ao_arch_naked_declare	__attribute__((naked))
#define ao_arch_naked_define
#define __interrupt(n)
#define __at(n)

#define ao_arch_nop()		asm("nop")

#define ao_arch_interrupt(n)	/* nothing */

#define AO_ROMCONFIG_SYMBOL __attribute__((section(".init.1"))) const
#define AO_USBCONFIG_SYMBOL __attribute__((section(".init.2"))) const

#define AO_SYSTICK	(AO_HCLK / 8)

#if AO_NONMASK_INTERRUPT
#define AO_STM_NVIC_NONMASK_PRIORITY	0x00

/* Set the basepri register to this value to mask all
 * non-maskable priorities
 */
#define AO_STM_NVIC_BASEPRI_MASK	0x10
#endif

#define AO_STM_NVIC_HIGH_PRIORITY	0x40
#define AO_STM_NVIC_MED_PRIORITY	0x80
#define AO_STM_NVIC_LOW_PRIORITY	0xC0
#define AO_STM_NVIC_CLOCK_PRIORITY	0xf0

#define AO_PCLK1	AO_APB1CLK
#define AO_PCLK2	AO_APB2CLK

#endif /* _AO_ARCH_H_ */
