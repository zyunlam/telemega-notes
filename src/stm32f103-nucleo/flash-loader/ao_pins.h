/*
 * Copyright © 2013 Keith Packard <keithp@keithp.com>
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

#ifndef _AO_PINS_H_
#define _AO_PINS_H_

#define AO_HSE		1
#define AO_HSE_BYPASS	1

#define AO_SYSCLK	72000000
#define AO_HCLK		72000000
#define AO_APB1CLK	36000000
#define AO_APB2CLK	72000000
#define AO_ADCCLK	12000000

#define AO_RCC_CFGR_USBPRE	STM_RCC_CFGR_USBPRE_1_5
#define AO_RCC_CFGR_PLLMUL	STM_RCC_CFGR_PLLMUL_9
#define AO_RCC_CFGR_PLLXTPRE	STM_RCC_CFGR_PLLXTPRE_1
#define AO_RCC_CFGR_PPRE2_DIV	STM_RCC_CFGR_PPRE2_DIV_1
#define AO_RCC_CFGR_PPRE1_DIV	STM_RCC_CFGR_PPRE1_DIV_2
#define AO_RCC_CFGR_HPRE_DIV	STM_RCC_CFGR_HPRE_DIV_1
#define AO_RCC_CFGR_ADCPRE	STM_RCC_CFGR_ADCPRE_6

#include <ao_flash_stm_pins.h>

/* USB DM is on PA11 (CN10 pin 14). USB DP is on PA12 (CN10 pin 12) */

/* For pullup, we'll use PB6 (CN10 pin 16) */

#define AO_BOOT_PIN			1
#define AO_BOOT_APPLICATION_GPIO	stm_gpiob
#define AO_BOOT_APPLICATION_PIN		6
#define AO_BOOT_APPLICATION_VALUE	1
#define AO_BOOT_APPLICATION_MODE	AO_EXTI_MODE_PULL_UP

#define HAS_USB_PULLUP	1
#define AO_USB_PULLUP_PORT	(&stm_gpiob)
#define AO_USB_PULLUP_PIN	12

#endif /* _AO_PINS_H_ */
