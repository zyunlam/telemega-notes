/*
 * Copyright © 2018 Keith Packard <keithp@keithp.com>
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
#ifndef _AO_PINS_H_
#define _AO_PINS_H_

#define HAS_BEEP	0

#define B_USER_PORT	(&stm_gpioa)
#define B_USER_PIN	0

#define LED_GREEN_PORT	(&stm_gpioc)
#define LED_GREEN_PIN	5
#define LED_RED_PORT	(&stm_gpioe)
#define LED_RED_PIN	3

#define AO_HSE		8000000	/* fed from st/link processor */
#define AO_HSE_BYPASS	1	/* no xtal, directly fed */

#define AO_PLL_M	4	/* down to 2MHz */

#define AO_PLL1_N	96	/* up to 192MHz */
#define AO_PLL1_P	2	/* down to 96MHz */
#define AO_PLL1_Q	4	/* down to 48MHz for USB and SDIO */

#define AO_AHB_PRESCALER	1
#define AO_RCC_CFGR_HPRE_DIV	STM_RCC_CFGR_HPRE_DIV_1

#define AO_APB1_PRESCALER	1
#define AO_RCC_CFGR_PPRE1_DIV	STM_RCC_CFGR_PPRE1_DIV_1
#define AO_APB2_PRESCALER	1
#define AO_RCC_CFGR_PPRE2_DIV	STM_RCC_CFGR_PPRE2_DIV_1

#define DEBUG_THE_CLOCK	1

#endif /* _AO_PINS_H_ */
