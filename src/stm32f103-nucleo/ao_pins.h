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

#define HAS_BEEP	0
#define HAS_USB		1

#define HAS_USB_PULLUP	1
#define AO_USB_PULLUP_PORT	(&stm_gpiob)
#define AO_USB_PULLUP_PIN	12

#define HAS_LED		1
#define LED_0_PORT	(&stm_gpioa)
#define LED_0_PIN	5
#define AO_LED_GREEN	(1 << 0)
#define AO_LED_PANIC	AO_LED_GREEN

#define HAS_SERIAL_1		0
#define USE_SERIAL_1_STDIN	0
#define SERIAL_1_PA9_PA10	1

#define HAS_SERIAL_2		1
#define USE_SERIAL_2_STDIN	1
#define SERIAL_2_PA2_PA3	1
#define SERIAL_2_SPEED		AO_SERIAL_SPEED_115200

#define HAS_SPI_1		1
#define SPI_1_PA5_PA6_PA7	1
#define SPI_1_MODE_OUTPUT	STM_GPIO_CR_MODE_OUTPUT_10MHZ

#define AO_ST7565_CS_PORT	(&stm_gpioa)	/* pin 1 */
#define AO_ST7565_CS_PIN	4
#define AO_ST7565_RESET_PORT	(&stm_gpioa)	/* pin 2 */
#define AO_ST7565_RESET_PIN	0
#define AO_ST7565_A0_PORT	(&stm_gpioa)	/* pin 3 */
#define AO_ST7565_A0_PIN	1
#define AO_ST7565_SPI_BUS	AO_SPI_1_PA5_PA6_PA7
#define AO_ST7565_WIDTH		128
#define AO_ST7565_HEIGHT	64
#define AO_ST7565_BIAS		AO_ST7565_LCD_BIAS_1_9
