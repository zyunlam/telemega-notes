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

#include "ao.h"

void
ao_clock_init(void)
{
	uint32_t	cfgr;

	/* Switch to HSI while messing about */
	stm_rcc.cr |= (1 << STM_RCC_CR_HSION);
	while (!(stm_rcc.cr & (1 << STM_RCC_CR_HSIRDY)))
		ao_arch_nop();

	stm_rcc.cfgr = (stm_rcc.cfgr & ~(uint32_t) (STM_RCC_CFGR_SW_MASK << STM_RCC_CFGR_SW)) |
		(STM_RCC_CFGR_SW_HSI << STM_RCC_CFGR_SW);

	/* wait for system to switch to HSI */
	while ((stm_rcc.cfgr & (STM_RCC_CFGR_SWS_MASK << STM_RCC_CFGR_SWS)) !=
	       (STM_RCC_CFGR_SWS_HSI << STM_RCC_CFGR_SWS))
		ao_arch_nop();

	/* Disable all interrupts */
	stm_rcc.cir = 0;

#if AO_HSE
#if AO_HSE_BYPASS
	stm_rcc.cr |= (1 << STM_RCC_CR_HSEBYP);
#else
	stm_rcc.cr &= ~(uint32_t) (1 << STM_RCC_CR_HSEBYP);
#endif
	/* Enable HSE clock */
	stm_rcc.cr |= (1 << STM_RCC_CR_HSEON);
	while (!(stm_rcc.cr & (1 << STM_RCC_CR_HSERDY)))
		asm("nop");

#define STM_RCC_CFGR_SWS_TARGET_CLOCK		(STM_RCC_CFGR_SWS_HSE << STM_RCC_CFGR_SWS)
#define STM_RCC_CFGR_SW_TARGET_CLOCK		(STM_RCC_CFGR_SW_HSE)
#define STM_PLLSRC				AO_HSE
#define STM_RCC_CFGR_PLLSRC_TARGET_CLOCK	(STM_RCC_CFGR_PLLSRC_HSE << STM_RCC_CFGR_PLLSRC)
#else
#define STM_HSI 				16000000
#define STM_RCC_CFGR_SWS_TARGET_CLOCK		(STM_RCC_CFGR_SWS_HSI << STM_RCC_CFGR_SWS)
#define STM_RCC_CFGR_SW_TARGET_CLOCK		(STM_RCC_CFGR_SW_HSI)
#define STM_PLLSRC				(STM_HSI/2)
#define STM_RCC_CFGR_PLLSRC_TARGET_CLOCK	(STM_RCC_CFGR_PLLSRC_HSI_2 << STM_RCC_CFGR_PLLSRC)
#endif

#if !AO_HSE || HAS_ADC || HAS_ADC_SINGLE
	/* Enable HSI RC clock 16MHz */
	stm_rcc.cr |= (1 << STM_RCC_CR_HSION);
	while (!(stm_rcc.cr & (1 << STM_RCC_CR_HSIRDY)))
		asm("nop");
#endif

	/* Set flash latency to tolerate 72MHz SYSCLK  -> 2 wait states */

	/* Enable 64-bit access and prefetch */
	stm_flash.acr = ((1 << STM_FLASH_ACR_PRFTBE) |
			 (0 << STM_FLASH_ACR_HLFCYA) |
			 (STM_FLASH_ACR_LATENCY_2 << STM_FLASH_ACR_LATENCY));

	/* Enable power interface clock */
	stm_rcc.apb1enr |= (1 << STM_RCC_APB1ENR_PWREN);

#if 0
	/* Set voltage range to 1.8V */

	/* poll VOSF bit in PWR_CSR. Wait until it is reset to 0 */
	while ((stm_pwr.csr & (1 << STM_PWR_CSR_VOSF)) != 0)
		asm("nop");

	/* Configure voltage scaling range */
	cr = stm_pwr.cr;
	cr &= ~(STM_PWR_CR_VOS_MASK << STM_PWR_CR_VOS);
	cr |= (STM_PWR_CR_VOS_1_8 << STM_PWR_CR_VOS);
	stm_pwr.cr = cr;

	/* poll VOSF bit in PWR_CSR. Wait until it is reset to 0 */
	while ((stm_pwr.csr & (1 << STM_PWR_CSR_VOSF)) != 0)
		asm("nop");
#endif

	/* HCLK */
	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_HPRE_MASK << STM_RCC_CFGR_HPRE);
	cfgr |= (AO_RCC_CFGR_HPRE_DIV << STM_RCC_CFGR_HPRE);
	stm_rcc.cfgr = cfgr;
	while ((stm_rcc.cfgr & (STM_RCC_CFGR_HPRE_MASK << STM_RCC_CFGR_HPRE)) !=
	       (AO_RCC_CFGR_HPRE_DIV << STM_RCC_CFGR_HPRE))
		asm ("nop");

	/* APB1 Prescaler = AO_APB1_PRESCALER */
	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_PPRE1_MASK << STM_RCC_CFGR_PPRE1);
	cfgr |= (AO_RCC_CFGR_PPRE1_DIV << STM_RCC_CFGR_PPRE1);
	stm_rcc.cfgr = cfgr;

	/* APB2 Prescaler = AO_APB2_PRESCALER */
	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_PPRE2_MASK << STM_RCC_CFGR_PPRE2);
	cfgr |= (AO_RCC_CFGR_PPRE2_DIV << STM_RCC_CFGR_PPRE2);
	stm_rcc.cfgr = cfgr;

	/* ADC Prescaler */
	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_ADCPRE_MASK << STM_RCC_CFGR_ADCPRE);
	cfgr |= (AO_RCC_CFGR_ADCPRE << STM_RCC_CFGR_ADCPRE);
	stm_rcc.cfgr = cfgr;

	/* Disable the PLL */
	stm_rcc.cr &= ~(1UL << STM_RCC_CR_PLLON);
	while (stm_rcc.cr & (1UL << STM_RCC_CR_PLLRDY))
		asm("nop");

	/* PLLMUL */
	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_PLLMUL_MASK << STM_RCC_CFGR_PLLMUL);
	cfgr |= (AO_RCC_CFGR_PLLMUL << STM_RCC_CFGR_PLLMUL);

	/* PLLXTPRE */
	cfgr &= ~(STM_RCC_CFGR_PLLXTPRE_MASK << STM_RCC_CFGR_PLLXTPRE);
	cfgr |= (AO_RCC_CFGR_PLLXTPRE << STM_RCC_CFGR_PLLXTPRE);

	/* PLL source */
	cfgr &= ~(1UL << STM_RCC_CFGR_PLLSRC);
	cfgr |= STM_RCC_CFGR_PLLSRC_TARGET_CLOCK;

	stm_rcc.cfgr = cfgr;

	/* Enable the PLL and wait for it */
	stm_rcc.cr |= (1 << STM_RCC_CR_PLLON);
	while (!(stm_rcc.cr & (1 << STM_RCC_CR_PLLRDY)))
		asm("nop");

	/* Switch to the PLL for the system clock */

	cfgr = stm_rcc.cfgr;
	cfgr &= ~(STM_RCC_CFGR_SW_MASK << STM_RCC_CFGR_SW);
	cfgr |= (STM_RCC_CFGR_SW_PLL << STM_RCC_CFGR_SW);
	stm_rcc.cfgr = cfgr;
	for (;;) {
		uint32_t	c, part, mask, val;

		c = stm_rcc.cfgr;
		mask = (STM_RCC_CFGR_SWS_MASK << STM_RCC_CFGR_SWS);
		val = (STM_RCC_CFGR_SWS_PLL << STM_RCC_CFGR_SWS);
		part = c & mask;
		if (part == val)
			break;
	}

#if 0
	stm_rcc.apb2rstr = 0xffff;
	stm_rcc.apb1rstr = 0xffff;
	stm_rcc.ahbrstr = 0x3f;
	stm_rcc.ahbenr = (1 << STM_RCC_AHBENR_FLITFEN);
	stm_rcc.apb2enr = 0;
	stm_rcc.apb1enr = 0;
	stm_rcc.ahbrstr = 0;
	stm_rcc.apb1rstr = 0;
	stm_rcc.apb2rstr = 0;
#endif

	/* Clear reset flags */
	stm_rcc.csr |= (1 << STM_RCC_CSR_RMVF);

	/* Enable AFIO */
	stm_rcc.apb2enr |= (1 << STM_RCC_APB2ENR_AFIOEN);

	/* Release PB3, PA15 and PB4 from JTAG use */
	stm_afio.mapr = (stm_afio.mapr &
			 ~(STM_AFIO_MAPR_SWJ_CFG_MASK << STM_AFIO_MAPR_SWJ_CFG)) |
		STM_AFIO_MAPR_SWJ_CFG_SW_DP << STM_AFIO_MAPR_SWJ_CFG;

#if DEBUG_THE_CLOCK
	/* Output SYSCLK on PA8 for measurments */

	stm_rcc.ahbenr |= (1 << STM_RCC_AHBENR_GPIOAEN);

	stm_afr_set(&stm_gpioa, 8, STM_AFR_AF0);
	stm_moder_set(&stm_gpioa, 8, STM_MODER_ALTERNATE);
	stm_ospeedr_set(&stm_gpioa, 8, STM_OSPEEDR_40MHz);

	stm_rcc.cfgr |= (STM_RCC_CFGR_MCOPRE_DIV_1 << STM_RCC_CFGR_MCOPRE);
	stm_rcc.cfgr |= (STM_RCC_CFGR_MCOSEL_HSE << STM_RCC_CFGR_MCOSEL);
#endif
}
